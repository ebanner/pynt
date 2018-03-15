;;; pynt.el --- Generate and scroll EIN buffers from python code -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Edward Banner <edward.banner@gmail.com>
;; Version: 1.0
;; Package-Requires: ((emacs "24.4") (ein "0.13.1") (epc "0.1.1") (deferred "0.5.1") (helm "2.8.8"))
;; Keywords: convenience
;; URL: https://github.com/ebanner/pynt

;;; Commentary:

;; pynt is an Emacs minor mode for generating and interacting with EIN notebooks.
;;
;; Feature List
;; ------------
;; - On-the-fly notebook creation
;;   - Run the command `pynt-mode' on a python buffer and a new notebook will be created for you to interact with (provided you have set the variable `pynt-start-jupyter-server-on-startup' to t)
;; - Dump a region of python code into a EIN notebook
;;   - Selectable regions include functions, methods, and code at the module level (i.e. outside of any function or class)
;; - Scroll the resulting EIN notebook with the code buffer
;;   - Alignment between code and cells are preserved even when cells are added and deleted

;;; Code:

(require 'seq)
(require 'epc)
(require 'epcs)
(require 'ein-jupyter)
(require 'magit)

;; Globals.
(setq pynt-lock nil
      pynt-code-buffer-file-name nil
      pynt-code-buffer-file-names nil
      pynt-code-buffer-name nil)

(defgroup pynt nil
  "Customization group for pynt."
  :group 'applications)

(defcustom pynt-epc-server-hostname "localhost"
  "The hostname of the EPC server.

Usually set to \"localhost\" but if the jupyter kernel is running
inside a docker container then this value should be
\"docker.for.mac.localhost\" when on a mac.

Using the value of a remote machine should be possible but is
currently untested."
  :options '("localhost" "docker.for.mac.localhost"))

(defcustom pynt-scroll t
  "Scroll the notebook buffer with the code buffer. "
  :options '(nil t))

(defcustom pynt-scroll-narrow-view nil
  "Narrow the notebook buffer if t and don't otherwise.

When scrolling through the code buffer narrow the EIN notebook
buffer. This could have a good use case but is currently rarely
used."
  :options '(nil t))

(defcustom pynt-epc-port 9999
  "The port that the EPC server listens on.

Every invocation of pynt mode increments this number so that pynt
mode can run in multiple buffers.")

(defcustom pynt-start-jupyter-server-on-startup t
  "Start a jupyter server on startup if t and do not otherwise.

The jupyter server listens on the port defined by the variable
`ein:url-or-port'.")

(defcustom pynt-verbose nil
  "Log pynt debug information if t and do not otherwise.")

(defvar pynt-init-code-template
  "

%%matplotlib inline

import time
import traceback
import IPython
from epc.client import EPCClient
from IPython.core.ultratb import AutoFormattedTB
import ast
import inspect

epc_client = EPCClient(('%s', %s), log_traceback=True)
def __cell__(content, buffer_name, cell_type, line_number):
    epc_client.call_sync('make-cell', args=[content, buffer_name, cell_type, line_number])
    time.sleep(0.01)

class LineNumberFinder(ast.NodeTransformer):
    @staticmethod
    def _equals(a, b):
        return abs(a-b) < 2 # FIXME decorators
    def __init__(self, func_name, lineno):
        super().__init__()
        self.func_name = func_name
        self.lineno = lineno
    def visit_ClassDef(self, classdef):
        methods = [stmt for stmt in classdef.body if isinstance(stmt, ast.FunctionDef)]
        for method in methods:
            if method.name == self.func_name and self._equals(method.lineno, self.lineno):
                raise Exception(f'{classdef.name}.{method.name}')
        return classdef
    def visit_FunctionDef(self, func):
        if func.name == self.func_name and self._equals(func.lineno, self.lineno):
            raise Exception(func.name)
        return func
def find_namespace(code, func_name, lineno):
    try:
        tree = ast.parse(code)
        LineNumberFinder(func_name, lineno).visit(tree)
    except Exception as e:
        return e.args[0]

tbf = AutoFormattedTB(mode='Plain', tb_offset=1)
def handler(shell, etype, evalue, tb, tb_offset=None):
    shell.showtraceback((etype, evalue, tb), tb_offset=tb_offset)
    stb = tbf.structured_traceback(etype, evalue, tb)
    while True:
        if not tb.tb_next:
            break
        tb = tb.tb_next
    frame_summary, = traceback.extract_tb(tb)
    if frame_summary.name == '<module>':
        return
    lines, _ = inspect.findsource(tb.tb_frame.f_code)
    code = ''.join(lines)
    namespace = find_namespace(code, frame_summary.name, tb.tb_frame.f_code.co_firstlineno)
    epc_client.call_sync('report-exception', args=[namespace])
    globals().update(tb.tb_frame.f_locals)
    return stb
#IPython.get_ipython().set_custom_exc(exc_tuple=(Exception,), handler=handler)

%s # additional code
"
  "Python code template which is evaluated early on.

The value of `pynt-epc-server-hostname' and
`pynt-epc-port' are used to complete this template.

Having '__cell__()' and 'epc_client' defined in the associated
IPython kernel allow the running python code to send code
snippets to the EPC server.")

(defvar pynt-epc-server nil
  "Emacs server.

There only ever needs to be one EPC server, even across multiple
pynt mode instances.")

(defvar pynt-ast-server nil "Python AST server")

(defvar-local pynt-namespace nil "The name current namespace.")

(defvar-local pynt-line-to-cell-map nil
  "Map of source code lines to EIN cell(s).

A source code line may be associated with more than one EIN
cell (e.g. a line in the body of a for loop.")

(defvar-local pynt-namespace-to-notebook-map nil
  "Map of namespaces to notebooks.

Each namespace has its own indepenedent notebook.")

(defvar-local pynt-namespace-to-region-map nil
  "Map of namespace names to start and end lines.

This map is used to produce a visual indication of which
namespace corresponds to which code. It was originally part of a
feature that was purely intended for making video demos prettier
but does serve as a way to intuitively select a region of code.

This map is used after a user changes the active namespace via
the command `pynt-choose-namespace'.")

(defun pynt-log (&rest args)
  "Log the message when the variable `pynt-verbose' is t.

Optional argument ARGS the arguments you would normally pass to the function `message'."
  (when pynt-verbose
    (apply 'message args)))

(defun pynt-toggle-debug ()
  "Toggle pynt development mode.

In pynt development mode we set the print-* variables to values
so that when we try and print EIN deeply nested and recursive
data structures they print and do not lock up emacs."
  (interactive)
  (setq pynt-verbose (not pynt-verbose)
        print-level (if print-level nil 1)
        print-length (if print-length nil 1)
        print-circle (not print-circle))
  (if ein:debug
      (ein:dev-stop-debug)
    (ein:dev-start-debug)))

(defun pynt-toggle-scroll ()
  "Toggle notebook scrolling.

Flips the value of the variable `pynt-scroll'."
  (interactive)
  (setq pynt-scroll (not pynt-scroll)))

(defun pynt-notebook (&optional namespace)
  "Get the current notebook."
  (gethash (or namespace pynt-namespace) pynt-namespace-to-notebook-map))

(defun pynt-notebook-buffer (&optional namespace)
  "Get the buffer of the notebook."
  (ein:notebook-buffer (pynt-notebook (or namespace pynt-namespace))))

(defun pynt-notebook-buffer-name (&optional namespace)
  "Get the buffer name of the EIN notebook."
  (let ((notebook-buffer (ein:notebook-buffer (pynt-notebook (or namespace pynt-namespace)))))
    (buffer-name notebook-buffer)))

(defun pynt-notebook-window (&optional namespace)
  "Get the notebook window."
  (get-buffer-window (ein:notebook-buffer (pynt-notebook (or namespace pynt-namespace)))))

(defun pynt-notebook-kernel (&optional namespace)
  (ein:$notebook-kernel (pynt-notebook (or namespace pynt-namespace))))

(defun pynt-module-name ()
  "Extract the module-level name of the pynt code buffer.

If the buffer is associated with a python file then chop off the
'.py' suffix. Otherwise (e.g. if this is a *scratch* buffer) then
just return the buffer name.

Throw an error if the buffer name has a period in it because that
will mess with the namespace naming convention that pynt uses."
  (let* ((script-name (file-name-nondirectory (buffer-file-name))))
    (file-name-sans-extension script-name)))

(defun pynt-choose-namespace ()
  "Switch the active code region by selecting from a list of namespaces."
  (interactive)
  (setq pynt-namespace-to-region-map (make-hash-table :test 'equal))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (epc:call-deferred pynt-ast-server 'parse_namespaces `(,code ,(pynt-module-name)))
      (deferred:nextc it
        (lambda (namespaces)
          (defun annotate-namespace (namespace)
            (multiple-value-bind (nothing name start-line end-line) namespace
              (let ((jack-in-command (pynt-jack-in-command name))
                    (test-runner-command (alist-get 'runner (alist-get 'tests (pynt-command-map)))))
                (list (format "[%s] %s :: %s" (if (string= jack-in-command test-runner-command) "✓" "✗") name jack-in-command)
                      name
                      start-line
                      end-line))))
          (setq namespaces (mapcar 'annotate-namespace namespaces))
          (helm :sources
                `((name . "Choose a namespace to interact with")
                  (candidates . ,namespaces)
                  (action . (lambda (namespace)
                              (multiple-value-bind (name start-line end-line) namespace
                                (goto-line start-line)
                                (recenter 0)
                                (pynt-switch-or-init name)))))))))))

(defun pynt-recover-notebook-window ()
  "Recover the notebook window.

You can call this function in case the automatic window
arrangement fails. But it's been working pretty well lately so
you shouldn't have to use it."
  (interactive)
  (pynt-pop-up-notebook-buffer (pynt-notebook-buffer)))

(defun pynt-kill-cells ()
  "Delete all the cells in the notebook.

If the argument OTHER-WORKSHEET is non-nil then use the other
worksheet."
  (interactive)
  (with-current-buffer (pynt-notebook-buffer)
    (beginning-of-buffer)
    (condition-case exception
        (while t (call-interactively 'ein:worksheet-kill-cell))
      ('error))
    (beginning-of-buffer)))

(defun pynt-dump-namespace ()
  "Dump the code in `pynt-active-namespace' into its EIN worksheet buffer.

This is done by sending the code region out to the AST server
where it is annotated with EPC calls and then the resulting code
is sent to the IPython kernel to be executed."
  (interactive)
  (setq pynt-lock nil)
  (pynt-offload-to-scratch-worksheet)
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (pynt-log "Dumping the namespace!")
      (pynt-log "Calling python AST server with active namespace = %s ..." pynt-namespace)
      (epc:call-deferred pynt-ast-server 'annotate `(,code ,pynt-namespace))
      (deferred:nextc it
        (lambda (annotated-code)
          (pynt-log "Annotated code = %S" annotated-code)
          (ein:connect-eval-buffer)
          (ein:shared-output-eval-string annotated-code))))))

(defun pynt-scroll-cell-window ()
  "Scroll the EIN worksheet buffer with the code buffer.

Do it so the cell which corresponds to the line of code the point
is on goes to the top.  Make sure the cell we're about to jump to
is is indeed the active buffer.

Go off of the variable `pynt-nth-cell-instance' in the case where
we want to see the nth pass though, say, a for loop.

Wrap the main logic in a condition case because it could be the
case that the cell that did correspond to a line has since been
deleted. Basically there is a bunch of data invalidation that I
don't want to worry about at this time."
  (interactive)
  (when pynt-scroll
    (save-selected-window
      (let ((entries (gethash (line-number-at-pos) pynt-line-to-cell-map)))
        (when entries
          (condition-case exception
              (multiple-value-bind (namespace cell) (nth pynt-nth-cell-instance entries)
                (if (string= namespace pynt-namespace)
                    (let* ((cell-marker (ein:cell-location cell :input))
                           (point-line (count-screen-lines (window-start) (point))))
                      (when cell-marker
                        (select-window (pynt-notebook-window))
                        (widen)
                        (goto-char cell-marker)
                        (recenter point-line)
                        (when pynt-scroll-narrow-view
                          (beginning-of-line)
                          (previous-line)
                          (call-interactively 'set-mark-command)
                          (call-interactively 'ein:worksheet-goto-next-input)
                          (call-interactively 'ein:worksheet-goto-next-input)
                          (previous-line)
                          (call-interactively 'narrow-to-region)
                          (beginning-of-buffer))))
                  (pynt-switch-to-namespace namespace)))
            ('error)))))))

(defun pynt-prev-cell-instance ()
  "Scroll the EIN worksheet to the next occurrence of the current code line.

This only happens in the body of for and while loops where some
lines of code and executed many times.

This function is part of pynt scroll mode."
  (interactive)
  (setq pynt-nth-cell-instance (1- pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-next-cell-instance ()
  "Scroll the EIN worksheet to the previous occurrence of the current code line.

This only happens in the body of for and while loops where some
lines of code and executed many times.

This function is part of pynt scroll mode."
  (interactive)
  (setq pynt-nth-cell-instance (1+ pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-pop-up-notebook-buffer (buffer)
  "Pop up the notebook window.

If the notebook window is in the current frame then jump over to
it and switch it out the correct notebook. Otherwise just use a
modified version of the function `pop-to-buffer'."
  (condition-case nil
      (save-selected-window
        (windmove-right)
        (switch-to-buffer buffer))
    (error nil (display-buffer (window-normalize-buffer-to-switch-to buffer) nil))))

(defun pynt-rename-notebook ()
  "Rename the notebook to the current namespace.

TODO this is currently not used but would be nice to get
working."

  (pynt-log "Setting notebook name variable...")
  (let* ((relative-path (replace-regexp-in-string (expand-file-name "~/") "" (buffer-file-name)))
         (relative-dir (file-name-directory relative-path))
         (notebook-name (concat relative-dir pynt-namespace ".ipynb")))
    (setq pynt-notebook-path))

  (delete-file (file-name-nondirectory pynt-notebook-path))

  ;; Poll until notebook file is deleted.
  (deferred:nextc it
    (deferred:lambda (msg)
      (if (not (file-exists-p (file-name-nondirectory pynt-notebook-path)))
          (progn
            (pynt-log "%s is deleted! Renaming notebook to %s..." (file-name-nondirectory pynt-notebook-path) pynt-notebook-path)
            'notebook-is-deleted)
        (pynt-log "%s still exists..." (file-name-nondirectory pynt-notebook-path))
        (deferred:nextc (deferred:wait 500) self))))

  ;; Rename the notebook.
  (deferred:nextc it
    (lambda (msg)
      (with-current-buffer pynt-code-buffer-name
        (with-current-buffer (pynt-notebook-buffer)
          (ein:notebook-rename-command pynt-notebook-path)))
      'notebook-rename-initiated))

  ;; Poll until notebook is renamed.
  (deferred:next
    (deferred:lambda ()
      (with-current-buffer pynt-code-buffer-name
        (if (string= (ein:$notebook-notebook-path (pynt-notebook)) pynt-notebook-path)
            (progn
              (pynt-log "[2] : Notebook name is set!")
              'pynt-connected)
          (pynt-log "Still waiting for notebook name to change...")
          (deferred:nextc (deferred:wait 500) self))))))

(defun pynt-setup-notebook (&rest -ignore-)
  "Default callback for `ein:notebook-open'.

After popping open the notebook in a window jack into the
namespace if there is a command in pynt.json. Finally dump the
code into the notebook."

  ;; Pop up notebook.
  (pynt-pop-up-notebook-buffer (current-buffer))

  (setq notebook ein:%notebook%)
  (with-current-buffer pynt-code-buffer-name
    (pynt-log "Putting %s into hash..." pynt-namespace)
    (puthash pynt-namespace notebook pynt-namespace-to-notebook-map)
    (with-current-buffer (pynt-notebook-buffer)
      (ein:notebook-worksheet-insert-next ein:%notebook% ein:%worksheet% :render nil)))

  (deferred:$
    ;; Connect code buffer to notebook.
    (deferred:next
      (lambda ()
        (with-current-buffer pynt-code-buffer-name
          (pynt-connect-to-notebook-buffer (pynt-notebook-buffer)))))

    ;; Poll until code buffer is connected.
    (deferred:nextc it
      (deferred:lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (if ein:connect-mode
              (pynt-log "Code buffer connected!")
            (pynt-log "Code buffer not connected...")
            (deferred:nextc (deferred:wait 500) self)))))

    ;; Dump the module-level namespace. Possibly jack in first.
    (deferred:nextc it
      (lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (let ((command (pynt-jack-in-command)))
            (when command
              (pynt-jack-in command))))))))

(defun pynt-command-map ()
  "Get the command map.

Read the pynt.json file in the project root."
  (json-read-file (concat (pynt-project-root) "pynt.json")))

(defun pynt-new-notebook ()
  "Create a new EIN notebook and bring it up side-by-side.

Make sure the new notebook is created in the same directory as
the python file so that relative imports in the code work fine.

Set the pynt code buffer name because this function will jump the
point to another window. In general buffer names are not to be
relied on remember!"
  (interactive)
  (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
    (let* ((dir-path (substring (file-name-directory pynt-code-buffer-file-name) 0 -1))
           (nb-dir (replace-regexp-in-string (expand-file-name "~/") "" dir-path))
           (notebook-list-buffer-name (concat "*ein:notebooklist " url-or-port "*")))
      (with-current-buffer notebook-list-buffer-name
        (ein:notebooklist-new-notebook url-or-port nil nb-dir 'pynt-setup-notebook)))))

(defun pynt-start-epc-server ()
  "Start the EPC server and register its associated callback.

The EPCS server's job is to relay commands to create an execute
EIN cells from the python EPC client."
  (when (not pynt-epc-server)

    (defun handle-make-cell (&rest args)
      (multiple-value-bind (expr namespace cell-type line-number) args
        (pynt-make-cell expr namespace cell-type (string-to-number line-number))
        nil))

    (defun handle-report-exception (&rest args)
      "Switch to the namespace where the exception happened and execute it.

Currently this only works with functions that do not have any
arguments. If the function has arguments then the one in the
docstring will be the ones that get inserted."
      (multiple-value-bind (namespace-where) args
        (when (not (string= namespace-where "<module>")) ; only jump when exception originated in some other function
          (let* ((namespace (concat (pynt-module-name) "." namespace-where))
                 (namespace-buffer-name (format "ns=%s" namespace)))
            (pynt-log "The exception was hit in the namespace = %s" namespace-buffer-name)
            (pynt-switch-to-worksheet namespace-buffer-name)
            (pynt-dump-namespace)))
        nil))

    (defun handle-python-exception (&rest args)
      "Switch to the namespace where the exception happened and execute it.

In this case the exception occurred in an external kernel outside
of jupyter. In addition to popping over to the relevant notebook
we also need to retrieve the appropriate kernel, switch over to
it, and set the local variables.

Sit for 5 seconds while the python process generates a kernel
file in ~/Library/Jupyter/runtime/."
      (multiple-value-bind (namespace-where) args
        (let ((namespace-buffer-name (format "ns=%s" namespace-where)))
          (pynt-log "The exception was hit in the namespace = %s" namespace-buffer-name)
          (with-selected-window (pynt-notebook-window) (switch-to-buffer namespace-buffer-name)))
        (sit-for 5)
        (pynt-switch-kernel (pynt-get-latest-kernel-id))
        (pynt-dump-namespace)
        nil))

    (let ((connect-function
           (lambda (mngr)
             (let ((mngr mngr))
               (epc:define-method mngr 'make-cell 'handle-make-cell)
               (epc:define-method mngr 'report-exception 'handle-report-exception)
               (epc:define-method mngr 'python-exception 'handle-python-exception)))))
      (setq pynt-epc-server (epcs:server-start connect-function pynt-epc-port)))))

(defun pynt-make-cell (expr namespace cell-type line-number)
  "Make a new EIN cell and evaluate it.

Insert a new code cell with contents EXPR into the worksheet
buffer NAMESPACE with cell type CELL-TYPE at the end of the
worksheet and evaluate it.

This function is called from python code running in a jupyter
kernel via RPC.

LINE-NUMBER is the line number in the code that the cell
corresponds and is used during pynt scroll mode. If LINE-NUMBER
is -1 then that means the cell has no corresponding line. This
happens with certain markdown cells which are generated.

Since the variable `pynt-line-to-cell-map' is buffer-local we
have to take special care to not access it while we're over in
the worksheet buffer. Instead we save the variable we wish to
append to `pynt-line-to-cell-map' into a temporary variable and
then add it to `pynt-line-to-cell-map' when we're back in the
code buffer."
  (pynt-log "(pytn-make-cell %S %S %S %S)..." expr namespace cell-type line-number)

  ;; These variables are buffer local so we need to grab them before switching
  ;; over to the worksheet buffer.
  (setq new-cell nil)                   ; new cell to be added
  (with-current-buffer (pynt-notebook-buffer)
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (let ((cell (ein:get-cell-at-point))
          (ws (ein:worksheet--get-ws-or-error)))
      (cond ((string= cell-type "code") (ein:cell-set-autoexec cell t))
            ((string= cell-type "markdown") (ein:worksheet-change-cell-type ws cell "markdown"))
            (t (ein:worksheet-change-cell-type ws cell "heading" (string-to-number cell-type))))
      (setq new-cell cell)))
  (with-selected-window (pynt-notebook-window)
    (beginning-of-buffer))
  (when (not (eq line-number -1))
    (let ((previous-entries (gethash line-number pynt-line-to-cell-map))
          (new-entry (list namespace new-cell)))
      (puthash line-number (add-to-list 'previous-entries new-entry) pynt-line-to-cell-map))))

(defun pynt-start-ast-server ()
  "Start python AST server."
  (when (not pynt-ast-server)
    (setq pynt-ast-server (epc:start-epc "pynt-serve" nil))))

(defun pynt-init-epc-client (additional-code &optional callback cbargs)
  "Initialize the EPC client for the EIN notebook.

This needs to be done so python can send commands to Emacs to
create code cells. Use the variables
`pynt-epc-server-hostname' and `pynt-epc-port' to define
the communication channels for the EPC client.

Argument CALLBACK is a function to call afterwards."
  (pynt-log "Initiating EPC cient...")

  (deferred:$
    ;; Poll until kernel is live and then connect code buffer to notebook.
    (deferred:next
      (deferred:lambda ()
        (with-current-buffer pynt-code-buffer-name
          (if (ein:kernel-live-p (pynt-notebook-kernel))
              (progn
                (pynt-log "Kernel is live!")
                'kernel-is-live)
            (pynt-log "Kernel not live...")
            (deferred:nextc (deferred:wait 500) self)))))

    ;; Evaluate init code.
    (deferred:nextc it
      (lambda (msg)
        (let ((pynt-init-code (format pynt-init-code-template pynt-epc-server-hostname pynt-epc-port additional-code)))
          (pynt-log "Evaluating initial code!")
          (ein:shared-output-eval-string pynt-init-code))
        nil))

    ;; Run callback.
    (deferred:nextc it
      (lambda (msg)
        (when callback (apply callback cbargs))))))

(defun pynt-intercept-ein-notebook-name (old-function buffer-or-name)
  "Advice to add around `ein:connect-to-notebook-buffer'.

So pynt mode can grab the buffer name of the main worksheet.

Argument OLD-FUNCTION the function we are wrapping.
Argument BUFFER-OR-NAME the name of the notebook we are connecting to."
  (pynt-log "Setting main worksheet name = %S" buffer-or-name)
  (apply old-function (list buffer-or-name)))

(defun pynt-offload-to-scratch-worksheet ()
  "Move cells from the main notebook to the scratch notebook."
  (interactive)
  (with-current-buffer (pynt-notebook-buffer)
    (beginning-of-buffer)
    (push-mark (point-max))
    (activate-mark)
    (call-interactively 'ein:worksheet-kill-cell)
    (let ((worksheets (ein:$notebook-worksheets ein:%notebook%)))
      (with-current-buffer (ein:worksheet-buffer (nth 1 worksheets))
        (beginning-of-buffer)
        (call-interactively 'ein:worksheet-yank-cell)))))

(defun pynt-switch-kernel (process event)
  "Switch to most recently started kernel.

Because we are using the ExternalIPythonKernelManager issuing a
restart to the jupyter notebook causes us to switch to the most
recently started kernel.

This function is meant to be used as a sentinel after the process
pynt-embed finishes."
  (pynt-make-cell (format "Jack in command %s" event)
                  pynt-namespace
                  "markdown"
                  -1)
  (select-window (get-buffer-window "*pynt code*"))
  (pynt-log "Setting the visited file name by to %s..." pynt-code-buffer-file-name)
  (set-visited-file-name pynt-code-buffer-file-name)

  (deferred:$
    ;; Restart the kernel.
    (deferred:next
      (lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (ein:notebook-restart-kernel (pynt-notebook)))))

    ;; Poll until kernel is killed.
    (deferred:nextc it
      (deferred:lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (if (not (ein:kernel-live-p (pynt-notebook-kernel)))
              (progn
                (pynt-log "Kernel killed!")
                'kernel-killed)
            (pynt-log "Kernel not killed yet. Polling...")
            (deferred:nextc (deferred:wait 10) self)))))

    ;; Initialize the EPC client and dump the namespace.
    (deferred:nextc it
      (lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (pynt-init-epc-client "pass" 'pynt-dump-namespace))))))

(defun pynt-run-all-cells-above ()
  "Execute all cells above and including the cell at point.

This is a convenience function meant to be used by users of
pynt (and EIN)."
  (interactive)
  (save-excursion
    (let ((end-cell (ein:get-cell-at-point)))
      (beginning-of-buffer)
      (setq cell (ein:get-cell-at-point))
      (while (not (eq cell end-cell))
        (call-interactively 'ein:worksheet-execute-cell-and-goto-next)
        (setq cell (ein:get-cell-at-point)))
      (call-interactively 'ein:worksheet-execute-cell))))

(defun pynt-project-root ()
  "Get the project root.

Use the variable `pynt-project-root' first. If that is not set
then ask magit for the closest enclosing github repo root."
  (magit-toplevel))

(defun pynt-project-relative-path ()
  "Relative path from the project root."
  (let* ((project-path (expand-file-name (pynt-project-root)))
         (project-relative-path (replace-regexp-in-string project-path "" (buffer-file-name))))
    project-relative-path))

(defun pynt-project-relative-dir ()
  "Relative directory the project root."
  (let* ((project-path (expand-file-name (pynt-project-root)))
         (absolute-dir-path (file-name-directory (buffer-file-name)))
         (relative-dir-path (replace-regexp-in-string project-path "" absolute-dir-path)))
    relative-dir-path))

(defun pynt-jack-in-command (&optional namespace)
  "Get the jack in command from pynt.json.

Check override commands first then testable then in testing
directory then matching a fallback command. If nothing matches
then return nil."
  (setq namespace (or namespace pynt-namespace)
        namespace-project-relative-path (concat (pynt-project-relative-dir) namespace))

  (defun override-command ()
    (let* ((override-cmds (alist-get 'override-commands (pynt-command-map)))
           (override-cmd (alist-get (intern namespace-project-relative-path) override-cmds)))
      override-cmd))

  (defun testable-namespace ()
    (let* ((testable-namespaces (alist-get 'testable (pynt-command-map)))
           (testable-namespaces (append testable-namespaces nil)))
      (when (member namespace-project-relative-path testable-namespaces)
          (alist-get 'runner (alist-get 'tests (pynt-command-map))))))

  (defun test ()
    (let* ((test-directory (alist-get 'directory (alist-get 'tests (pynt-command-map))))
           (test-command (alist-get 'runner (alist-get 'tests (pynt-command-map)))))
      (when (string-prefix-p test-directory namespace-project-relative-path)
        test-command)))

  (cond ((override-command) (override-command))
        ((testable-namespace) (testable-namespace))
        ((test) (test))
        (t (car (pynt-jack-in-commands)))))

(defun pynt-jack-in (command &optional namespace)
  "Jack into the current namespace via a command.

If the command is equal to the
string\"ein:connect-run-or-eval-buffer\" then just send the
buffer to EIN. Otherwise make a call out to the external script
pynt-embed to jack into the desired namespace."
  (interactive)
  (when namespace (setq pynt-namespace namespace))
  (pynt-kill-cells)
  (pynt-make-cell "Initializing your kernel..." pynt-namespace "1" -1)
  (with-current-buffer pynt-code-buffer-name
    (if (string= command "ein:connect-run-or-eval-buffer")
        (pynt-init-epc-client (format "__name__ = '%s'" pynt-namespace) 'pynt-dump-namespace)

      ;; pynt-embed will temporarily modify the underlying file. To prevent emacs
      ;; from rendering the change in buffer change the underlying file until
      ;; pynt-embed finishes.
      (set-visited-file-name "*pynt code*")

      (set-process-sentinel
       (let* ((default-directory (pynt-project-root))
              (namespace-path (concat (pynt-project-relative-dir) pynt-namespace)))
         (pynt-make-cell (format "Jacking into namespace `%s` with the command

```
$ %s
```

..." pynt-namespace command)
                         pynt-namespace
                         "markdown"
                         -1)
         (pynt-log "Calling pynt-embed -namespace %s -cmd %s ..." namespace-path command)
         (start-process "PYNT Kernel"
                        "*pynt-kernel*"
                        "pynt-embed"
                        "-namespace" namespace-path
                        "-cmd" command))
       'pynt-switch-kernel))))

(defun pynt-matching-jack-in-patterns ()
  "Get the patterns which match this file in pynt.json.

FIXME this can probably be sped up substantially."
  (defun directory-length (path) (length (split-string path "/")))
  (let* ((symbol-patterns (mapcar 'car (alist-get 'fallback-commands (pynt-command-map))))
         (patterns (mapcar 'symbol-name symbol-patterns))
         (sorted-patterns
          (reverse
           (sort patterns
                 (lambda (left right)
                   (< (directory-length left) (directory-length right))))))
         (path (pynt-project-relative-path))
         (default-directory (pynt-project-root))
         (globs (mapcar 'file-expand-wildcards sorted-patterns))
         (preds (mapcar (lambda (glob) (member path glob)) globs))
         (zip (mapcar* 'cons preds sorted-patterns))
         (survivors (seq-filter (lambda (pair) (car pair)) zip)))
    (mapcar 'cdr survivors)))

(defun pynt-jack-in-commands (&optional namespace)
  "Return the commands for NAMESPACE.

Commands are in \"pynt-project-dir/pynt.json\".

The argument NAMESPACE is the namespace to get the jack in
commands for. If NAMESPACE is not provided then default to the
active namespace.

If there are no jack in commands for NAMESPACE then look for look
for the default jack in commands identified by the identifier
\"*\"."
  (with-current-buffer pynt-code-buffer-name
    (let* ((command-map (alist-get 'fallback-commands (pynt-command-map)))
           (commands (mapcar (lambda (command) (alist-get (intern command) command-map)) (pynt-matching-jack-in-patterns))))
      commands)))

(defun pynt-choose-jack-in-command ()
  "Select the jack-in command.

Available commands include those in the file pynt.json whose key
is the variable `pynt-active-namespace'."
  (interactive)
  (helm :sources
        `((name . ,(format "Namespace *%s* jack-in commands" pynt-namespace))
          (candidates . ,(pynt-jack-in-commands))
          (action . pynt-jack-in))))

(defun pynt-jupyter-server-start ()
  "Start a jupyter notebook server.

Start it in the user's home directory and use the
`ExternalIPythonKernelManager' so we can attach to external
IPython kernels."
  (interactive)
  (let* ((server-cmd-path ein:jupyter-default-server-command)
         (notebook-directory (expand-file-name "~"))
         (extipy-args '("--NotebookApp.kernel_manager_class=codebook.ExternalIPythonKernelManager"
                        "--Session.key=b''"))
         (ein:jupyter-server-args (append ein:jupyter-server-args extipy-args)))
    (ein:jupyter-server-start server-cmd-path notebook-directory)))

(defun pynt-switch-to-namespace (namespace)
  "Switch to NAMESPACE.

This involves switching out the active notebook and also
attaching the code buffer to it.

The function `pynt-pop-up-notebook-buffer' calls `display-buffer'
which will run the `pynt-rebalance-on-window-change' hook which
will run this function again and we will get into an infinite
loop. Hence we set this lock so that the buffer and window hooks
won't run until this finishes."
  (pynt-log "Switching to namespace = %s..." namespace)
  (setq pynt-namespace namespace
        pynt-lock t)
  (pynt-pop-up-notebook-buffer (pynt-notebook-buffer))
  (setq pynt-lock nil)
  (deferred:$
    (deferred:next
      (lambda ()
        (with-current-buffer pynt-code-buffer-name
          (pynt-log "Connecting to code buffer to notebook...")
          (pynt-connect-to-notebook-buffer (pynt-notebook-buffer)))))))

(defun pynt-connect-to-notebook-buffer (notebook-buffer-name)
  (ein:connect-to-notebook-buffer notebook-buffer-name)
  (when (not (slot-value ein:%connect% 'autoexec))
    (ein:connect-toggle-autoexec)))

(defun pynt-switch-or-init (namespace)
  "Switch to NAMESPACE.

Initialize it if it has not been initialized.

This function is necessary because of the way we can create
notebooks. When we run the command `pynt-switch-namespace' if the
selected namespace has not been created then we create it. It's
like launching pynt mode for the very first time. Otherwise if it
exists we just switch to it."
  (if (gethash namespace pynt-namespace-to-notebook-map)
      (pynt-switch-to-namespace namespace)
    (pynt-init namespace)))

(defun pynt-invalidate-scroll-map (&optional a b c)
  (pynt-log "Invalidating scroll map!")
  (setq pynt-line-to-cell-map (make-hash-table :test 'equal)))

(defun pynt-init (namespace)
  "Initialize a namespace.

This involves creating a notebook if we haven't created one yet."

  ;; Globals.
  (setq pynt-lock t               ; no hooks should run until init finishes
        pynt-code-buffer-file-name (buffer-file-name))
  (add-to-list 'pynt-code-buffer-file-names (buffer-file-name))

  ;; Locals.
  (setq pynt-namespace namespace
        pynt-code-buffer-name (buffer-name))

  ;; Create new notebook.
  (pynt-new-notebook))

(defun pynt-rebalance-on-buffer-change ()
  "Try to keep code buffer and notebook consistent.

If you switch to another buffer and pynt mode is already running
then swap out the notebook for the correct one.

Meant to be added to the hook
`window-configuration-change-hook'."
  (when (and (boundp 'pynt-mode)
             pynt-mode
             (not pynt-lock)
             (member (buffer-file-name) pynt-code-buffer-file-names)
             (not (string= (buffer-file-name) pynt-code-buffer-file-name)))
    (setq pynt-code-buffer-name (buffer-name))
    (pynt-log "buffer-change: fname = %s" (buffer-file-name))
    (pynt-switch-to-namespace pynt-namespace)
    (setq pynt-code-buffer-file-name (buffer-file-name))))

(defun pynt-rebalance-on-window-change ()
  "Try to keep the code buffer and notebook consistent.

This function is needed to handle the case where we change
between windows in the same frame. Unfortunately
`window-configuration-change-hook' does not fire in that case. So
we hook into `buffer-list-update-hook' additionally."
  (let ((fname (buffer-file-name (car (buffer-list)))))
    (when (and (boundp 'pynt-mode)
               pynt-mode
               (not pynt-lock)
               (member fname pynt-code-buffer-file-names)
               (not (string= fname pynt-code-buffer-file-name)))
      (pynt-log "window-change: fname = %s" fname)
      (setq pynt-code-buffer-name (buffer-name))
      (pynt-switch-to-namespace pynt-namespace)
      (setq pynt-code-buffer-file-name fname))))

(defun pynt-mode-deactivate ()
  "Deactivate pynt mode."

  ;; Remove hooks.
  (advice-remove #'ein:connect-to-notebook-buffer #'pynt-intercept-ein-notebook-name)
  (remove-hook 'window-configuration-change-hook 'pynt-rebalance-on-buffer-change :local)
  (remove-hook 'buffer-list-update-hook 'pynt-rebalance-on-window-change :local)
  (remove-hook 'after-change-functions 'pynt-invalidate-scroll-map :local)
  (remove-hook 'post-command-hook #'pynt-scroll-cell-window :local)

  ;; Remove notebooks.
  (delete-window (pynt-notebook-window))
  (dolist (namespace (map-keys pynt-namespace-to-notebook-map))
    (with-current-buffer (pynt-notebook-buffer namespace)
      (call-interactively 'ein:notebook-kill-kernel-then-close-command)))

  ;; Remove code buffer from global list.
  (delete (buffer-file-name) pynt-code-buffer-file-names))

(defvar pynt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'pynt-dump-namespace)
    (define-key map (kbd "C-c C-w") 'pynt-recover-notebook-window)
    (define-key map (kbd "C-c C-s") 'pynt-choose-namespace)
    (define-key map (kbd "C-c C-k") 'pynt-choose-jack-in-command)
    (define-key map (kbd "<up>") 'pynt-next-cell-instance)
    (define-key map (kbd "<down>") 'pynt-prev-cell-instance)
    map))

(define-minor-mode pynt-mode
  "Minor mode for generating and interacting with jupyter notebooks via EIN

\\{pynt-mode-map}"
  :keymap pynt-mode-map
  :lighter "pynt"
  (if pynt-mode
      (progn
        ;; Hooks.
        (add-hook 'window-configuration-change-hook 'pynt-rebalance-on-buffer-change nil :local)
        (add-hook 'buffer-list-update-hook 'pynt-rebalance-on-window-change nil :local)
        (add-hook 'after-change-functions 'pynt-invalidate-scroll-map nil :local)
        (add-hook 'post-command-hook 'pynt-scroll-cell-window nil :local)

        ;; Initialize servers.
        (pynt-start-epc-server)
        (pynt-start-ast-server)

        ;; Variables.
        (setq pynt-namespace-to-notebook-map (make-hash-table :test 'equal)
              pynt-line-to-cell-map (make-hash-table :test 'equal)
              pynt-nth-cell-instance 0)

        (pynt-init (pynt-module-name)))

    (pynt-mode-deactivate)))

(when pynt-start-jupyter-server-on-startup (pynt-jupyter-server-start))

(provide 'pynt)
;;; pynt.el ends here
