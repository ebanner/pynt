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

(defcustom pynt-verbose t
  "Log pynt debug information if t and do not otherwise.")

(defcustom pynt-project-root nil
  "Project root to call pynt jack in commands from.

You must put a trailing slash on the end for it to work right!")

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

(defun pynt-make-notebook-path ()
  "Make a notebook name from NAMESPACE.

The format of the notebook name is
$HOME/path/to/curdir/NAMESPACE. This is so it can correspond to
an actual jupyter notebook file that will be saved on disk."
  (let* ((relative-path (replace-regexp-in-string (expand-file-name "~/") "" (buffer-file-name)))
         (relative-dir (file-name-directory relative-path))
         (notebook-name (concat relative-dir pynt-namespace ".ipynb")))
    notebook-name))

(defun pynt-notebook (&optional namespace)
  "Get the current notebook."
  (gethash (or namespace pynt-namespace) pynt-namespace-to-notebook-map))

(defun pynt-notebook-buffer ()
  "Get the buffer of the notebook."
  (ein:notebook-buffer (pynt-notebook)))

(defun pynt-notebook-buffer-name ()
  "Get the buffer name of the EIN notebook."
  (let ((notebook-buffer (ein:notebook-buffer (pynt-notebook))))
    (buffer-name notebook-buffer)))

(defun pynt-module-name ()
  "Extract the module-level name of the pynt code buffer.

If the buffer is associated with a python file then chop off the
'.py' suffix. Otherwise (e.g. if this is a *scratch* buffer) then
just return the buffer name.

Throw an error if the buffer name has a period in it because that
will mess with the namespace naming convention that pynt uses."
  (let* ((script-name (file-name-nondirectory (buffer-file-name))))
    (file-name-sans-extension script-name)))

(defun pynt-get-worksheet-buffer-names (namespace)
  "Get the buffer names associated with the worksheets.

Note we cannot simply use the function
`ein:worksheet--buffer-name' since we rename the buffers and the
reference appears to get stale. Hence a layer of indirection is
required where we need to get references to the buffers
themselves and then ask them for their names."
  (let* ((worksheet-buffers (mapcar 'ein:worksheet--get-buffer (ein:$notebook-worksheets (pynt-notebook namespace))))
         (worksheet-buffer-names (mapcar 'buffer-name worksheet-buffers)))
    worksheet-buffer-names))

(defun pynt-choose-namespace ()
  "Switch the active code region by selecting from a list of namespaces."
  (interactive)
  (setq pynt-namespace-to-region-map (make-hash-table :test 'equal))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (epc:call-deferred pynt-ast-server 'parse_namespaces `(,code ,(pynt-module-name)))
      (deferred:nextc it
        (lambda (namespaces)
          (helm :sources
                `((name . "Select Active Code Region")
                  (candidates . ,namespaces)
                  (action . (lambda (namespace)
                              (multiple-value-bind (name start-line end-line) namespace
                                (goto-line start-line)
                                (recenter 0)
                                (pynt-switch-or-init name)))))))))))

(defun pynt-recover-notebook-window ()
  "Recover the notebook window.

Split the notebook window and switch to the notebook buffer in
the new window.

This function is called when the user switches code buffers and
makes the notebook window disappear for some reason.

Return a reference to the new window."
  (interactive)
  (let* ((notebook (gethash (pynt-module-name) pynt-namespace-to-notebook-map))
         (notebook-buffer-name (ein:notebook-buffer notebook))
         (new-window (split-window-right)))
    (with-selected-window new-window
      (switch-to-buffer notebook-buffer-name))
    new-window))

(defun pynt-notebook-window ()
  "Get the notebook window."
  (get-buffer-window (ein:notebook-buffer (pynt-notebook))))

(defun pynt-log (&rest args)
  "Log the message when the variable `pynt-verbose' is t.

Optional argument ARGS the arguments you would normally pass to the function `message'."
  (when pynt-verbose
    (apply 'message args)))

(defun pynt-kill-cells (namespace)
  "Delete all the cells in the NAMESPACE worksheet.

Argument NAMESPACE is the namespace of the worksheet for which we
are killing all the cells.

Do nothing if the worksheet does not exist."
  (interactive)
  (with-current-buffer (pynt-notebook-buffer-name)
    (beginning-of-buffer)
    (condition-case exception
        (while t (call-interactively 'ein:worksheet-kill-cell))
      ('error))))

(defun pynt-get-buffer-names-in-frame ()
  "Get the buffer names in the active frame.

This function is mostly userd to query the active frame for the
active namespace."
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (buffer-names (mapcar 'buffer-name buffers)))
    buffer-names))

(defun pynt-query-notebook-buffer-name ()
  "Return the worksheet buffer name in this frame.

The active namespace will have a buffer in the active frame and
will have the prefix 'ns='. If there is no such window then an
error is thrown."
  (let* ((buffer-names (pynt-get-buffer-names-in-frame)))
    (multiple-value-bind (notebook-buffer-name)
        (seq-filter (lambda (buffer-name) (string-prefix-p "*ein: " buffer-name)) buffer-names)
      (if (not notebook-buffer-name)
          (error "No buffer window in the current frame starting with \"*ein: \"!")
        notebook-buffer-name))))

(defun pynt-query-namespace ()
  "Return the name of the active namespace.

The active namespace is the file name of the notebook in the
current frame sans the directory and the \".ipynb\" extension."
  (with-current-buffer (pynt-query-notebook-buffer-name)
    (let* ((notebook-path (ein:notebook-name ein:%notebook%))
           (notebook-name (file-name-nondirectory notebook-path)))
      (file-name-sans-extension notebook-name))))

(defun pynt-namespace-to-buffer-name (namespace)
  "Compute the worksheet buffer name from a namespace.

The resulting buffer name concatentates the current directory
with namespace.

Argument NAMESPACE is the namespace."
  (concat "ns=" (expand-file-name default-directory) namespace))

(defun pynt-dump-namespace ()
  "Dump the code in `pynt-active-namespace' into its EIN worksheet buffer.

This is done by sending the code region out to the AST server
where it is annotated with EPC calls and then the resulting code
is sent to the IPython kernel to be executed."
  (interactive)
  (setq pynt-lock nil)
  (pynt-kill-cells pynt-namespace)
  (message "Notebook kernel session ID = %s" (ein:$kernel-session-id (pynt-notebook-kernel)))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (message "Dumping the namespace!")
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
  (save-selected-window
    (let ((cells (gethash (line-number-at-pos) pynt-line-to-cell-map)))
      (when cells
        (condition-case exception
            (let* ((cell (nth pynt-nth-cell-instance cells))
                   (cell-marker (ein:cell-location cell :input))
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
          ('error))))))

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

(defun pynt-notebook-kernel ()
  (ein:$notebook-kernel (pynt-notebook)))

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
  "Rename the notebook to the current namespace."

  (message "Setting notebook name variable...")
  (setq pynt-notebook-path (pynt-make-notebook-path))

  (delete-file (file-name-nondirectory pynt-notebook-path))

  ;; Poll until notebook file is deleted.
  (deferred:nextc it
    (deferred:lambda (msg)
      (if (not (file-exists-p (file-name-nondirectory pynt-notebook-path)))
          (progn
            (message "%s is deleted! Renaming notebook to %s..." (file-name-nondirectory pynt-notebook-path) pynt-notebook-path)
            'notebook-is-deleted)
        (message "%s still exists..." (file-name-nondirectory pynt-notebook-path))
        (deferred:nextc (deferred:wait 500) self))))

  ;; Rename the notebook.
  (deferred:nextc it
    (lambda (msg)
      (with-current-buffer pynt-code-buffer-name
        (with-current-buffer (pynt-notebook-buffer-name)
          (ein:notebook-rename-command pynt-notebook-path)))
      'notebook-rename-initiated))

  ;; Poll until notebook is renamed.
  (deferred:next
    (deferred:lambda ()
      (with-current-buffer pynt-code-buffer-name
        (if (string= (ein:$notebook-notebook-path (pynt-notebook)) pynt-notebook-path)
            (progn
              (message "[2] : Notebook name is set!")
              'pynt-connected)
          (message "Still waiting for notebook name to change...")
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
    (message "Putting %s into hash..." pynt-namespace)
    (puthash pynt-namespace notebook pynt-namespace-to-notebook-map)
    (pynt-connect-to-notebook-buffer (pynt-notebook-buffer-name)))

  (deferred:$
    ;; Poll until code buffer is connected.
    (deferred:next
      (deferred:lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (if ein:connect-mode
              (message "Code buffer connected!")
            (message "Code buffer not connected...")
            (deferred:nextc (deferred:wait 500) self)))))

    ;; Dump the module-level namespace. Possibly jack in first.
    (deferred:nextc it
      (lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (let ((command (car (pynt-jack-in-commands pynt-namespace))))
            (when command
              (if (string= command "ein:connect-run-or-eval-buffer")
                  (pynt-init-epc-client (format "__name__ = '%s'" pynt-namespace) 'pynt-dump-namespace)
                (pynt-jack-in command)))))))))

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

(defun pynt-toggle-debug ()
  "Toggle pynt development mode.

In pynt development mode we set the print-* variables to values
so that when we try and print EIN deeply nested and recursive
data structures they print and do not lock up emacs."
  (interactive)
  (setq pynt-verbose (if pynt-verbose nil t)
        print-level (if print-level nil 1)
        print-length (if print-length nil 1)
        print-circle (if print-circle nil t))
  (if ein:debug
      (ein:dev-stop-debug)
    (ein:dev-start-debug)))

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
            (message "The exception was hit in the namespace = %s" namespace-buffer-name)
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
          (message "The exception was hit in the namespace = %s" namespace-buffer-name)
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
  (with-current-buffer (pynt-notebook-buffer-name)
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (let ((cell (ein:get-cell-at-point))
          (ws (ein:worksheet--get-ws-or-error)))
      (cond ((string= cell-type "code") (ein:cell-set-autoexec cell t))
            ((string= cell-type "markdown") (ein:worksheet-change-cell-type ws cell "markdown"))
            (t (ein:worksheet-change-cell-type ws cell "heading" (string-to-number cell-type))))
      (setq new-cell cell)))
  (when (not (eq line-number -1))
    (let ((previous-cells (gethash line-number pynt-line-to-cell-map)))
      (puthash line-number (append previous-cells (list new-cell)) pynt-line-to-cell-map))))

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
                (message "Kernel is live!")
                'kernel-is-live)
            (message "Kernel not live...")
            (deferred:nextc (deferred:wait 500) self)))))

    ;; Evaluate init code.
    (deferred:nextc it
      (lambda (msg)
        (let ((pynt-init-code (format pynt-init-code-template pynt-epc-server-hostname pynt-epc-port additional-code)))
          (message "Evaluating initial code!")
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

(defun pynt-narrow-code (namespace)
  "Narrow the code buffer to the region defined by `NAMESPACE'."
  (let ((location (gethash namespace pynt-namespace-to-region-map)))
    (when location
      (multiple-value-bind (code-buffer-name start-line end-line) location
        (progn
          (widen)
          (beginning-of-buffer)
          (when (and (/= start-line -1) (/= end-line -1))
            (goto-line start-line)
            (setq start (point))
            (goto-line end-line)
            (setq end (point))
            (narrow-to-region start end)
            (beginning-of-buffer)))))))

(defun pynt-delete-worksheet (worksheet-name)
  "Delete the EIN worksheet with buffer name of `WORKSHEET-NAME'.

This is called for every EIN worksheet when pynt mode is
deactivated."
  (interactive)
  (when (get-buffer worksheet-name)
    (with-current-buffer worksheet-name
      (ein:notebook-worksheet-delete (ein:notebook--get-nb-or-error) (ein:worksheet--get-ws-or-error) nil))))

(defun pynt-switch-kernel (process event)
  "Switch to most recently started kernel.

Because we are using the ExternalIPythonKernelManager issuing a
restart to the jupyter notebook causes us to switch to the most
recently started kernel.

This function is meant to be used as a sentinel after the process
pynt-embed finishes."
  (select-window (get-buffer-window "*pynt code*"))
  (message "Setting the visited file name by to %s..." pynt-code-buffer-file-name)
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
                (message "Kernel killed!")
                'kernel-killed)
            (message "Kernel not killed yet. Polling...")
            (deferred:nextc (deferred:wait 10) self)))))

    ;; Initialize the EPC client and dump the namespace.
    (deferred:nextc it
      (lambda (msg)
        (with-current-buffer pynt-code-buffer-name
          (pynt-init-epc-client "pass" 'pynt-dump-namespace))))))

(defun pynt-run-all-cells-above ()
  "Execute all cells above and including the cell at point."
  (interactive)
  (save-excursion
    (let ((end-cell (ein:get-cell-at-point)))
      (beginning-of-buffer)
      (setq cell (ein:get-cell-at-point))
      (while (not (eq cell end-cell))
        (call-interactively 'ein:worksheet-execute-cell-and-goto-next)
        (setq cell (ein:get-cell-at-point)))
      (call-interactively 'ein:worksheet-execute-cell))))

(defun pynt-get-project-root ()
  "Get the project root.

Use the variable `pynt-project-root' first. If that is not set
then ask magit for the closest enclosing github repo root."
  (or pynt-project-root (magit-toplevel)))

(defun pynt-relative-curdir-path ()
  "Relative path of the current directory from the project root."
  (let* ((project-path (expand-file-name (pynt-get-project-root)))
         (absolute-dir-path (file-name-directory (buffer-file-name)))
         (relative-dir-path (replace-regexp-in-string project-path "" absolute-dir-path)))
    relative-dir-path))

(defun pynt-jack-in (command &optional namespace)
  "Jack into the current namespace via a command.

If the command is equal to the
string\"ein:connect-run-or-eval-buffer\" then just send the
buffer to EIN. Otherwise make a call out to the external script
pynt-embed to jack into the desired namespace."
  (interactive)
  (when namespace (setq pynt-namespace namespace))
  (with-current-buffer pynt-code-buffer-name
    (message "Jacking into namespace = %s with command = %s..." pynt-namespace command)
    (if (string= command "ein:connect-run-or-eval-buffer")
        (pynt-dump-namespace)

      ;; pynt-embed will temporarily modify the underlying file. To prevent emacs
      ;; from rendering the change in buffer change the underlying file until
      ;; pynt-embed finishes.
      (set-visited-file-name "*pynt code*")

      (set-process-sentinel
       (let* ((default-directory (pynt-get-project-root))
              (namespace-path (concat (pynt-relative-curdir-path) pynt-namespace)))
         (message "Calling pynt-embed -namespace %s -cmd %s..." namespace-path command)
         (start-process "PYNT Kernel"
                        "*pynt-kernel*"
                        "pynt-embed"
                        "-namespace" namespace-path
                        "-cmd" command))
       'pynt-switch-kernel))))

(defun pynt-config-file ()
  "Return a path to the project config file."
  (concat (pynt-get-project-root) "pynt.json"))

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
    (let* ((namespace-to-cmd-map (json-read-file (pynt-config-file)))
           (namespace (or namespace pynt-namespace))
           (namespace (concat (pynt-relative-curdir-path) namespace))
           (namespace (intern namespace))
           (namespace-cmds (alist-get namespace namespace-to-cmd-map))
           (namespace-cmds (or namespace-cmds (alist-get '* namespace-to-cmd-map))))
      (append namespace-cmds nil))))

(defun pynt-choose-jack-in-command ()
  "Select the jack-in command.

Available commands include those in the file pynt.json whose key
is the variable `pynt-active-namespace'."
  (interactive)
  (let ((namespace-cmds (pynt-jack-in-commands)))
    (helm :sources
          `((name . "Select Jack-In Command")
            (candidates . ,namespace-cmds)
            (action . pynt-jack-in)))))

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

(defun pynt-eval-buffer ()
  "Send the buffer to the EIN notebook for evaluation."
  (let ((current-prefix-arg 4))
    (call-interactively 'ein:connect-run-or-eval-buffer)))

(defun pynt-switch-to-worksheet (namespace)
  "Switch the worksheet in the current view.

ARGUMENT namepsace-buffer-names is a string which contains the
buffer name of the worksheet to switch to."
  (let ((worksheet-buffer-name (pynt-namespace-to-buffer-name namespace)))
    (with-selected-window (pynt-notebook-window)
      (switch-to-buffer worksheet-buffer-name))))

(defun pynt-switch-to-namespace (namespace)
  "Switch to NAMESPACE.

This involves switching out the active notebook and also
attaching the code buffer to it.

The function `pynt-pop-up-notebook-buffer' calls `display-buffer'
which will run the `pynt-rebalance-on-window-change' hook which
will run this function again and we will get into an infinite
loop. Hence we set this lock so that the buffer and window hooks
won't run until this finishes."
  (setq pynt-namespace namespace
        pynt-lock t)
  (pynt-pop-up-notebook-buffer (pynt-notebook-buffer))
  (setq pynt-lock nil)
  (deferred:$
    (deferred:next
      (lambda ()
        (message "Connecting to code buffer to notebook...")
        (pynt-connect-to-notebook-buffer (pynt-notebook-buffer-name))))))

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
  (message "Invalidating scroll map!")
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
    (message "buffer-change: fname = %s" (buffer-file-name))
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
      (message "window-change: fname = %s" fname)
      (pynt-switch-to-namespace pynt-namespace)
      (setq pynt-code-buffer-file-name fname))))

(defun pynt-mode-deactivate ()
  "Deactivate pynt mode."

  ;; Remove advice.
  (advice-remove #'ein:connect-to-notebook-buffer #'pynt-intercept-ein-notebook-name)

  ;; Remove notebook worksheets and pynt window.
  (dolist (namespace (map-keys pynt-namespace-to-notebook-map))
    (dolist (worksheet-buffer-name (pynt-get-worksheet-buffer-names namespace))
      (pynt-delete-worksheet worksheet-buffer-name)))
  (delete-window (pynt-notebook-window))

  ;; Remove code buffer from global list.
  (delete (buffer-file-name) pynt-code-buffer-file-names)

  ;; Deactivate pynt scroll mode.
  (pynt-scroll-mode -1))

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
        (add-hook 'post-command-hook #'pynt-scroll-cell-window nil :local)

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
