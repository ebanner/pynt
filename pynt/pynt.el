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

(defcustom pynt-verbose nil
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
IPython.get_ipython().set_custom_exc(exc_tuple=(Exception,), handler=handler)

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

(defvar-local pynt-notebook nil
  "Reference to the EIN notebook.")

(defvar-local pynt-line-to-cell-map nil
  "Map of source code lines to EIN cell(s).

A source code line may be associated with more than one EIN
cell (e.g. a line in the body of a for loop.")

(defvar-local pynt-namespace-to-region-map nil
  "Map of namespace names to start and end lines.

This map is used to produce a visual indication of which
namespace corresponds to which code. It was originally part of a
feature that was purely intended for making video demos prettier
but does serve as a way to intuitively select a region of code.

This map is used after a user changes the active namespace via
the command `pynt-select-namespace'.")

(defun pynt-notebook-buffer-name ()
  "Get the buffer name of the EIN notebook."
  (let ((notebook-buffer (ein:notebook-buffer pynt-notebook)))
    (buffer-name notebook-buffer)))

(defun pynt-module-name ()
  "Extract the module-level name of the pynt code buffer.

If the buffer is associated with a python file then chop off the
'.py' suffix. Otherwise (e.g. if this is a *scratch* buffer) then
just return the buffer name.

Throw an error if the buffer name has a period in it because that
will mess with the namespace naming convention that pynt uses."
  (let* ((script-name (file-name-nondirectory (buffer-file-name)))
         (namespace-tokens (nbutlast (split-string script-name "\\.py") 1)))
    (if (or (> (length namespace-tokens) 1)
            (string-match-p (regexp-quote "=") (car namespace-tokens)))
        (error "Buffer name cannot contain '.' nor '='.  Rename your buffer and try again!")
      (car namespace-tokens))))

(defun pynt-get-worksheet-buffer-names ()
  "Get the buffer names associated with the worksheets.

Note we cannot simply use the function
`ein:worksheet--buffer-name' since we rename the buffers and the
reference appears to get stale. Hence a layer of indirection is
required where we need to get references to the buffers
themselves and then ask them for their names."
  (let* ((worksheet-buffers (mapcar 'ein:worksheet--get-buffer (ein:$notebook-worksheets pynt-notebook)))
         (worksheet-buffer-names (mapcar 'buffer-name worksheet-buffers)))
    worksheet-buffer-names))

(defun pynt-get-namespaces ()
    "Get all of the namespaces.

Query the EIN notebook and don't call out to the AST server for
them."
  (mapcar 'file-name-nondirectory (cdr (pynt-get-worksheet-buffer-names))))

(defun pynt-select-namespace ()
  "Switch the active code region by selecting from a list of namespaces."
  (interactive)
  (helm :sources
        `((name . "Select Active Code Region")
          (candidates . ,(pynt-get-namespaces))
          (action . (lambda (namespace)
                      (pynt-switch-to-worksheet namespace)
                      (pynt-narrow-code namespace)
                      (message "Type 'C-c C-e' to dump *%s* into the notebook!" namespace))))))

(defun pynt-get-notebook-window ()
  "Get the EIN notebook window.

A reference to this window cannot be cached into a variable
because if users delete the window and then bring it back up,
it's a different window now."
  (interactive)
  (multiple-value-bind (worksheet-buffer-name)
      (seq-filter 'get-buffer-window (pynt-get-worksheet-buffer-names))
    (get-buffer-window worksheet-buffer-name)))

(defun pynt-log (&rest args)
  "Log the message when the variable `pynt-verbose' is t.

Optional argument ARGS the arguments you would normally pass to the function `message'."
  (when pynt-verbose
    (apply #'message args)))

(defun pynt-kill-cells (namespace)
  "Delete all the cells in the NAMESPACE worksheet.

Argument NAMESPACE is the namespace of the worksheet for which we
are killing all the cells.

Do nothing if the worksheet does not exist."
  (interactive)
  (let ((worksheet-buffer-name (pynt-get-worksheet-buffer-name-from namespace)))
    (when (get-buffer worksheet-buffer-name)
      (with-current-buffer worksheet-buffer-name
        (beginning-of-buffer)
        (condition-case exception
            (while t (call-interactively 'ein:worksheet-kill-cell))
          ('error))))))

(defun pynt-kill-all-cells ()
  "Kill cells in all EIN worksheets.

This function mainly exists to clear out each EIN worksheet
shortly after calling the function
`pynt-make-namespace-worksheets' to start them each with a blank
slate."
  (interactive)
  (dolist (worksheet-name (pynt-get-worksheet-buffer-names))
    (pynt-kill-cells worksheet-name)))

(defun pynt-get-buffer-names-in-frame ()
  "Get the buffer names in the active frame.

This function is mostly userd to query the active frame for the
active namespace."
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (buffer-names (mapcar 'buffer-name buffers)))
    buffer-names))

(defun pynt-get-worksheet-buffer-name ()
  "Return the worksheet buffer name in this frame.

The active namespace will have a buffer in the active frame and
will have the prefix 'ns='. If there is no such window then an
error is thrown."
  (let* ((buffer-names (pynt-get-buffer-names-in-frame))
         (active-buffer-singleton (seq-filter
                                   (lambda (buffer-name) (string-prefix-p "ns=" buffer-name))
                                   buffer-names))
         (active-buffer-name (car active-buffer-singleton)))
    (if (not active-buffer-name)
        (error "No window in the current frame whose buffer name is prefixed with 'ns='!")
      active-buffer-name)))

(defun pynt-get-active-namespace ()
  "Return the name of the active namespace.

The active namespace is the buffer name of the window in the
active frame sans the leading \"ns=\"."
  (multiple-value-bind (empty namespace) (split-string (pynt-get-worksheet-buffer-name) "ns=")
    (file-name-nondirectory namespace)))

(defun pynt-make-namespace-worksheets ()
  "Parse out the namespaces and create namespace worksheets.

This is the last function called after activating pynt mode. The
command `pynt-select-namespace' is ready to be called after this
function completes."
  (interactive)
  (dolist (worksheet-buffer-name (cdr (pynt-get-worksheet-buffer-names))) ; don't delete the first one!
    (pynt-delete-worksheet worksheet-buffer-name))
  (setq pynt-namespace-to-region-map (make-hash-table :test 'equal))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (epc:call-deferred pynt-ast-server 'parse_namespaces `(,code ,(pynt-module-name)))
      (deferred:nextc it
        (lambda (namespaces)
          (pynt-log "Namespaces = %s" namespaces)
          (dolist (namespace namespaces)
            (multiple-value-bind (name start-line end-line) namespace
              (when (not (get-buffer name))
                (progn
                  (pynt-create-new-worksheet name)
                  (pynt-kill-cells name)
                  (puthash name (list (buffer-name) start-line end-line) pynt-namespace-to-region-map))))))))))

(defun pynt-get-worksheet-buffer-name-from (namespace)
  "Compute the worksheet buffer name from a namespace.

The resulting buffer name concatentates the current directory
with namespace.

Argument NAMESPACE is the namespace."
  (concat "ns=" default-directory namespace))

(defun pynt-execute-current-namespace ()
  "Dump the code in `pynt-active-namespace' into its EIN worksheet buffer.

This is done by sending the code region out to the AST server
where it is annotated with EPC calls and then the resulting code
is sent to the IPython kernel to be executed."
  (interactive)
  (setq pynt-line-to-cell-map (make-hash-table :test 'equal))
  (widen)
  (pynt-kill-cells (pynt-get-active-namespace))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (pynt-log "Calling python AST server with active namespace = %s ..." (pynt-get-active-namespace))
      (epc:call-deferred pynt-ast-server 'annotate `(,code ,(pynt-get-active-namespace)))
      (deferred:nextc it
        (lambda (annotated-code)
          (pynt-log "Annotated code = %S" annotated-code)
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
  (when pynt-line-to-cell-map           ; sometimes this is nil!
    (save-selected-window
      (let ((cells (gethash (line-number-at-pos) pynt-line-to-cell-map)))
        (when cells
          (condition-case exception
              (let* ((cell (nth pynt-nth-cell-instance cells))
                     (cell-marker (ein:cell-location cell :input))
                     (point-line (count-screen-lines (window-start) (point)))
                     (window (get-buffer-window (pynt-get-worksheet-buffer-name))))
                (when (and cell-marker (string= (buffer-name (marker-buffer cell-marker)) (pynt-get-worksheet-buffer-name)))
                  (select-window window)
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

(defun pynt-new-notebook (url-or-port)
  "Create a new EIN notebook and bring it up side-by-side.

Make sure the new notebook is created in the same directory as
the python file so that relative imports in the code work fine.

The argument URL-OR-PORT is the url or port of the jupyter
notebook server to connect to."
  (interactive)
  (save-selected-window
    (let* ((path (buffer-file-name))
           (dir-path (substring (file-name-directory path) 0 -1))
           (home-dir (concat (expand-file-name "~") "/"))
           (nb-dir (replace-regexp-in-string home-dir "" dir-path))
           (notebook-list-buffer-name (concat "*ein:notebooklist " url-or-port "*")))
      (with-current-buffer notebook-list-buffer-name
        (ein:notebooklist-new-notebook url-or-port nil nb-dir)))
    (sit-for 1)))

(defun pynt-toggle-debug ()
  "Toggle pynt development mode.

In pynt development mode we set the print-* variables to values
so that when we try and print EIN deeply nested and recursive
data structures they print and do not lock up emacs."
  (interactive)
  (setq print-level (if print-level nil 1)
        print-length (if print-length nil 1)
        print-circle (if print-circle nil t)))

(defun pynt-create-new-worksheet (namespace)
  "Create a new worksheet in the EIN notebook.

Argument NAMESPACE the name of the namespace to create a
worksheet for.

The buffer name will be $PWD/namespace in order to avoid emacs
buffer naming collisions for having the same file name in
different directories."
  (interactive)
  (let ((new-worksheet-buffer-name (pynt-get-worksheet-buffer-name-from namespace)))
    (save-excursion
      (save-window-excursion
        (with-current-buffer (pynt-notebook-buffer-name)
          (call-interactively 'ein:notebook-worksheet-insert-next)
          (rename-buffer new-worksheet-buffer-name))))))

(defun pynt-start-epc-server ()
  "Start the EPC server and register its associated callback.

The EPCS server's job is to relay commands to create an execute
EIN cells from the python EPC client."
  (defun handle-make-cell (&rest args)
    (multiple-value-bind (expr buffer-name cell-type line-number) args
      (pynt-make-cell expr buffer-name cell-type (string-to-number line-number))
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
          (pynt-execute-current-namespace)))
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
        (with-selected-window (pynt-get-notebook-window) (switch-to-buffer namespace-buffer-name)))
      (sit-for 5)
      (pynt-switch-kernel (pynt-get-latest-kernel-id))
      (pynt-execute-current-namespace)
      nil))

  (let ((connect-function
         (lambda (mngr)
           (let ((mngr mngr))
             (epc:define-method mngr 'make-cell 'handle-make-cell)
             (epc:define-method mngr 'report-exception 'handle-report-exception)
             (epc:define-method mngr 'python-exception 'handle-python-exception)))))
    (setq pynt-epc-server (epcs:server-start connect-function pynt-epc-port))))

(defun pynt-make-cell (expr buffer-name cell-type line-number)
  "Make a new EIN cell and evaluate it.

Insert a new code cell with contents EXPR into the worksheet
buffer BUFFER-NAME with cell type CELL-TYPE at the end of the
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
  (pynt-log "(pytn-make-cell %S %S %S %S)..." expr buffer-name cell-type line-number)

  ;; These variables are buffer local so we need to grab them before switching
  ;; over to the worksheet buffer.
  (setq new-cell nil)                   ; new cell to be added
  (with-current-buffer (pynt-get-worksheet-buffer-name-from buffer-name)
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (let ((cell (ein:get-cell-at-point))
          (ws (ein:worksheet--get-ws-or-error)))
      (cond ((string= cell-type "code") (call-interactively 'ein:worksheet-execute-cell))
            ((string= cell-type "markdown") (ein:worksheet-change-cell-type ws cell "markdown"))
            (t (ein:worksheet-change-cell-type ws cell "heading" (string-to-number cell-type))))
      (setq new-cell cell)))
  (when (not (eq line-number -1))
    (let ((previous-cells (gethash line-number pynt-line-to-cell-map)))
      (puthash line-number (append previous-cells (list new-cell)) pynt-line-to-cell-map))))

(defun pynt-start-ast-server ()
  "Start python AST server."
  (setq pynt-ast-server (epc:start-epc "pynt-serve" nil)))

(defun pynt-init-epc-client ()
  "Initialize the EPC client for the EIN notebook.

This needs to be done so python can send commands to Emacs to
create code cells. Use the variables
`pynt-epc-server-hostname' and `pynt-epc-port' to define
the communication channels for the EPC client."
  (let ((pynt-init-code (format pynt-init-code-template pynt-epc-server-hostname pynt-epc-port)))
    (ein:shared-output-eval-string pynt-init-code)))

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

(defun pynt-attach-to-running-kernel (process event)
  "Attach to the most recently started kernel."
  (interactive)
  (message "Process: %s had the event %s" process event)
  (ein:notebook-restart-kernel pynt-notebook)
  (sit-for 5)
  (pynt-init-epc-client)
  (pynt-eval-buffer))

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
  (if (string= command "ein:connect-run-or-eval-buffer")
      (pynt-eval-buffer)
    (set-process-sentinel
     (let* ((default-directory (pynt-get-project-root))
            (namespace (or namespace (pynt-get-active-namespace)))
            (namespace-path (concat (pynt-relative-curdir-path) namespace)))
       (pynt-log "Calling pynt-embed -namespace %s -cmd %s..." namespace-path command)
       (start-process "PYNT Kernel"
                      "*pynt-kernel*"
                      "pynt-embed"
                      "-namespace" namespace-path
                      "-cmd" command))
     'pynt-attach-to-running-kernel)))

(defun pynt-config-file ()
  "Return a path to the project config file."
  (concat (pynt-get-project-root) "pynt.json"))

(defun pynt-get-jack-in-commands (&optional namespace)
  "Return the commands for NAMESPACE.

Commands are in \"pynt-project-dir/pynt.json\".

The argument NAMESPACE is the namespace to get the jack in
commands for. If NAMESPACE is not provided then default to the
active namespace.

If there are no jack in commands for NAMESPACE then look for look
for the default jack in commands identified by the identifier
\"*\"."
  (let* ((namespace-to-cmd-map (json-read-file (pynt-config-file)))
         (namespace (or namespace (pynt-get-active-namespace)))
         (namespace (concat (pynt-relative-curdir-path) namespace))
         (namespace (intern namespace))
         (namespace-cmds (alist-get namespace namespace-to-cmd-map))
         (namespace-cmds (or namespace-cmds (alist-get '* namespace-to-cmd-map))))
    (append namespace-cmds nil)))

(defun pynt-select-jack-in-command ()
  "Select the jack-in command.

Available commands include those in the file pynt.json whose key
is the variable `pynt-active-namespace'."
  (interactive)
  (let* ((namespace-cmds (pynt-get-jack-in-commands)))
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
  (let ((worksheet-buffer-name (pynt-get-worksheet-buffer-name-from namespace)))
    (with-selected-window (pynt-get-notebook-window)
      (switch-to-buffer worksheet-buffer-name))))

(defun pynt-mode-activate ()
  "Initialize pynt mode."

  ;; If the user has already got their notebook and code side-by-side then just
  ;; prompt them to confirm that's the notebook they want to use. Otherwise
  ;; create a new notebook and line it up side-by-side.
  (if (not (intersection (ein:notebook-opened-buffer-names) (pynt-get-buffer-names-in-frame)))
      (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
        (pynt-new-notebook url-or-port))
    (advice-add #'ein:connect-to-notebook-buffer :around #'pynt-intercept-ein-notebook-name)
    (call-interactively 'ein:connect-to-notebook-buffer))

  ;; Capture the code buffer name, worksheet buffer name, and maps that will
  ;; link lines of code to their corresponding cells for scrolling.
  (multiple-value-bind (notebook-buffer-name)
      (intersection (ein:notebook-opened-buffer-names) (pynt-get-buffer-names-in-frame))
    (setq notebook nil)
    (with-current-buffer notebook-buffer-name (setq notebook ein:%notebook%))
    (setq pynt-notebook notebook))

    ;; Set up EPC and AST servers but only if this is the first time we are
    ;; starting pynt mode in this emacs session!
    (when (and (not pynt-epc-server) (not pynt-ast-server))
      (pynt-start-epc-server)
      (pynt-start-ast-server))

    ;; Connect the code buffer to the notebook instance, send the code buffer
    ;; over to the notebook kernel to get top-level definitions defined, and
    ;; initialize an EPC client in that notebook so it can talk to the EPC
    ;; server.
    (ein:connect-to-notebook-buffer (pynt-notebook-buffer-name))
    (sit-for 2)
    (pynt-init-epc-client)

    ;; Parse out the code regions so we can select one right away and start
    ;; coding!
    (pynt-make-namespace-worksheets)

    ;; Jack in to the appropriate environment so you can start running stuff
    ;; right away with no additional effort required.
    (let ((command (car (pynt-get-jack-in-commands (pynt-module-name)))))
      (when command
        (if (not (string= command "ein:connect-run-or-eval-buffer"))
            (pynt-jack-in command (pynt-module-name))
          (ein:shared-output-eval-string (format "__name__ = '%s'" pynt-module-level-namespace))
          (pynt-eval-buffer)))))

(defun pynt-mode-deactivate ()
  "Deactivate pynt mode."

  ;; Remove advice
  (advice-remove #'ein:connect-to-notebook-buffer #'pynt-intercept-ein-notebook-name)

  ;; Remove notebook worksheets and pynt window
  (dolist (worksheet-buffer-name (cdr (pynt-get-worksheet-buffer-names))) (pynt-delete-worksheet worksheet-buffer-name))
  (delete-window (pynt-get-notebook-window))
  (pynt-delete-worksheet (pynt-notebook-buffer-name))

  ;; Deactivate pynt scroll mode
  (pynt-scroll-mode -1))

(defvar pynt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'pynt-execute-current-namespace)
    (define-key map (kbd "C-c C-w") 'pynt-make-namespace-worksheets)
    (define-key map (kbd "C-c C-s") 'pynt-select-namespace)
    (define-key map (kbd "C-c C-k") 'pynt-select-jack-in-command)
    map))

(define-minor-mode pynt-mode
  "Minor mode for generating and interacting with jupyter notebooks via EIN

\\{pynt-mode-map}"
  :keymap pynt-mode-map
  :lighter "pynt"
  (if pynt-mode (pynt-mode-activate) (pynt-mode-deactivate)))

(defvar pynt-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'pynt-next-cell-instance)
    (define-key map (kbd "<down>") 'pynt-prev-cell-instance)
    map))

(define-minor-mode pynt-scroll-mode
  "Minor mode for scrolling a EIN notebook side-by-side with code.

\\{pynt-scroll-mode-map}"
  :keymap pynt-scroll-mode-map
  :lighter "pynt-scroll"
  (if pynt-scroll-mode
      (progn
        (add-hook 'post-command-hook #'pynt-scroll-cell-window :local))
    (remove-hook 'post-command-hook #'pynt-scroll-cell-window))
  (setq pynt-nth-cell-instance 0))

(when pynt-start-jupyter-server-on-startup (pynt-jupyter-server-start))

(provide 'pynt)
;;; pynt.el ends here
