;;; pynt.el --- Generate jupyter notebooks from python via EIN  -*- lexical-binding: t -*-

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Edward Banner <edward.banner@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4") (ein "0.13.1") (epc "0.1.1") (deferred "0.5.1"))
;; Keywords: convenience
;; URL: https://github.com/ebanner/pynt

;;; Commentary:

;; pynt provides a minor mode `pynt-mode' to generate and interact with jupyter
;; notebooks via EIN from code in a python buffer.

;;; Code:

(require 'seq)
(require 'epc)
(require 'epcs)
(require 'ein-jupyter)

(defgroup pynt nil
  "Customization group for pynt."
  :group 'applications)

;;; Variables
(defcustom pynt-elisp-relay-server-hostname "localhost"
  "The hostname of the elisp relay server.

Usually 'localhost' but if the jupyter kernel is running inside a
docker container then this value should be
'docker.for.mac.localhost' when on a mac."
  :options '("localhost" "docker.for.mac.localhost"))

(defcustom pynt-scroll-narrow-view nil
  "Narrow the notebook buffer if t and don't otherwise."
  :options '(nil t))

(defcustom pynt-epc-port 9999
  "The port that the current EPC client and server are communicating on.

Every invocation of the command `pynt-mode' increments this
number so there can be multiple EPC client-server pairs.")

(defcustom pynt-start-jupyter-server-on-startup t
  "Whether to start a jupyter server on startup.

Start a jupyter server on the port defined by the variable
`ein:url-or-port' if t and do nothing otherwise.")

(defvar pynt-init-code-template
  "

%%matplotlib inline

from epc.client import EPCClient
import time

epc_client = EPCClient(('%s', %s), log_traceback=True)

def __cell__(content, buffer_name, cell_type, line_number):
    elisp_func = 'make-cell'
    epc_client.call_sync(elisp_func, args=[content, buffer_name, cell_type, line_number])
    time.sleep(0.01)

__name__ = '__pynt__'

"
  "Python code template which is evaluated in the current notebook..

The value of `pynt-elisp-relay-server-hostname' and
`pynt-epc-port' are used to complete this template.")

(defcustom pynt-verbose nil
  "Logging flag.

Log debugging information if t and do not otherwise.")

(defvar-local pynt-module-level-namespace ""
  "The module-level namespace of the buffer.

This is computed from the buffer name *once* when you activate
pynt. It should never change after that.")

(defvar-local pynt-active-namespace ""
  "The active function name.

This is the function who will have code cells made from
it/annotated/rolled out/whatever you want to call it. Needs to be
the name of a python function in the current buffer or the value
'outside' to indicate the code outside of any function.")

(defvar-local pynt-active-namespace-buffer-name (format "ns=%s" pynt-active-namespace)
  "The buffer name of the active namespace.

When you run the command `pynt-execute-current-namespace' this is the
buffer that will have code cells added to it and evaluated. Is
always of the form 'ns=`pynt-active-namespace''")

(defvar-local pynt-notebook-buffer-name ""
  "Buffer name of the EIN notebook

This variable holds the name of the notebook associated with the
current `pynt-mode' session. The value gets set when you can
`pynt-mode' and choose a EIN notebook name.'")

(defvar-local pynt-namespaces nil
  "List of namespaces in the code.

The value of this variable via a call to the function
`pynt-make-namespace-worksheets'.")

(defvar-local pynt-notebook-buffer-names nil
  "List of buffer names in the notebook.")

(defvar pynt-line-to-cell-map nil
  "Table mapping of source code line to EIN cell(s).

A source code line may be associated with many EIN cells (e.g. a
line in the body of a for loop.")

(defvar pynt-namespace-to-region-map nil
  "Map of namespace names to start and end lines.

This map is used to produce a visual indication of which
namespace corresponds to which code.")

(defvar-local pynt-elisp-relay-server nil "Elisp relay server")
(defvar-local pynt-ast-server nil "Python AST server")

(defun pynt-get-module-level-namespace ()
  "Extract the module-level name of the pynt text buffer.

If the buffer is associated with a python file then chop off the
'.py' suffix.  Otherwise (e.g. if this is a *scratch* buffer just
retrun the buffer name.  Throw an error if the buffer name has a
period in it because that will mess with the naming of namespaces
that pynt uses."
  (if (string-suffix-p ".py" (buffer-name))
      (let ((namespace-tokens (nbutlast (split-string (buffer-name) "\\.py") 1)))
        (if (or (> (length namespace-tokens) 1)
                (string-match-p (regexp-quote "=") (car namespace-tokens)))
            (error "Buffer name cannot contain '.' nor '='.  Rename your buffer and try again!")
          (car namespace-tokens)))
    (buffer-name)))

(defun pynt-select-namespace ()
  "Switch the active namespace.

Choose from a helm popup list."
  (interactive)
  (helm :sources
        `((name . "Select Active Code Region")
          (candidates . ,pynt-namespaces)
          (action . (lambda (namespace)
                      (with-selected-window (pynt-get-notebook-window) (switch-to-buffer namespace))
                      (pynt-narrow-code namespace)
                      (message "Type 'C-c C-e' to dump *%s* into the notebook!" namespace))))))

(defun pynt-get-notebook-window ()
  "Get the EIN notebook window."
  (interactive)
  (let* ((notebook-buffer-names (append (list pynt-notebook-buffer-name) pynt-notebook-buffer-names))
         (active-notebook-buffer-names-singleton (seq-filter 'get-buffer-window notebook-buffer-names))
         (active-notebook-buffer-name (car active-notebook-buffer-names-singleton)))
    (get-buffer-window active-notebook-buffer-name)))

(defun pynt-log (&rest args)
  "Log the message when the variable `pynt-verbose' is `t.

Optional argument ARGS the arguments you would normally pass to the function `message'.'."
  (when pynt-verbose
    (apply #'message args)))

(defun pynt-kill-cells (worksheet-buffer-name)
  "Pop over to the worksheet buffer and delete all the cells.

Do nothing if the buffer does not exist.
Argument WORKSHEET-BUFFER-NAME the buffer name of the worksheet for which we are killing all the cells."
  (interactive)
  (when (get-buffer worksheet-buffer-name)
    (with-current-buffer worksheet-buffer-name
      (beginning-of-buffer)
      (condition-case exception
          (while t (call-interactively 'ein:worksheet-kill-cell))
        ('error)))))

(defun pynt-kill-all-cells ()
  "Clear cells in every namespace worksheet.

This function mainly exists to clear out each namespace worksheet
in the beginning to start them each with a blank slate."
  (interactive)
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
         (worksheet-names (seq-filter (lambda (buffer-name) (string-prefix-p "ns=" buffer-name)) buffer-names)))
    (dolist (worksheet-name worksheet-names)
      (pynt-kill-cells worksheet-name))))

(defun pynt-get-buffer-names-in-frame ()
  "Get the buffer names in the active frame.

This function is used so we can pull out the worksheet name (i.e. name of the active function) to pass to the AST server"
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (buffer-names (mapcar 'buffer-name buffers)))
    buffer-names))

(defun pynt-get-active-namespace-buffer-name ()
  "Return the name of the active namespace.

The active namespace will have a buffer in the active frame and
will have the prefix 'ns='.  If there is no such window then
produce an error."
  (let* ((buffer-names (pynt-get-buffer-names-in-frame))
         (active-buffer-singleton (seq-filter
                                   (lambda (buffer-name) (string-prefix-p "ns=" buffer-name))
                                   buffer-names))
         (active-buffer-name (car active-buffer-singleton)))
    (if (not active-buffer-name)
        (error "No window in the current frame whose buffer name is prefixed with 'ns='!")
      active-buffer-name)))

(defun pynt-set-active-namespace (ns-buffer-name)
  "Parse through the active frame and pick out the active buffer.

Set `pynt-active-namespace-buffer-name' and
`pynt-active-namespace' accordingly.  `NS-BUFFER-NAME' is a string."
  (setq pynt-active-namespace-buffer-name ns-buffer-name)
  (let ((namespace-singleton (split-string pynt-active-namespace-buffer-name "ns=")))
    (setq pynt-active-namespace (cadr namespace-singleton))))

(defun pynt-make-namespace-worksheets ()
  "Parse through the code and create the namespace worksheets.

This function should be called before evaluating any namespaces."
  (interactive)
  (setq pynt-namespaces nil)
  (setq pynt-notebook-buffer-names nil)
  (setq pynt-namespace-to-region-map (make-hash-table :test 'equal))
  (pynt-set-active-namespace (format "ns=*%s*" (pynt-get-module-level-namespace)))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (pynt-log "Calling parse_namespaces with pynt-activae-namespace = %s" pynt-active-namespace)
      (epc:call-deferred pynt-ast-server 'parse_namespaces `(,code ,pynt-active-namespace))
      (deferred:nextc it
        (lambda (namespaces)
          (pynt-log "Namespaces = %s" namespaces)
          (dolist (namespace namespaces)
            (multiple-value-bind (name start-line end-line) namespace
              (progn
                (pynt-create-new-worksheet name)
                (pynt-kill-cells name)
                (setq pynt-namespaces (append pynt-namespaces (list name)))
                (puthash name (list (buffer-name) start-line end-line) pynt-namespace-to-region-map)))))))))

(defun pynt-execute-current-namespace ()
  "Generate a worksheet determined by the active namespace.

Call out to the AST server to annotate the current namespace with
calls to make cells. Then send this code to be evaluated to the
activate notebook defined by the variable
`pynt-notebook-buffer-name'."
  (interactive)
  (widen)
  (pynt-set-active-namespace (pynt-get-active-namespace-buffer-name))
  (pynt-kill-cells pynt-active-namespace-buffer-name)
  (setq pynt-line-to-cell-map (make-hash-table :test 'equal))
  (let ((code (buffer-substring-no-properties (point-min) (point-max))))
    (deferred:$
      (pynt-log "Calling python AST server with active namespace = %s ..." pynt-active-namespace)
      (epc:call-deferred pynt-ast-server 'annotate `(,code ,pynt-active-namespace))
      (deferred:nextc it
        (lambda (annotated-code)
          (pynt-log "Annotated code = %S" annotated-code)
          (ein:shared-output-eval-string annotated-code))))))

(defun pynt-scroll-cell-window ()
  "Scroll the worksheet buffer.

Do it so the cell which corresponds to the line of code the point
is on goes to the top.  Make sure the cell we're about to jump to
is is indeed the active buffer.

Go off of the variable `pynt-nth-cell-instance' in the case where
we want to see the nth pass though, say, a for loop.

Wrap the main logic in a condition case because it could be the
case that the cell that did correspond to a line has since been
deleted.  Basically there is a bunch of data invalidation that I
don't want to worry about at this point in time."
  (interactive)
  (when (not (string-prefix-p "ns=" (buffer-name)))
    (save-selected-window
      (let ((cells (gethash (line-number-at-pos) pynt-line-to-cell-map)))
        (when cells
          (condition-case exception
              (let* ((cell (nth pynt-nth-cell-instance cells))
                     (cell-marker (ein:cell-location cell :input))
                     (point-line (count-screen-lines (window-start) (point)))
                     (window (get-buffer-window pynt-active-namespace-buffer-name)))
                (when (and cell-marker (string= (buffer-name (marker-buffer cell-marker)) pynt-active-namespace-buffer-name))
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
  "Scroll the cell buffer to the next occurrence of the current code line.

This only happens in the body of for and while loops where some
lines of code and executed many times."
  (interactive)
  (setq pynt-nth-cell-instance (1- pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-next-cell-instance ()
  "Scroll the cell buffer to the previous occurrence of the current code line.

This only happens in the body of for and while loops where some
lines of code and executed many times."
  (interactive)
  (setq pynt-nth-cell-instance (1+ pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-new-notebook ()
  "Create a new EIN notebook and bring it up side-by-side."
  (interactive)
  (save-selected-window
    (let* ((path (buffer-file-name))
           (dir-path (substring (file-name-directory path) 0 -1))
           (home-dir (concat (expand-file-name "~") "/"))
           (nb-dir (replace-regexp-in-string home-dir "" dir-path))
           (url-or-port (car (ein:jupyter-server-conn-info)))
           (notebook-list-buffer-name (concat "*ein:notebooklist " url-or-port "*")))
      (with-current-buffer notebook-list-buffer-name
        (ein:notebooklist-new-notebook url-or-port nil nb-dir)))
    (sit-for 1)))

(defun pynt-create-new-worksheet (buffer-name)
  "Create a new worksheet in the `pynt-module-level-namespace' notebook.

Argument BUFFER-NAME the name of the buffer to create a new worksheet in."
  (interactive "MFunction name: ")
  (setq pynt-notebook-buffer-names (append pynt-notebook-buffer-names (list buffer-name)))
  (save-excursion
    (save-window-excursion
      (with-current-buffer pynt-notebook-buffer-name
        (call-interactively 'ein:notebook-worksheet-insert-next)
        (rename-buffer buffer-name)))))

(defun pynt-start-elisp-relay-server ()
  "Start the EPCS server and register its associated callback.

The EPCS server's job is to relay commands to create an execute
EIN cells from the python EPC client."
  (let ((connect-function
         (lambda (mngr)
           (let ((mngr mngr))
             (epc:define-method
              mngr 'make-cell
              (lambda (&rest args)
                (multiple-value-bind (expr buffer-name cell-type line-number) args
                  (pynt-make-cell expr buffer-name cell-type (string-to-number line-number))
                  nil)))))))
    (setq pynt-elisp-relay-server (epcs:server-start connect-function pynt-epc-port))))

(defun pynt-stop-elisp-relay-server ()
  "Terminate the elisp EPC relay server.

This is called when pynt mode is deactivated.'"
  (epcs:server-stop pynt-elisp-relay-server)
  (setq pynt-elisp-relay-server nil))

(defun pynt-make-cell (expr buffer-name cell-type line-number)
  "Make a new EIN cell and possibly evaluate its contents.

Insert a new code cell with contents `EXPR' into the worksheet
buffer `BUFFER-NAME' with cell type `CELL-TYPE' at the end of the
worksheet and evaluate it.

This function is called from python code running in a jupyter
kernel via RPC.

`LINE-NUMBER' is the line number in the code that the cell
corresponds and is used during pynt scroll mode."
  (pynt-log "(pytn-make-cell %S %S %S %S)..." expr buffer-name cell-type line-number)

  ;; These variables are buffer local so we need to grab them before switching
  ;; over to the worksheet buffer.
  (with-current-buffer buffer-name
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (let ((cell (ein:get-cell-at-point))
          (ws (ein:worksheet--get-ws-or-error)))
      (cond ((string= cell-type "code") (call-interactively 'ein:worksheet-execute-cell))
            ((string= cell-type "markdown") (ein:worksheet-change-cell-type ws cell "markdown"))
            (t (ein:worksheet-change-cell-type ws cell "heading" (string-to-number cell-type))))
      (when (and (not (eq line-number -1)) pynt-line-to-cell-map) ; not sure why maps would be nil but it happens ¯\_(ツ)_/¯
        (let ((previous-cells (gethash line-number pynt-line-to-cell-map)))
          (puthash line-number (append previous-cells (list cell)) pynt-line-to-cell-map))))))

(defun pynt-start-ast-server ()
  "Start python AST server."
  (let* ((dirname (file-name-directory (symbol-file 'pynt-log)))
         (ast-server-path (concat dirname "ast-server.py")))
    (setq pynt-ast-server (epc:start-epc "python" `(,ast-server-path)))))

(defun pynt-stop-ast-server ()
  "Terminate the python AST server.

This happens when pynt mode is exited."
  (epc:stop-epc pynt-ast-server)
  (setq pynt-ast-server nil))

(defun pynt-start-py-epc-client ()
  "Initialize the EPC client for the active kernel.

This needs to be done so python can send commands to Emacs to
create code cells.  Use whatever the value is for
`pynt-elisp-relay-server-hostname' and `pynt-epc-port' to define the
communication channels for the EPC client."
  (let ((pynt-init-code (format pynt-init-code-template pynt-elisp-relay-server-hostname pynt-epc-port)))
    (ein:shared-output-eval-string pynt-init-code))
  (setq pynt-epc-port (1+ pynt-epc-port)))

(defun pynt-intercept-ein-notebook-name (old-function buffer-or-name)
  "Advice to add around `ein:connect-to-notebook-buffer'.

So pynt mode can grab the buffer name of the main worksheet.

Argument OLD-FUNCTION the function we are wrapping.
Argument BUFFER-OR-NAME the name of the notebook we are connecting to."
  (pynt-log "Setting main worksheet name = %S" buffer-or-name)
  (setq pynt-notebook-buffer-name buffer-or-name)
  (apply old-function (list buffer-or-name)))

(defun pynt-init-servers ()
  "Start AST and elisp relay server along with python EPC client."
  (pynt-start-ast-server)
  (pynt-start-elisp-relay-server)
  (pynt-start-py-epc-client))

(defun pynt-narrow-code (namespace)
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

(defvar pynt-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-e") 'pynt-execute-current-namespace)
    (define-key map (kbd "C-c C-w") 'pynt-make-namespace-worksheets)
    (define-key map (kbd "C-c C-s") 'pynt-select-namespace)
    map))

(define-minor-mode pynt-mode
  "Minor mode for generating and interacting with jupyter notebooks via EIN

\\{pynt-mode-map}"
  :lighter " pynt "
  :keymap pynt-mode-map
  (if pynt-mode
      (progn
        (if (not (intersection (ein:notebook-opened-buffer-names) (pynt-get-buffer-names-in-frame)))
            (pynt-new-notebook)
          (advice-add #'ein:connect-to-notebook-buffer :around #'pynt-intercept-ein-notebook-name)
          (call-interactively 'ein:connect-to-notebook-buffer))
        (let* ((notebook-buffer-name-singleton (intersection (ein:notebook-opened-buffer-names) (pynt-get-buffer-names-in-frame)))
               (notebook-buffer-name (car notebook-buffer-name-singleton)))
          (setq pynt-buffer-name (buffer-name)
                pynt-notebook-buffer-name notebook-buffer-name
                pynt-notebook-to-buffer-map (make-hash-table :test 'equal)
                pynt-module-level-namespace (pynt-get-module-level-namespace))
          (puthash pynt-notebook-buffer-name pynt-buffer-name pynt-notebook-to-buffer-map)
          (ein:connect-to-notebook-buffer pynt-notebook-buffer-name)
          (sit-for 2)
          (pynt-init-servers)
          (let ((current-prefix-arg 4)) (call-interactively 'ein:connect-run-or-eval-buffer))
          (pynt-make-namespace-worksheets)))
    (advice-remove #'ein:connect-to-notebook-buffer #'pynt-intercept-ein-notebook-name)
    (pynt-stop-elisp-relay-server)
    (pynt-stop-ast-server)))

(defvar pynt-scroll-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<up>") 'pynt-next-cell-instance)
    (define-key map (kbd "<down>") 'pynt-prev-cell-instance)
    map))

(define-minor-mode pynt-scroll-mode
  "Minor mode for scrolling a EIN notebook side-by-side with code.

\\{pynt-scroll-mode-map}"
  :lighter " pynt-scroll "
  :keymap pynt-scroll-mode-map
  (if pynt-scroll-mode
      (progn
        (add-hook 'post-command-hook #'pynt-scroll-cell-window :local))
    (remove-hook 'post-command-hook #'pynt-scroll-cell-window))
  (setq pynt-nth-cell-instance 0))

;;; Start a jupyter notebook server in the user's home directory
(when pynt-start-jupyter-server-on-startup
  (deferred:$
    (deferred:next
      (lambda ()
        (message "Starting jupyter notebook server...")
        (let ((server-cmd-path (executable-find "jupyter"))
              (notebook-directory (expand-file-name "~")))
          (ein:jupyter-server--run ein:jupyter-server-buffer-name server-cmd-path notebook-directory))
        (deferred:wait 6000)))
    (deferred:nextc it
      (lambda ()
        (ein:force-ipython-version-check)
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (pynt-log "url-or-port = % and token = %s" url-or-port token)
          (ein:notebooklist-login url-or-port token))
        (deferred:wait 1000)))
    (deferred:nextc it
      (lambda ()
        (multiple-value-bind (url-or-port token) (ein:jupyter-server-conn-info)
          (ein:notebooklist-open url-or-port "" t))))))

;;; Narrowing from the EIN worksheet
(advice-add 'ein:notebook-worksheet-open-next-or-first :after 'pynt-narrow-code)
(advice-add 'ein:notebook-worksheet-open-prev-or-last :after 'pynt-narrow-code)

(provide 'pynt)
;;; pynt.el ends here
