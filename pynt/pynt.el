;;; pynt.el --- Interact with jupyter notebooks

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Edward Banner <edward.banner@gmail.com>
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Keywords: convenience
;; URL: https://github.com/ebanner/pynt

;;; Commentary:

;; pynt provides a minor mode `pynt-mode' to generate and interact with jupyter
;; notebooks via EIN from code in a python buffer.

;;; Dependencies
(require 'cl)
(require 'epcs)
(require 'epc)

(defgroup pynt nil
  "Customization group for pynt."
  :group 'applications)

;;; Variables
(defcustom pynt-elisp-relay-server-hostname "localhost"
  "The hostname of the elisp relay server. Usually `localhost'
  but if the jupyter kernel is running inside a docker container
  then this value should be `docker.for.mac.localhost' when on a
  mac."
  :options '("localhost" "docker.for.mac.localhost"))

(defcustom pynt-scroll-narrow-view t
  "Narrow the notebook buffer if t and don't otherwise"
  :options '(nil t))

(defvar pynt-epc-port 9999
  "The port that the current EPC client and server are communicating on.

Every invocation of `pynt-mode' increments this number so there
can be multiple EPC client-server pairs.")

(defvar pynt-init-code-template
  "

%%matplotlib inline

from epc.client import EPCClient
import time

epc_client = EPCClient(('%s', %s), log_traceback=True)

def __cell__(content, buffer_name, cell_type, line_number):
    elisp_func = 'make-code-cell-and-eval'
    epc_client.call_sync(elisp_func, args=[content, buffer_name, cell_type, line_number])
    time.sleep(0.01)

__name__ = '__pynt__'

"
  "Template for the code that gets evaluated when you call `pynt-mode'

The value of `pynt-elisp-relay-server-hostname' and `pynt-epc-port'
are needed to complete this template.")

(defvar pynt-verbose nil
  "Logging flag.

Log `pynt-mode' debugging information if `t' and do not otherwise.")

(defvar-local pynt-module-level-namespace ""
  "The module-level namespace of the buffer.

This is computed from the buffer name *once* when you activate
`pynt-mode'. It should never change after that.")

(defvar-local pynt-active-namespace ""
  "The active function name.

This is the function who will have code cells made from
it/annotated/rolled out/whatever you want to call it. Needs to be
the name of a python function in the current buffer or the value
'outside' to indicate the code outside of any function.")

(defvar-local pynt-active-namespace-buffer-name (format "ns=%s" pynt-active-namespace)
  "The buffer name of the active namespace.

When you run `pynt-execute-namespace' this is the buffer that
will have code cells added to it and evaluated. Is always of the
form 'ns=`pynt-active-namespace''")

(defvar-local pynt-notebook-buffer-name ""
  "Buffer name of the EIN notebook

This variable holds the name of the notebook associated with the
current `pynt-mode' session. The value gets set when you can
`pynt-mode' and choose a EIN notebook name.'")

(defvar pynt-line-to-cell-map nil
  "Table mapping of source code line to EIN cell(s)

A source code line may be associated with many EIN cells (e.g. a
line in the body of a for loop.")

(defvar-local pynt-elisp-relay-server nil "Elisp relay server")
(defvar-local pynt-ast-server nil "Python AST server")

;;; Set these variables to accomodate EIN recursive data structures
(setq-local print-level 0)
(setq-local print-length 1)
(setq-local print-circle t)

(defun pynt-get-module-level-namespace ()
  "Extract the module-level name of the pynt text buffer

If the buffer is associated with a python file then chop off the
'.py' suffix. Otherwise (e.g. if this is a *scratch* buffer just
retrun the buffer name. Throw an error if the buffer name has a
period in it because that will mess with the naming of namespaces
that `pynt-mode' uses."
  (if (string-suffix-p ".py" (buffer-name))
      (let ((namespace-tokens (nbutlast (split-string (buffer-name) "\\.py") 1)))
        (if (or (> (length namespace-tokens) 1)
                (string-match-p (regexp-quote "=") (car namespace-tokens)))
            (error "Buffer name cannot contain '.' nor '='. Rename your buffer and try again!")
          (car namespace-tokens)))
    (buffer-name)))

(defun pynt-get-buffer-string ()
  "Get the entire buffer as a string"
  (save-excursion
    (end-of-buffer)
    (buffer-substring-no-properties 1 (point))))

(defun pynt-log (&rest args)
  "Log the message when the variable `pynt-verbose' is `t'"
  (when pynt-verbose
    (apply #'message args)))

(defun pynt-kill-cells (worksheet-buffer-name)
  "Pop over to the worksheet buffer and delete all the cells.

Do nothing if the buffer does not exist."
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

(defun pynt-get-namespace-buffer-names ()
  "Get the buffer names in the active frame

This function is used so we can pull out the worksheet name (i.e. name of the active function) to pass to the AST server"
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (buffer-names (mapcar 'buffer-name buffers)))
    buffer-names))

(defun pynt-get-active-ns-buffer-name ()
  "Return the name of the active namespace.

The active namespace will have a buffer in the active frame and
will have the prefix 'ns='. If there is no such window then
produce an error."
  (let* ((buffer-names (pynt-get-namespace-buffer-names))
         (active-buffer-singleton (seq-filter
                                   (lambda (buffer-name) (string-prefix-p "ns=" buffer-name))
                                   buffer-names))
         (active-buffer-name (car active-buffer-singleton)))
    (if (not active-buffer-name)
        (error "No window in the current frame whose buffer name is prefixed with 'ns='!")
      active-buffer-name)))

(defun pynt-set-active-ns (ns-buffer-name)
  "Parse through the active frame and pick out the active buffer

Set `pynt-active-namespace-buffer-name' and
`pynt-active-namespace' accordingly. `ns-buffer-name' is a string."
  (setq pynt-active-namespace-buffer-name ns-buffer-name)
  (let ((namespace-singleton (split-string pynt-active-namespace-buffer-name "ns=")))
    (setq pynt-active-namespace (cadr namespace-singleton))))

(defun pynt-generate-namespace-worksheets ()
  "Parse through the code and create the namespace worksheets.

This function should be called before evaluating any namespaces."
  (interactive)
  (pynt-set-active-ns (format "ns=*%s*" (pynt-get-module-level-namespace)))
  (let ((code (pynt-get-buffer-string)))
    (pynt-annotate-make-cells-eval code)))

(defun pynt-execute-namespace ()
  "Generate a worksheet determined by the active namespace

This is the main function which kicks off much of the work."
  (interactive)
  (pynt-set-active-ns (pynt-get-active-ns-buffer-name))
  (pynt-kill-cells pynt-active-namespace-buffer-name)
  (setq pynt-line-to-cell-map (make-hash-table :test 'equal))
  (let ((code (pynt-get-buffer-string)))
    (pynt-annotate-make-cells-eval code)))

(defun pynt-scroll-cell-window ()
  "Scroll the worksheet buffer

Do it so the cell which corresponds to the line of code the point
is on goes to the top. Make sure the cell we're about to jump to
is is indeed the active buffer.

Go off of the variable `pynt-nth-cell-instance' in the case where
we want to see the nth pass though, say, a for loop.

Wrap the main logic in a condition case because it could be the
case that the cell that did correspond to a line has since been
deleted. Basically there is a bunch of data invalidation that I
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
  (interactive)
  (setq pynt-nth-cell-instance (1- pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-next-cell-instance ()
  (interactive)
  (setq pynt-nth-cell-instance (1+ pynt-nth-cell-instance))
  (pynt-scroll-cell-window)
  (message "iteration # = %s" pynt-nth-cell-instance))

(defun pynt-create-new-worksheet (buffer-name)
  "Create a new worksheet in a notebook who has a buffer called *epc-client*"
  (interactive "MFunction name: ")
  (save-excursion
    (save-window-excursion
      (with-current-buffer pynt-notebook-buffer-name
        (call-interactively 'ein:notebook-worksheet-insert-next)
        (rename-buffer (concat buffer-name))))))

(defun pynt-start-elisp-relay-server ()
  "Start the EPCS server and register its associated callback.

The EPCS server's job is to relay commands to create an execute
EIN cells from the python EPC client."
  (let ((connect-function
         (lambda (mngr)
           (lexical-let ((mngr mngr))
             (epc:define-method
              mngr 'make-code-cell-and-eval
              (lambda (&rest args)
                (let ((expr (car args))
                      (buffer-name (cadr args))
                      (cell-type (nth 2 args))
                      (line-number (string-to-int (nth 3 args))))
                  (pynt-log "(make-code-cell-and-eval %S %S %S %S)..." expr buffer-name cell-type line-number)
                  (pynt-make-code-cell-and-eval expr buffer-name cell-type line-number)
                  nil)))))))
    (setq pynt-elisp-relay-server (epcs:server-start connect-function pynt-epc-port))))

(defun pynt-stop-elisp-relay-server ()
  (epcs:server-stop pynt-elisp-relay-server)
  (setq pynt-elisp-relay-server nil))

(defun pynt-make-code-cell-and-eval (expr buffer-name cell-type line-number)
  "EPC callback

Pop over to the worksheet buffer `buffer-name' and insert a new
cell or type `cell-type' containing `expr' at the bottom and
evaluate it.

This function is called from python code running in a jupyter
kernel via RPC.

`line-number' is the line number in the code that the cell
corresponds to and is saved in the map."
  (pynt-log "Inserting %S" expr)
  (pynt-log "line-number = %S" line-number)

  (when (not (get-buffer buffer-name))
    (pynt-create-new-worksheet buffer-name)
    (pynt-kill-cells buffer-name))

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
            (t (ein:worksheet-change-cell-type ws cell "heading" (string-to-int cell-type))))
      (when (and (not (eq line-number -1)) pynt-line-to-cell-map) ; not sure why maps would be nil but it happens ¯\_(ツ)_/¯
        (let ((previous-cells (gethash line-number pynt-line-to-cell-map)))
          (puthash line-number (append previous-cells (list cell)) pynt-line-to-cell-map))))))

(defun pynt-start-ast-server ()
  "Start python AST server"
  (let* ((dirname (file-name-directory (symbol-file 'pynt-log)))
         (ast-server-path (concat dirname "ast-server.py")))
    (setq pynt-ast-server (epc:start-epc "python" `(,ast-server-path)))))

(defun pynt-annotate-make-cells-eval (code)
  "This server receives code and annotates it with code to call out to the elisp server."
  (deferred:$
    (pynt-log "Calling python AST server with active namespace = %s ..." pynt-active-namespace)
    (epc:call-deferred pynt-ast-server 'annotate `(,code ,pynt-active-namespace))
    (deferred:nextc it
      (lambda (annotated-code)
        (pynt-log "Annotated code = %S" annotated-code)
        (ein:shared-output-eval-string annotated-code)))
    (deferred:error it
      (lambda (err)
        (cond
         ((stringp err) (pynt-log "Error is %S" err))
         ((eq 'epc-error (car err)) (pynt-log "Error is %S" (cadr err))))))))

(defun pynt-stop-ast-server ()
  (epc:stop-epc pynt-ast-server)
  (setq pynt-ast-server nil))

(defun pynt-start-py-epc-client ()
  "Initialize the EPC client for the active kernel.

This needs to be done so python can send commands to emacs to
create code cells. Use whatever the value is for
`pynt-elisp-relay-server-hostname' and `pynt-epc-port' to define the
communication channels for the EPC client."
  (let ((pynt-init-code (format pynt-init-code-template pynt-elisp-relay-server-hostname pynt-epc-port)))
    (ein:shared-output-eval-string pynt-init-code))
  (setq pynt-epc-port (1+ pynt-epc-port)))

(defun pynt-intercept-ein-notebook-name (old-function buffer-or-name)
  "Advice to be added around `ein:connect-to-notebook-buffer'

So pynt-mode can grab the buffer name of the main worksheet."
  (pynt-log "Setting main worksheet name = %S" buffer-or-name)
  (setq pynt-notebook-buffer-name buffer-or-name)
  (apply old-function (list buffer-or-name)))

(defun pynt-init-servers ()
  "Start AST and elisp relay server along with python EPC client"
  (pynt-start-ast-server)
  (pynt-start-elisp-relay-server)
  (pynt-start-py-epc-client))

(define-minor-mode pynt-mode
  "Toggle pynt-mode

Minor mode for generating and interacting with jupyter notebooks via EIN"
  :lighter " pynt "
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-e") 'pynt-execute-namespace)
            (define-key map (kbd "C-c C-w") 'pynt-generate-namespace-worksheets)
            map)
  (if pynt-mode
      (progn
        (advice-add #'ein:connect-to-notebook-buffer :around #'pynt-intercept-ein-notebook-name)
        (call-interactively 'ein:connect-to-notebook-buffer)
        (pynt-init-servers)
        (let ((current-prefix-arg 4)) (call-interactively 'ein:connect-run-or-eval-buffer))
        (setq pynt-module-level-namespace (pynt-get-module-level-namespace))
        (setq pynt-buffer-name (buffer-name))
        (pynt-generate-namespace-worksheets))
    (advice-remove #'ein:connect-to-notebook-buffer #'pynt-intercept-ein-notebook-name)
    (pynt-stop-elisp-relay-server)
    (pynt-stop-ast-server)))

(define-minor-mode pynt-scroll-mode
  "Toggle pynt-scroll-mode

Minor mode for scrolling an open EIN notebook."
  :lighter " pynt-scroll "
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-n") 'pynt-next-cell-instance)
            (define-key map (kbd "C-c C-p") 'pynt-prev-cell-instance)
            map)
  (if pynt-scroll-mode
      (progn
        (add-hook 'post-command-hook #'pynt-scroll-cell-window :local))
    (remove-hook 'post-command-hook #'pynt-scroll-cell-window))
  (setq pynt-nth-cell-instance 0))

(provide 'pynt)

;;; pynt.el ends here
