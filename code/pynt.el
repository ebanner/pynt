;;; pynt.el --- Generate and interact with jupyter notebooks with EIN

;; Copyright (C) 2018 Free Software Foundation, Inc.

;; Author: Edward Banner <edward.banner@gmail.com>
;; Version: 0.1
;; Package-Requires: ((flange "1.0"))
;; Keywords: jupyter, ein, interactive, python
;; URL: https://github.com/ebanner/pynt

;;; Commentary:

;; This package provides a minor mode to generate and interact with jupyter
;; notebooks via EIN from a Python script.

(require 'cl)
(require 'epcs)
(require 'epc)

;;; EIN-specific variables to set
(setq print-level 2)
(setq print-length 10)
(setq print-circle t)

;;; Global variables
(setq pynt-verbose t)
(setq pynt-active-buffer-name "context=foo")
(setq pynt-active-defun-name "foo")
(setq pynt-main-worksheet-name "Untitled.ipynb")
(setq pynt-line-number-to-cell-location-map (make-hash-table :test 'equal))
(setq pynt-elisp-relay-server nil)
(setq pynt-ast-server nil)

;;; Helper functions
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
  "Clear cells in every context worksheet.

This function mainly exists to clear out each context worksheet
in the beginning to start them each with a blank slate."
  (interactive)
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
        (worksheet-names (seq-filter (lambda (buffer-name) (string-prefix-p "context=" buffer-name)) buffer-names)))
    (dolist (worksheet-name worksheet-names)
      (pynt-kill-cells worksheet-name))))

(defun pynt-get-buffer-names ()
  "Get the buffer names in the active frame

This function is used so we can pull out the worksheet name (i.e. name of the active function) to pass to the AST server"
  (interactive)
  (let* ((windows (window-list))
         (buffers (mapcar 'window-buffer windows))
         (buffer-names (mapcar 'buffer-name buffers)))
    buffer-names))

(defun pynt-get-active-context-buffer-name ()
  "Return the name of the active context.

The active context will have a buffer in the active frame and
will have the prefix 'context='."
  (let* ((buffer-names (pynt-get-buffer-names))
         (active-buffer-singleton (seq-filter
                                   (lambda (buffer-name) (string-prefix-p "context=" buffer-name))
                                   buffer-names))
         (active-buffer-name (car active-buffer-singleton)))
    (if (not active-buffer-name)
        "N/A"
      active-buffer-name)))

(defun pynt-set-active-context ()
  "Parse through the active frame and pick out the active buffer

Set `pynt-active-buffer-name' and `pynt-active-defun-name' accordingly."
  (setq pynt-active-buffer-name (pynt-get-active-context-buffer-name))
  (setq pynt-active-defun-name
        (if (not (string= pynt-active-buffer-name "N/A"))
            (cadr (split-string pynt-active-buffer-name "context="))
          "N/A")))

(defun pynt-generate-worksheet ()
  "Generate a worksheet determined by the active context

This is the main function which kicks off much of the work."
  (interactive)
  (let ((code (pynt-get-buffer-string)))
    (pynt-set-active-context)
    (pynt-log "Doing code = %S" code)
    (if (string= pynt-active-buffer-name "N/A")
        (pynt-kill-all-cells)
      (pynt-kill-cells pynt-active-buffer-name))
    (pynt-annotate-make-cells-eval code)))

(defun pynt-move-cell-window ()
  (interactive)
  (save-selected-window
    (let ((window (get-buffer-window pynt-active-buffer-name))
          (cell-location (gethash (line-number-at-pos) pynt-line-number-to-cell-location-map)))
      (when cell-location
        (select-window window)
        (goto-char cell-location)
        (recenter 5)))))

(defun pynt-create-new-worksheet (buffer-name)
  "Create a new worksheet in a notebook who has a buffer called *epc-client*"
  (interactive "MFunction name: ")
  (save-excursion
    (save-window-excursion
      (with-current-buffer pynt-main-worksheet-name
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
    (setq pynt-elisp-relay-server (epcs:server-start connect-function 9999))))

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
  (pynt-log "Inserting: %S" expr)
  (pynt-log "line-number = %S" line-number)

  (when (not (get-buffer buffer-name))
    (pynt-create-new-worksheet buffer-name)
    (pynt-kill-cells buffer-name))

  (with-current-buffer buffer-name
    (end-of-buffer)
    (call-interactively 'ein:worksheet-insert-cell-below)
    (insert expr)
    (let ((cell (ein:get-cell-at-point))
          (ws (ein:worksheet--get-ws-or-error)))
      (if (string= cell-type "code")
          (progn
            (call-interactively 'ein:worksheet-execute-cell)
            (when (not (eq line-number -1))
              (puthash line-number (ein:cell-location cell) pynt-line-number-to-cell-location-map)))
        (if (string= cell-type "markdown")
            (ein:worksheet-change-cell-type ws cell "markdown")
          (ein:worksheet-change-cell-type ws cell "heading" (string-to-int cell-type)))))))

(defun pynt-start-ast-server ()
  "Start python AST server"
  (setq pynt-ast-server (epc:start-epc "python" '("/Users/ebanner/.dotfiles/elisp/ast-server.py"))))

(defun pynt-annotate-make-cells-eval (code)
  "This server receives code and annotates it with code to call out to the elisp server."
  (deferred:$
    (pynt-log "Calling python AST server...")
    (pynt-log "with code = %S" code)
    (pynt-log "and active defun = %S" pynt-active-defun-name)
    (epc:call-deferred pynt-ast-server 'annotate `(,code ,pynt-active-defun-name))
    (deferred:nextc it
      (lambda (annotated-code)
        (pynt-log "Annotated code: %S" annotated-code)
        (ein:shared-output-eval-string annotated-code)))
    (deferred:error it
      (lambda (err)
        (cond
         ((stringp err) (pynt-log "Error is %S" err))
         ((eq 'epc-error (car err)) (pynt-log "Error is %S" (cadr err))))))))

(defun pynt-stop-ast-server ()
  (epc:stop-epc pynt-ast-server)
  (setq pynt-ast-server nil))

(defun pynt-grab-ein-buffer-name (old-function buffer-or-name)
  "Advice to be added around `ein:connect-to-notebook-buffer'

So pynt-mode can grab the buffer name of the main worksheet."
  (pynt-log "Setting main worksheet name = %S" buffer-or-name)
  (setq pynt-main-worksheet-name buffer-or-name)
  (apply old-function (list buffer-or-name)))

(setq pynt-init-code "

from epc.client import EPCClient
import time

def __cell__(content, buffer_name, cell_type, line_number):
    elisp_func = 'make-code-cell-and-eval'
    epc_client.call_sync(elisp_func, args=[content, buffer_name, cell_type, line_number])
    time.sleep(0.01)

epc_client = EPCClient(('docker.for.mac.localhost', 9999), log_traceback=True)
__name__ = '__pynt__'

")

(defun pynt-start-py-epc-client ()
  "Initialize the EPC client for the active kernel.

This needs to be done so python can send commands to emacs to create code cells."
  (ein:shared-output-eval-string pynt-init-code))

(define-minor-mode pynt-mode
  "Toggle pynt-mode

Minor mode for generating and interacting with jupyter notebooks via EIN"
  :lighter " pynt"
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c C-c") 'pynt-generate-worksheet)
            map)
  (if pynt-mode
      (progn
        (advice-add #'ein:connect-to-notebook-buffer :around #'pynt-grab-ein-buffer-name)
        (add-hook 'post-command-hook #'pynt-move-cell-window :local)
        (call-interactively 'ein:connect-to-notebook-buffer)
        (pynt-start-ast-server)
        (pynt-start-elisp-relay-server)
        (pynt-start-py-epc-client)
        (let ((current-prefix-arg 4)) (call-interactively 'ein:connect-run-or-eval-buffer)))
    (advice-remove #'ein:connect-to-notebook-buffer #'pynt-grab-ein-buffer-name)
    (remove-hook 'post-command-hook #'pynt-move-cell-window :local)
    (pynt-stop-elisp-relay-server)
    (pynt-stop-ast-server)))

(provide 'pynt)
