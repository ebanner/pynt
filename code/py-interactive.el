(defun my/buffer-string ()
  "Get the entire buffer as a string"
  (save-excursion
    (end-of-buffer)
    (buffer-substring-no-properties 1 (point))))

;;; global variables
(setq *next-line-number-to-eval* 1)
(setq *my/debug* t)
(setq *active-buffer-name* "context=foo")
(setq *active-defun-name* "foo")
(setq *line-number-to-cell-map* (make-hash-table :test 'equal))

;;; Prevent emacs from printing out recursive data structures
(setq print-level 2)
(setq print-length 10)
(setq print-circle t)

(defun my/message (&rest args)
  (when *my/debug*
    (apply #'message args)))

(defun my/clear-cells (buffer-name)
  "Pop over to the *cells* buffer and delete all the cells"
  (interactive)
  (when (get-buffer buffer-name)
    (with-current-buffer buffer-name
      (beginning-of-buffer)
      (condition-case exception
          (while t (call-interactively 'ein:worksheet-kill-cell))
        ('error)))))

(defun my/clear-worksheets ()
  "Clear the cells in all the worksheets which correspond to regions"
  (interactive)
  (let* ((buffer-names (mapcar 'buffer-name (buffer-list)))
        (worksheet-names (seq-filter (lambda (buffer-name) (string-prefix-p "context=" buffer-name)) buffer-names)))
    (dolist (worksheet-name worksheet-names)
      (my/clear-cells worksheet-name))))

;;; elisp server
(require 'cl)
(require 'epcs)
(defun my/start-epcs ()
  (interactive)
  "Start elisp RPC server"
  (defun my/make-code-cell-and-eval (expr buffer-name cell-type line-number)
    "Pop over to *cells* buffer and insert a new cell containing `expr' at the bottom and evaluate it
This function is called from python code running in a jupyter kernel via RPC.
`buffer-name' is the name of the buffer to insert the code cell which is of the form *func-name*.
`cell-type' is either 'code' or 'markdown' or '1'."
    (interactive "MExpression: ")
    (my/message "Inserting: %S" expr)
    (my/message "line-number = %S" line-number)

    (when (not (get-buffer buffer-name))
      (my/create-new-worksheet buffer-name)
      (my/clear-cells buffer-name))

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
                (puthash line-number (ein:cell-location cell) *line-number-to-cell-map*)))
          (if (string= cell-type "markdown")
              (ein:worksheet-change-cell-type ws cell "markdown")
            (ein:worksheet-change-cell-type ws cell "heading" (string-to-int cell-type)))))))

  ;; elisp server callback
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
                  (my/message "(make-code-cell-and-eval %S %S %S %S)..." expr buffer-name cell-type line-number)
                  (my/make-code-cell-and-eval expr buffer-name cell-type line-number)
                  nil)))))))
    (setq server-process (epcs:server-start connect-function 9999))))
(defun my/stop-epcs ()
  "Bring down the EPC server"
  (epcs:server-stop server-process))
(defun my/restart-epcs ()
  "Bring down the EPC server"
  (interactive)
  (my/stop-epcs)
  (my/start-epcs))

;;; python server
(require 'epc)

(defun my/annotate-make-cells-eval (code)
  "This server receives code and annotates it with code to call out to the elisp server."
  (deferred:$
    (my/message "Calling python AST server...")
    (my/message "with code = %S" code)
    (my/message "and active defun = %S" *active-defun-name*)
    (epc:call-deferred py-epc 'annotate `(,code ,*active-defun-name*))
    (deferred:nextc it
      (lambda (annotated-code)
        (my/message "Annotated code: %S" annotated-code)
        (ein:shared-output-eval-string annotated-code)))
    (deferred:error it
      (lambda (err)
        (cond
         ((stringp err) (my/message "Error is %S" err))
         ((eq 'epc-error (car err)) (my/message "Error is %S" (cadr err))))))))

(defun my/start-py-epc ()
  (interactive)
  (setq py-epc (epc:start-epc "python" '("/Users/ebanner/.dotfiles/elisp/ast-server.py"))))
(defun my/stop-py-epc ()
  (interactive)
  (epc:stop-epc py-epc))
(defun my/restart-py-epc ()
  (interactive)
  (my/stop-py-epc)
  (my/start-py-epc))

(defun my/restart-py-interactive ()
  (interactive)
  (my/restart-epcs)
  (my/restart-py-epc))

(defun my/do-process ()
  "Populate live-coding buffer
First clear it out."
  (interactive)
  (let ((code (my/buffer-string)))
    (my/set-active-context)
    (my/message "Doing code = %S" code)
    (if (string= *active-buffer-name* "N/A")
        (my/clear-worksheets)
      (my/clear-cells *active-buffer-name*))
    (my/annotate-make-cells-eval code)))

(defun my/set-active-context ()
  "Parse through the active frame and pick out the active buffer
Set *active-buffer-name* and *active-defun-name* accordingly."

  (defun my/get-buffer-names ()
    "Get the buffer names in the active frame
This function is used so we can pull out the worksheet name (i.e. name of the active function) to pass to the AST server"
    (interactive)
    (let* ((windows (window-list))
           (buffers (mapcar 'window-buffer windows))
           (buffer-names (mapcar 'buffer-name buffers)))
      buffer-names))

  (defun my/get-active-buffer-name ()
    "Return the name of the active function.
The active function will have a buffer in the active frame with the name context=func-name"
    (let* ((buffer-names (my/get-buffer-names))
           (active-buffer-singleton (seq-filter (lambda (buffer-name) (string-prefix-p "context=" buffer-name)) buffer-names))
           (active-buffer-name (car active-buffer-singleton)))
      (if (not active-buffer-name)
          "N/A"
        active-buffer-name)))

  (setq *active-buffer-name* (my/get-active-buffer-name))
  (setq *active-defun-name*
        (if (not (string= *active-buffer-name* "N/A"))
            (cadr (split-string *active-buffer-name* "context="))
          "N/A")))

(defun my/create-new-worksheet (buffer-name)
  "Create a new worksheet in a notebook who has a buffer called *epc-client*"
  (interactive "MFunction name: ")
  (save-excursion
    (save-window-excursion
      (with-current-buffer "*epc-client*"
        (call-interactively 'ein:notebook-worksheet-insert-next)
        (rename-buffer (concat buffer-name))))))

(defun my/loop (&optional a b c)
  "Calls the main `my/do-process' function but only if the current buffer has a certain name"
  (when (string= (buffer-name) "*client*")
    (message "python-info-current-defun #1 = %S" (python-info-current-defun))
    (my/message "Change at %S!" (list a b c))
    (my/message "Change is %S!" (buffer-substring-no-properties a b))
    (my/message "Change has a length of %S" c)
    (let ((line-no (line-number-at-pos)))
      (when (not (eq line-no *next-line-number-to-eval*))
        (setq *next-line-number-to-eval* line-no)
        (my/do-process)))
    (my/message (concat "*next line number to eval* = " (number-to-string *next-line-number-to-eval*)))))

(add-hook 'after-change-functions 'my/loop)
(my/start-epcs)
(my/start-py-epc)

(defun my/move-cell-window ()
  (interactive)
  (save-selected-window
    (let ((window (get-buffer-window "context=vectorizerBulk"))
          (cell-location (gethash (line-number-at-pos) *line-number-to-cell-map*)))
      (when cell-location
        (select-window window)
        (goto-char cell-location)
        (recenter 5)))))

(defun my/start-auto-scrolling-mode ()
  (interactive)
  (add-hook 'post-command-hook #'my/move-cell-window :local))
