;; from https://emacs-china.org/t/xxx-thing-at-point/18047/10
(defcustom common-region-at-point-commands
  '(consult-line
    consult-ripgrep
    consult-locate
    quick-calc)
  "which command should get region at point."
  :type 'list
  :group 'common-thing-at-point)

(defcustom common-symbol-at-point-commands
  '(consult-line
    consult-ripgrep
    consult-locate
    anzu-query-replace
    anzu-query-replace-regexp
    replace-string)
  "which command should get symbol at point."
  :type 'list
  :group 'common-thing-at-point)

(defvar common-thing-at-point-overwrite-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun common-thing-at-point-get-things (command)
  (let ((things nil))
    (when (and (use-region-p) (memq command common-region-at-point-commands))
      (push 'region things))
    (when (and (not (use-region-p)) (memq command common-symbol-at-point-commands))
      (push 'symbol things))
    things))

(defun common-thing-at-point-minibuffer-actions ()
  (remove-hook 'pre-command-hook 'common-thing-at-point-minibuffer-actions t)
  (cond ((and (equal (this-command-keys-vector) (kbd "M-p"))
              (common-thing-at-point-get-things this-command))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command common-thing-at-point-overwrite-commands)
         (beginning-of-line 1)
         (delete-region (point) (point-max)))))

(defun common-thing-at-point-minibuffer-setup ()
  (when (equal current-prefix-arg nil) ;; without C-u
    (when-let (initial-input (with-minibuffer-selected-window
                               (seq-some (lambda (thing)
                                           (thing-at-point thing t))
                                         (common-thing-at-point-get-things this-command))))
      (save-excursion
        (insert (propertize initial-input 'face 'shadow)))
      (add-hook 'pre-command-hook 'common-thing-at-point-minibuffer-actions nil t))))
(add-hook 'minibuffer-setup-hook #'common-thing-at-point-minibuffer-setup)

(provide 'init-common-thing-at-point)
