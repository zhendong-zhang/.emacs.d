;; from https://emacs-china.org/t/xxx-thing-at-point/18047/10
(defcustom common-thing-at-point-commands '()
  "which command should get thing at point.")

(defcustom common-thing-at-point-commands-unless-use-region '()
  "which command should get thing at point unless use region.")

(defvar common-thing-at-point-overwrite-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun common-thing-at-point-minibuffer-actions ()
  (remove-hook 'pre-command-hook 'common-thing-at-point-minibuffer-actions t)
  (cond ((and (or (memq last-command common-thing-at-point-commands)
                  (memq last-command common-thing-at-point-commands-unless-use-region))
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command common-thing-at-point-overwrite-commands)
         (delete-region (point) (point-max)))))

(defun common-thing-at-point-minibuffer-setup ()
  (when (and (memq this-command common-thing-at-point-commands)
             (equal current-prefix-arg nil))
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some (lambda (thing)
                                               (thing-at-point thing t))
                                             '(region url symbol))
                                   ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'common-thing-at-point-minibuffer-actions nil t))
  (when (memq this-command common-thing-at-point-commands-unless-use-region)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (if (region-active-p)
                                   ""
                                 (or (seq-some (lambda (thing)
                                                 (thing-at-point thing t))
                                               '(url symbol))
                                     "")))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'common-thing-at-point-minibuffer-actions nil t)))
(add-hook 'minibuffer-setup-hook #'common-thing-at-point-minibuffer-setup)

(defun set-use-common-thing-at-point (&rest commands)
  (dolist (command commands)
    (push command common-thing-at-point-commands)))

(defun set-use-common-thing-at-point-unless-use-region (&rest commands)
  (dolist (command commands)
    (push command common-thing-at-point-commands-unless-use-region)))

(provide 'init-common-thing-at-point)
