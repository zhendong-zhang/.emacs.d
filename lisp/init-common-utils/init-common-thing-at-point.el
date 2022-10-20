;; from https://emacs-china.org/t/xxx-thing-at-point/18047/10
(defcustom common-thing-at-point-commands '()
  "which command should get thing at point.")

(defvar common-thing-at-point-overwrite-commands
  '(self-insert-command
    yank
    yank-pop
    org-yank))

(defun common-thign-at-point-minibuffer-actions ()
  (remove-hook 'pre-command-hook 'common-thign-at-point-minibuffer-actions t)
  (cond ((and (memq last-command common-thing-at-point-commands)
              (equal (this-command-keys-vector) (kbd "M-p")))
         ;; repeat one time to get straight to the first history item
         (setq unread-command-events
               (append unread-command-events
                       (listify-key-sequence (kbd "M-p")))))
        ((memq this-command common-thing-at-point-overwrite-commands)
         (delete-region (point) (point-max)))))

(defun common-thing-at-point-minibuffer-setup ()
  (when (memq this-command common-thing-at-point-commands)
    (let ((pre-insert-string (with-minibuffer-selected-window
                               (or (seq-some (lambda (thing)
                                               (thing-at-point thing t))
                                             '(region url symbol))
                                   ""))))
      (save-excursion
        (insert (propertize pre-insert-string 'face 'shadow))))
    (add-hook 'pre-command-hook 'common-thign-at-point-minibuffer-actions nil t)))
(add-hook 'minibuffer-setup-hook #'common-thing-at-point-minibuffer-setup)

(defun set-use-common-thing-at-point (&rest commands)
  (dolist (command commands)
    (push command common-thing-at-point-commands)))

(provide 'init-common-thing-at-point)
