(use-package elmacro
  :defer 5
  :diminish
  :config
  (elmacro-mode)

  (defun my-elmacro-show-defun (name commands)
  "Save a defun named NAME from COMMANDS to custom file."
  (declare-function elmacro-pp-to-string "elmacro")
  (declare-function elmacro-make-defun "elmacro")
  (let* ((buffer (generate-new-buffer (format "* elmacro - %s *" name))))
    (find-file (or custom-file user-init-file))
    (goto-char (point-max))
    (newline 1)
    (insert (elmacro-pp-to-string (elmacro-make-defun (make-symbol name) commands)))))

  (declare-function my-elmacro-show-defun "init-keyboard-macro")
  (advice-add 'elmacro-show-defun :override #'my-elmacro-show-defun)

  (defalias 'save-last-macro 'elmacro-show-last-macro)
  )

(provide 'init-keyboard-macro)
