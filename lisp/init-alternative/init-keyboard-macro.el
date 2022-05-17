(use-package elmacro
  :config
  (elmacro-mode)

  (defun my-elmacro-show-defun (name commands)
  "Save a defun named NAME from COMMANDS to this file."
  (declare-function elmacro-pp-to-string "elmacro")
  (declare-function elmacro-make-defun "elmacro")
  (let* ((buffer (generate-new-buffer (format "* elmacro - %s *" name))))
    (find-file (expand-file-name "lisp/init-alternative/init-keyboard-macro.el" user-emacs-directory))
    (goto-char (point-max))
    (forward-line -2)
    (newline 1)
    (insert (elmacro-pp-to-string (elmacro-make-defun (make-symbol name) commands)))))

  (declare-function my-elmacro-show-defun "init-keyboard-macro")
  (advice-add 'elmacro-show-defun :override #'my-elmacro-show-defun)
  )

(provide 'init-keyboard-macro)
