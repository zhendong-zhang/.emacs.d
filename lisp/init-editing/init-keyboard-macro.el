;;; init-keyboard-macro.el --- 键盘宏 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package elmacro
  :defer-incrementally t
  :diminish
  :functions elmacro-pp-to-string elmacro-make-defun
  :config
  (elmacro-mode)

  (defun my-elmacro-show-defun (name commands)
  "Save a defun named NAME from COMMANDS to custom file."
  (let* ((buffer (generate-new-buffer (format "* elmacro - %s *" name))))
    (find-file (or custom-file user-init-file))
    (goto-char (point-max))
    (newline 1)
    (insert (elmacro-pp-to-string (elmacro-make-defun (make-symbol name) commands)))))

  (declare-function my-elmacro-show-defun "init-keyboard-macro")
  (advice-add 'elmacro-show-defun :override #'my-elmacro-show-defun)

  (defalias 'save-last-macro 'elmacro-show-last-macro))

(provide 'init-keyboard-macro)

;;; init-keyboard-macro.el ends here
