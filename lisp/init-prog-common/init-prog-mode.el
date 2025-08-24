;;; init-prog-mode.el --- 编程相关通用配置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package eldoc :diminish)

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package subword :diminish)

(defun generic-prog-mode-hook-setup ()
  "Generic setup for prog mode."
  (subword-mode)
  (turn-on-eldoc-mode)
  (setq show-trailing-whitespace t))

(add-hook 'prog-mode-hook 'generic-prog-mode-hook-setup)

(use-package quickrun
  :bind ("<f5>" . quickrun))

(use-package find-file
  :init
  (setq ff-quiet-mode t)
  (setq ff-always-try-to-create nil)
  :bind ("C-x C-o" . ff-find-other-file))

(use-package flycheck
  :diminish
  :init
  (setq next-error-verbose nil)
  (setq flycheck-emacs-lisp-load-path 'inherit)
  (setq flycheck-emacs-lisp-check-declare t)
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  :hook
  (after-init . global-flycheck-mode))

(use-package flycheck-projectile
  :commands flycheck-projectile-list-errors)

(defun create-dot-editorconfig ()
  (interactive)
  (eval-when-compile
    (declare-function projectile-project-root "projectile"))
  (with-temp-buffer
    (erase-buffer)
    (setq buffer-file-coding-system 'utf-8-unix)
    (insert "root = true
[*]
indent_style = space
charset = utf-8
end_of_line = lf
trim_trailing_whitespace = true
insert_final_newline = true
[*.{h,c,cpp}]
indent_size = 4
[*.md]
trim_trailing_whitespace = false")
    (write-file (concat (projectile-project-root) ".editorconfig"))
    ))

(use-package editorconfig
  :diminish
  :hook (first-file . editorconfig-mode))

(provide 'init-prog-mode)

;;; init-prog-mode.el ends here
