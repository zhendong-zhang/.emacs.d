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

(defun create-dot-editorconfig ()
  (interactive)
  (declare-function projectile-project-root "projectile")
  (with-temp-buffer
    (erase-buffer)
    (insert "root = true
[*]
indent_style = space
indent_size = 4
charset = utf-8
trim_trailing_whitespace = true
insert_final_newline = true
end_of_line = lf
[*.md]
trim_trailing_whitespace = false")
    (write-file (concat (projectile-project-root) ".editorconfig"))
    ))

(use-package editorconfig
  :diminish
  :hook (first-file . editorconfig-mode))

(provide 'init-prog-mode)
