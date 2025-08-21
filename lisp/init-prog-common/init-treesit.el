;;; init-treesit.el --- tree sitter -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package treesit
  :defer-incrementally t
  :ensure nil
  :when (version<= "28.1" emacs-version)
  :init
  (setq treesit-font-lock-level 4)
  :config

  (use-package treesit-auto
    :commands global-treesit-auto-mode
    :demand
    :custom
    (treesit-auto-install 'prompt)
    :config
    (global-treesit-auto-mode))

  (use-package combobulate
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    :github "mickeynp/combobulate"))

(provide 'init-treesit)

;;; init-treesit.el ends here
