;;; init-which-key.el --- 快捷键提示 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package which-key
  :defer-incrementally t
  :diminish
  :init
  (setq which-key-sort-order 'which-key-key-order-alpha)
  :bind ("C-h C-b" . which-key-show-top-level)
  :config
  (which-key-mode 1))

(provide 'init-which-key)

;;; init-which-key.el ends here
