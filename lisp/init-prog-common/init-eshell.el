;;; init-eshell.el --- 终端模拟器 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package eshell
  :defer-incrementally esh-util esh-module esh-proc esh-io esh-cmd
  :hook (eshell-mode . eshell-smart-initialize)
  :config
  (use-package em-smart :ensure nil)
  (use-package eshell-z)
  (use-package eshell-prompt-extras
    :defines (eshell-highlight-prompt eshell-prompt-function)
    :init
    (setq eshell-highlight-prompt t
          eshell-prompt-function 'epe-theme-dakrone)))

(provide 'init-eshell)

;;; init-eshell.el ends here
