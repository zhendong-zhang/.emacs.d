;;; init-web-mode.el --- xml/html支持 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package web-mode
  :mode
  ("\\.xml?\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t))

(provide 'init-web-mode)

;;; init-web-mode.el ends here
