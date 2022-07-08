(use-package web-mode
  :mode
  ("\\.xml?\\'" . web-mode)
  ("\\.html?\\'" . web-mode)
  :init
  (setq web-mode-enable-auto-closing t
        web-mode-enable-auto-pairing t
        web-mode-enable-css-colorization t))

(provide 'init-web-mode)
