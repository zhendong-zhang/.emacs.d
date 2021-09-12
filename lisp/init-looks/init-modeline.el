(use-package spaceline
  :init
  (setq powerline-default-separator 'utf-8)
  :config
  (require 'spaceline-config)
  (spaceline-emacs-theme `(buffer-encoding)))

(provide 'init-modeline)
