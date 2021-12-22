(use-package spaceline
  :hook
  (after-init . (lambda ()
                  (require 'spaceline-config)
                  (spaceline-emacs-theme '(buffer-encoding))
                  (spaceline-toggle-buffer-encoding-abbrev-off)
                  ))
  :init
  (setq powerline-default-separator 'wave))

(provide 'init-modeline)
