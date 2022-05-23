(use-package snails
  :load-path "site-lisp/snails"
  :init
  (require 'snails)
  :config
  (setq snails-show-with-frame nil
        snails-input-buffer-text-scale 1)
  :bind
  ("M-g s" . snails)
  )

(provide 'init-snails)
