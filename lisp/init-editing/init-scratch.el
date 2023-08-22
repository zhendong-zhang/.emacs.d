(use-package scratch
  :defer t
  :commands (scratch))

(use-package persistent-scratch
  :functions persistent-scratch-setup-default
  :config
  (persistent-scratch-setup-default))

(provide 'init-scratch)
