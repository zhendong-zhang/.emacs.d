(use-package scratch
  :defer t
  :commands (scratch))

(use-package persistent-scratch
  :if (not is-windows-nt)
  :config
  (persistent-scratch-setup-default))

(provide 'init-scratch)
