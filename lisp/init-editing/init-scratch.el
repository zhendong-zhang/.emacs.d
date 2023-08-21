(use-package scratch
  :defer t
  :commands (scratch))

(with-no-warnings
  (unless is-windows-nt
    (use-package persistent-scratch
      :config
      (persistent-scratch-setup-default))))

(provide 'init-scratch)
