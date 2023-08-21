(use-package tramp
  :defines is-windows-nt
  :defer t
  :init
  (setq tramp-auto-save-directory "~/tmp/tramp/")
  (setq tramp-default-method (if is-windows-nt "plink" "ssh")))

(provide 'init-tramp)
