(use-package tramp
  :defer t
  :init
  (setq tramp-auto-save-directory "~/tmp/tramp/")
  (setq tramp-default-method (if (equal system-type 'windows-nt) "plink" "ssh")))

(provide 'init-tramp)
