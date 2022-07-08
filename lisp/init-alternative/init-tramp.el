(use-package tramp
  :defer t
  :init
  (setq tramp-auto-save-directory "~/tmp/tramp/")
  (setq tramp-default-method "ssh"))

(provide 'init-tramp)
