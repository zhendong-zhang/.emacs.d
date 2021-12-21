(use-package tramp
  :init
  (setq tramp-auto-save-directory "~/tmp/tramp/")
  (setq tramp-default-method "ssh"))

(provide 'init-tramp)
