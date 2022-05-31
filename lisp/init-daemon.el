(use-package server
  :functions (server-running-p)
  :init
  (setq server-auth-dir (no-littering-expand-var-file-name "server/"))
  :config
  (unless (or (daemonp) (server-running-p))
  (server-start)))

(provide 'init-daemon)
