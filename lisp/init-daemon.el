(use-package server
  :functions (server-running-p)
  :config
  (unless (or (daemonp) (server-running-p))
  (server-start)))

(provide 'init-daemon)
