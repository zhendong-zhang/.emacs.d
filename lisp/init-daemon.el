(use-package server
  :commands server-running-p
  :config
  (unless (or (daemonp) (server-running-p))
    (server-start)))

(provide 'init-daemon)
