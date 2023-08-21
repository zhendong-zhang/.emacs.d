(use-package server
  :functions server-running-p
  :config
  (with-no-warnings
    (unless (or (daemonp) (server-running-p))
      (server-start))))

(provide 'init-daemon)
