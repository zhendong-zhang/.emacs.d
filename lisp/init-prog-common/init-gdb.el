(use-package gdb-mi
  :config
  (setq-default gdb-many-windows nil
                gdb-show-main t
                gdb-display-io-nopopup t
                gdb-restore-window-configuration-after-quit t))

(provide 'init-gdb)
