(require 'server)
(unless (or (daemonp) (server-running-p))
  (server-start))

(use-package with-editor
  :hook
  ((shell-mode term-exec eshell-mode) .  with-editor-export-editor))

(provide 'init-daemon)
