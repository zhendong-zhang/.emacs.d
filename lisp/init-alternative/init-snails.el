(use-package snails
  :quelpa (snails :fetcher github :repo "manateelazycat/snails")
  :config
  (setq snails-show-with-frame nil
        snails-input-buffer-text-scale 1)
  :bind
  ("M-g s" . snails)
  )

(provide 'init-snails)
