(use-package doom-modeline
  :custom
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  :hook
  (after-init . doom-modeline-mode))

(provide 'init-modeline)
