(use-package yasnippet
  :defer 5
  :demand
  :config
  (yas-global-mode 1)
  :bind
  ("M-s s" . yas-insert-snippet))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)
