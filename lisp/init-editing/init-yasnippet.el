(use-package yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :config
  (yas-global-mode 1)
  :bind
  ("M-s s" . yas-insert-snippet))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)
