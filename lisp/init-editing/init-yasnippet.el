(use-package yasnippet
  :demand
  :config
  (yas-global-mode 1)
  :bind
  ("M-s s" . yas-insert-snippet))

(use-package yasnippet-snippets)

(provide 'init-yasnippet)
