(use-package yasnippet
  :demand
  :bind
  (:map yas-minor-mode-map
        ([tab] . nil)
        ("TAB" . nil))
  :config
  (yas-global-mode 1))

(use-package yasnippet-snippets)
(use-package ivy-yasnippet)

(provide 'init-yasnippet)
