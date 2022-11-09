(use-package symbol-overlay
  :diminish
  :bind
  (:map symbol-overlay-mode-map
        ("M-i" . symbol-overlay-put)
        ("M-p" . symbol-overlay-jump-prev)
        ("M-n" . symbol-overlay-jump-next))
  :hook
  (prog-mode . symbol-overlay-mode))

(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package highlight-indentation)

(provide 'init-highlight)
