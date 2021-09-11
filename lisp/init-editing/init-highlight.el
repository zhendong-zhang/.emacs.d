(use-package hl-todo
  :config
  (global-hl-todo-mode t))

(use-package indent-guide
  :diminish
  :init
  (setq indent-guide-recursive t)
  :config
  (indent-guide-global-mode t))

(provide 'init-highlight)

