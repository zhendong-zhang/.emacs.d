(use-package em-smart :ensure nil)

(use-package eshell
  :demand
  :hook (eshell-mode . eshell-smart-initialize)
  :config
  (use-package eshell-z)
  (use-package eshell-prompt-extras
    :defines (eshell-highlight-prompt eshell-prompt-function)
    :init
    (setq eshell-highlight-prompt nil
          eshell-prompt-function 'epe-theme-dakrone)))

(provide 'init-eshell)
