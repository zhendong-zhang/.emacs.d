(use-package treesit
  :ensure nil
  :init
  (setq treesit-font-lock-level 4)
  :config

  (use-package treesit-auto
    :commands global-treesit-auto-mode
    :custom
    (treesit-auto-install 'prompt)
    :config
    (global-treesit-auto-mode))

  (use-package combobulate
    :hook ((python-ts-mode . combobulate-mode)
           (js-ts-mode . combobulate-mode)
           (css-ts-mode . combobulate-mode)
           (yaml-ts-mode . combobulate-mode)
           (typescript-ts-mode . combobulate-mode)
           (tsx-ts-mode . combobulate-mode))
    :github "mickeynp/combobulate"))

(provide 'init-treesit)
