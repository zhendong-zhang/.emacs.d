(if (not is-windows-nt)
    (use-package rime
      :init
      (setq rime-user-data-dir (expand-file-name "rime-config" user-emacs-directory)
            rime-posframe-properties
            (list :background-color "#333333"
                  :foreground-color "#dcdccc"
                  :font "WenQuanYi Micro Hei Mono-14"
                  :internal-border-width 10)
            default-input-method "rime"
            rime-show-candidate 'posframe
            rime-title "CH "))
  (use-package pyim
    :defer-incrementally pyim-autoselector pyim-common pyim-cstring pyim-dhashcache pyim-indicator pyim-page pyim-preview pyim-process pyim-scheme pyim
    :init
    (setq default-input-method "pyim")
    :bind (:map pyim-mode-map
                ("." . pyim-next-page)
                ("," . pyim-previous-page))
    :config
    (setq pyim-page-length 5
          pyim-page-tooltip 'posframe
          pyim-page-style 'vertical
          pyim-dcache-backend 'pyim-dhashcache)
    (pyim-default-scheme 'quanpin)

    (install-package-from-github 'pyim-tsinghua-dict "redguardtoo/pyim-tsinghua-dict")
    (use-package pyim-tsinghua-dict
      :config
      (pyim-tsinghua-dict-enable))
    ))

(provide 'init-input-method)
