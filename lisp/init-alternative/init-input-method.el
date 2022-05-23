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
    :defer t
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

    (use-package pyim-tsinghua-dict
      :load-path "site-lisp/pyim-tsinghua-dict"
      :init
      (require 'pyim-tsinghua-dict)
      :config
      (pyim-tsinghua-dict-enable))
    ))

(provide 'init-input-method)
