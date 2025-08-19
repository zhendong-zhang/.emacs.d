(use-package pyim
  :when (equal system-type 'windows-nt)
  :defer-incrementally pyim-autoselector pyim-common pyim-cstring pyim-dhashcache pyim-indicator pyim-page pyim-preview pyim-process pyim-scheme
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
  (pyim-default-scheme 'quanpin))

(use-package pyim-tsinghua-dict
  :github "redguardtoo/pyim-tsinghua-dict"
  :after pyim
  :config
  (pyim-tsinghua-dict-enable))

(use-package rime
  :unless (equal system-type 'windows-nt)
  :custom
  (rime-user-data-dir (expand-file-name "rime-config" user-emacs-directory))
  (rime-posframe-properties (list :background-color "#333333"
                                  :foreground-color "#dcdccc"
                                  :font "WenQuanYi Micro Hei Mono-14"
                                  :internal-border-width 10))
  (default-input-method "rime")
  (rime-show-candidate 'posframe)
  (rime-title "CH "))

(provide 'init-input-method)
