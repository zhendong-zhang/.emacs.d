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

(provide 'init-input-method)
