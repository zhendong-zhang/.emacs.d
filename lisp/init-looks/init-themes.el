(use-package zenburn-theme
  :config
  (setq custom--inhibit-theme-enable nil)
  (zenburn-with-color-variables
    (custom-theme-set-faces
     'zenburn
     ;; spaceline
     `(spaceline-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-2))))
     ;; auto-highlight-symbol-mode
     `(ahs-face ((t (:background ,zenburn-bg+2))))
     `(ahs-face-unfocused ((t (:background ,zenburn-bg+2))))
     `(ahs-definition-face ((t (:background ,zenburn-bg+2))))
     `(ahs-definition-face-unfocused ((t (:background ,zenburn-bg+2))))
     `(ahs-plugin-default-face ((t (:background ,zenburn-bg+2))))
     `(ahs-plugin-default-face-unfocused ((t (:background ,zenburn-bg+2))))
     `(ahs-warning-face ((t (:background ,zenburn-bg+2))))
     `(ahs-plugin-whole-buffer-face ((t (:background ,zenburn-bg+2))))
     `(ahs-plugin-bod-face ((t (:background ,zenburn-bg+2))))
     `(ahs-edit-mode-face ((t (:background ,zenburn-bg+2))))
     ;; citre
     `(citre-peek-border-face ((t (:background ,zenburn-blue-2 :height 8 :extend t))))
     ;; bookmark
     `(bookmark-face ((t (:background ,zenburn-bg+2))))
    `(org-pomodoro-mode-line ((t (:inherit warning))))
    `(org-pomodoro-mode-line-overtime ((t (:inherit error))))
    `(org-pomodoro-mode-line-break ((t (:inherit success))))
     )))

(defun zenburn-setup ()
  "Activate another dark color theme."
  (interactive)
  (load-theme 'zenburn t))

(add-hook 'after-init-hook 'zenburn-setup)

(provide 'init-themes)
