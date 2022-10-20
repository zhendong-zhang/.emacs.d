;; (use-package zenburn-theme
;;   :config
;;   (setq custom--inhibit-theme-enable nil)
;;   (zenburn-with-color-variables
;;     (custom-theme-set-faces
;;      'zenburn
;;      `(header-line ((t (:foreground ,zenburn-yellow
;;                                   :background ,zenburn-bg
;;                                   :box (:line-width -1 :style released-button)
;;                                   :extend t))))
;;      ;; spaceline
;;      `(spaceline-highlight-face ((t (:foreground ,zenburn-bg :background ,zenburn-blue-2))))
;;      ;; auto-highlight-symbol-mode
;;      `(ahs-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-face-unfocused ((t (:background ,zenburn-bg+2))))
;;      `(ahs-definition-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-definition-face-unfocused ((t (:background ,zenburn-bg+2))))
;;      `(ahs-plugin-default-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-plugin-default-face-unfocused ((t (:background ,zenburn-bg+2))))
;;      `(ahs-warning-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-plugin-whole-buffer-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-plugin-bod-face ((t (:background ,zenburn-bg+2))))
;;      `(ahs-edit-mode-face ((t (:background ,zenburn-bg+2))))
;;      ;; citre
;;      `(citre-peek-border-face ((t (:background ,zenburn-blue-2 :height 8 :extend t))))
;;      ;; bookmark
;;      `(bookmark-face ((t (:background ,zenburn-bg+2))))
;;      `(org-pomodoro-mode-line ((t (:inherit warning))))
;;      `(org-pomodoro-mode-line-overtime ((t (:inherit error))))
;;      `(org-pomodoro-mode-line-break ((t (:inherit success))))
;;      ;; lsp-bridge
;;      `(lsp-bridge-diagnostics-error-face ((t (:underline (:style wave :color ,zenburn-red-1)))))
;;      `(lsp-bridge-diagnostics-warning-face ((t (:underline (:style wave :color ,zenburn-yellow)))))
;;      `(lsp-bridge-diagnostics-info-face ((t (:underline (:style wave :color ,zenburn-cyan)))))
;;      `(lsp-bridge-ref-font-lock-header-line-text ((t (:foreground ,zenburn-green))))
;;      `(lsp-bridge-ref-font-lock-header-line-edit-mode ((t (:foreground ,zenburn-yellow))))
;;      `(lsp-bridge-ref-font-lock-match ((t (:foreground ,zenburn-orange))))
;;      `(lsp-bridge-ref-font-lock-file ((t (:foreground ,zenburn-blue))))
;;      `(lsp-bridge-ref-font-lock-line-number ((t (:foreground ,zenburn-bg+3))))
;;      `(lsp-bridge-ref-font-lock-column-number ((t (:foreground ,zenburn-bg+3))))
;;      `(lsp-bridge-ref-font-lock-diagnostic ((t (:foreground ,zenburn-red))))
;;      `(lsp-bridge-ref-font-lock-mark-changed ((t (:foreground ,zenburn-bg+05 :background ,zenburn-blue))))
;;      `(lsp-bridge-ref-font-lock-mark-deleted ((t (:foreground ,zenburn-red))))
;;      `(lsp-bridge-ref-font-lock-function-location ((t (:foreground ,zenburn-orange))))
;;      ;; acm
;;      `(acm-default-face ((t (:foreground ,zenburn-bg-1 :background ,zenburn-bg))))
;;      `(acm-select-face ((t (:foreground ,zenburn-yellow))))
;;      `(acm-border-face ((t (:background ,zenburn-bg-1))))
;;      ;; cal-china-x
;;      `(cal-china-x-important-holiday-face ((t (:background ,zenburn-red))))
;;      `(cal-china-x-general-holiday-face ((t (:background ,zenburn-green))))
;;      `(highlight-indentation-current-column-face ((t :inherit fringe)))
;;      ))
;;   (advice-add 'lsp-bridge-frame-background-color :override (lambda () "#383838"))
;;   (load-theme 'zenburn t)
;;   )

(use-package ef-themes
  :config
  (load-theme 'ef-light :no-confirm))

(provide 'init-themes)
