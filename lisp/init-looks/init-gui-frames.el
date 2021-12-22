;;----------------------------------------------------------------------------
;; Suppress GUI features
;;----------------------------------------------------------------------------
(setq use-file-dialog nil
      use-dialog-box nil
      inhibit-startup-screen t
      inhibit-startup-echo-area-message t)
(setq-default right-fringe-width 0
              left-fringe-width 5)

;; no scrollbar, no menubar, no toolbar
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(when window-system
  (global-hl-line-mode 1)
  (global-linum-mode 1)
  (use-package linum-relative
    :diminish linum-relative-mode
    :init
    (setq linum-relative-current-symbol "")
    :config
    (linum-relative-global-mode)))

(which-function-mode t)
(column-number-mode t)
(size-indication-mode 1)

(use-package time
  :ensure nil
  :init
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-format "%a %m.%d %R"
        display-time-default-load-average nil)
  :config
  (display-time-mode t))

(add-hook 'window-setup-hook 'toggle-frame-maximized)

(provide 'init-gui-frames)
