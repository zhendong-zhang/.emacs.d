;;; init-gui-frames.el --- 外观设置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

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
  (setq frame-title-format
        '(buffer-file-name (:eval (abbreviate-file-name buffer-file-name))
                           (dired-directory dired-directory "%b")))
  (global-hl-line-mode 1)
  (global-display-line-numbers-mode 1)
  (setq-default display-line-numbers-width 3)
  (use-package linum-relative
    :diminish linum-relative-mode
    :commands linum-relative-global-mode
    :demand
    :custom
    (linum-relative-current-symbol "")
    (linum-relative-backend 'display-line-numbers-mode)
    :config
    (linum-relative-global-mode)))

(which-function-mode t)
(column-number-mode t)
(size-indication-mode 1)

(use-package time
  :defer-incrementally t
  :ensure nil
  :init
  (setq display-time-24hr-format t
        display-time-day-and-date t
        display-time-format "%a %m.%d %R"
        display-time-default-load-average nil)
  :config
  (unless (equal system-type 'windows-nt)
    (display-time-mode t)))

(provide 'init-gui-frames)

;;; init-gui-frames.el ends here
