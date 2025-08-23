;;; init-no-littering.el --- 临时文件管理 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package no-littering
  :config
  (no-littering-theme-backups)
  ;; FIXME: 设置回默认值，否则emacsclient找不到
  (setq server-auth-dir (locate-user-emacs-file "server/"))

  (eval-when-compile
    (defvar recentf-exclude)
    (defvar quelpa-dir)
    (declare-function recentf-expand-file-name "recentf"))

  (with-eval-after-load 'recentf
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-var-directory))
    (add-to-list 'recentf-exclude
                 (recentf-expand-file-name no-littering-etc-directory)))

  (with-eval-after-load 'quelpa
      (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

  (setq custom-file (no-littering-expand-etc-file-name "custom.el")))

(provide 'init-no-littering)

;;; init-no-littering.el ends here
