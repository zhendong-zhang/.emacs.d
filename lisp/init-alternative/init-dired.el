;;; init-dired.el --- dired -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defun my-dired-setup()
  (require 'diff-hl-dired)
  (diff-hl-dired-mode)
  (when (featurep 'hungry-delete)
    (hungry-delete-mode -1)))

(use-package dired
  :defer-incrementally dired-loaddefs dnd
  :ensure nil
  :hook
  (dired-mode . my-dired-setup)
  :init
  ;; 多目录操作时，优先以另外的目录做为目标
  (setq dired-dwim-target t))

(provide 'init-dired)

;;; init-dired.el ends here
