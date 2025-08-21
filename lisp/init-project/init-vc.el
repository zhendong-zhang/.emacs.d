;;; init-vc.el --- 版本控制 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package magit
  :defer-incrementally magit-core magit-diff magit-log magit-wip magit-apply magit-repos git-commit format-spec with-editor magit
  :commands (magit-status)
  :init
  (setq magit-refresh-status-buffer nil)
  :config
  (eval-when-compile
    (declare-function fullframe/maybe-restore-configuration "fullframe" (config)))
  (use-package fullframe)
  (fullframe magit-status magit-mode-quit-window)
  :bind
  (:map magit-status-mode-map
        ("C-<tab>" . nil)))

(use-package git-auto-commit-mode
  :commands (git-auto-commit-mode)
  :functions gac--after-save
  :hook
  (kill-emacs . gac-kill-emacs-hook)
  :preface
  (defun gac-kill-emacs-hook ()
    (when (and (bound-and-true-p gac-debounce-interval)
               (bound-and-true-p gac--debounce-timers))
      (maphash #'(lambda (key _)
                   (message "saving %s." key)
                   (gac--after-save key))
               gac--debounce-timers)))
  :config
  (setq-default gac-debounce-interval (* 60 60 24))
  (setq-default gac-automatically-push-p t))

(use-package diff-hl
  :defer-incrementally t
  :config
  (global-diff-hl-mode)
  (require 'diff-hl-margin)
  (diff-hl-margin-mode))

(provide 'init-vc)

;;; init-vc.el ends here
