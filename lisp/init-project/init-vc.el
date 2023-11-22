(use-package fullframe)

;; TODO magit安装重启后报错,删除magit重新安装即可
(use-package magit
  :defer-incrementally magit-core magit-diff magit-log magit-wip magit-apply magit-repos git-commit format-spec with-editor magit
  :commands (magit-status)
  :init
  (setq magit-refresh-status-buffer nil)
  :config
  (declare-function fullframe/maybe-restore-configuration "fullframe" (config))
  (fullframe magit-status magit-mode-quit-window)
  :bind
  (:map magit-status-mode-map
        ("C-<tab>" . nil)))

(use-package git-auto-commit-mode
  :commands (git-auto-commit-mode)
  :preface
  (defun gac-kill-emacs-hook ()
    (when (and (bound-and-true-p gac-debounce-interval)
               (bound-and-true-p gac--debounce-timers))
      (maphash #'(lambda (key value)
                   (message "saving %s." key)
                   (gac--after-save key))
               gac--debounce-timers)))
  :hook
  (kill-emacs . gac-kill-emacs-hook)
  :config
  (setq-default gac-debounce-interval (* 60 60 24))
  (setq-default gac-automatically-push-p t))

(use-package diff-hl
  :config
  (global-diff-hl-mode)
  (require 'diff-hl-margin)
  (diff-hl-margin-mode))

(provide 'init-vc)
