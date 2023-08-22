(use-package fullframe)

;; TODO magit安装重启后报错,删除magit重新安装即可
(use-package magit
  :defer-incrementally magit-core magit-diff magit-log magit-wip magit-apply magit-repos git-commit format-spec with-editor magit
  :commands (magit-status)
  :custom
  (magit-refresh-status-buffer nil)
  (magit-commit-diff-inhibit-same-window t)
  :config
  (declare-function fullframe/maybe-restore-configuration "fullframe" (config))
  (fullframe magit-status magit-mode-quit-window)
  :bind
  (:map magit-status-mode-map
        ("C-<tab>" . nil)))

(use-package git-auto-commit-mode
  :defer t
  :commands (git-auto-commit-mode)
  :config
  ;; (setq-default gac-debounce-interval 600)
  (setq-default gac-automatically-push-p t))

(use-package diff-hl
  :config
  (global-diff-hl-mode))

(provide 'init-vc)
