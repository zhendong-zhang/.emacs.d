(setq vc-handled-backends '(Git SVN))

(use-package fullframe)

(use-package magit
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  (setq magit-refresh-status-buffer nil)
  :config
  (declare-function fullframe/maybe-restore-configuration "fullframe" (config))
  (fullframe magit-status magit-mode-quit-window)
  :bind
  (:map magit-status-mode-map
        ("C-<tab>" . nil)))

(use-package git-auto-commit-mode
  :defer t
  :config
  ;; (setq-default gac-debounce-interval 600)
  (setq-default gac-automatically-push-p t))

(provide 'init-vc)
