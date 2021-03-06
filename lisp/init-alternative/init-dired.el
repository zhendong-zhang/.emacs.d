(defun my-dired-setup()
  (require 'diff-hl-dired)
  (diff-hl-dired-mode)
  (when (featurep 'hungry-delete)
    (hungry-delete-mode -1)))

(use-package dired
  :defer t
  :ensure nil
  :hook
  (dired-mode . my-dired-setup)
  :init
  ;; 多目录操作时，优先以另外的目录做为目标
  (setq dired-dwim-target t)
  :config
  (use-package diff-hl))

(provide 'init-dired)
