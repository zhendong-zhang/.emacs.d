(use-package auto-package-update
  :config
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (setq auto-package-update-last-update-day-path
        (no-littering-expand-var-file-name auto-package-update-last-update-day-filename))
  (auto-package-update-maybe))

(provide 'init-auto-package-update)
