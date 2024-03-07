(when (featurep 'modus-themes)
  (load-theme 'modus-operandi :no-confirm))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'init-themes)
