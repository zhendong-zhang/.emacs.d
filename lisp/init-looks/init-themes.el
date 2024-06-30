(when (version<= "27.1" emacs-version)
  (load-theme 'modus-operandi :no-confirm))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'init-themes)
