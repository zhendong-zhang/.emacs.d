(use-package ef-themes
  :custom
  (ef-themes-common-palette-overrides
   '((bg-tab-bar      bg-alt)
     (bg-tab-current  bg-alt)
     (bg-tab-other    bg-alt)
     (bg-hover-secondary bg-inactive)))
  :config
  (load-theme 'ef-light :no-confirm))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'init-themes)
