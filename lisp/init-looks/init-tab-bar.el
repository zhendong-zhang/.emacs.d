(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  :config
  (tab-bar-mode 1)
  (custom-set-faces
   '(tab-bar
     ((t (:inherit mode-line-inactive :foreground nil :box nil))))
   '(tab-bar-tab
     ((default
       :inherit mode-line-inactive :box nil :background nil)
      (((background dark))
       :foreground "cyan4")
      (t
       :foreground "red4")))
   '(tab-bar-tab-inactive
     ((default
       :inherit mode-line-inactive :box nil :background nil)
      (((background dark))
       :foreground "white")
      (t
       :foreground "black")))))

(provide 'init-tab-bar)
