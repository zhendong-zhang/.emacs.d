(use-package tab-bar
  :ensure nil
  :preface
  (defun my-clean-buffer-list-delay (oldfun name)
    (or (and (tab-bar-get-buffer-tab name) (* 30 24 60 60))
        (apply oldfun (list name))))
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
     ((t (:inherit mode-line-inactive :foreground unspecified :box unspecified))))
   '(tab-bar-tab
     ((default
       :inherit mode-line-inactive :box unspecified :background unspecified)
      (((background dark))
       :foreground "cyan4")
      (t
       :foreground "red4")))
   '(tab-bar-tab-inactive
     ((default
       :inherit mode-line-inactive :box unspecified :background unspecified)
      (((background dark))
       :foreground "white")
      (t
       :foreground "black"))))
  (advice-add 'clean-buffer-list-delay :around 'my-clean-buffer-list-delay))

(provide 'init-tab-bar)
