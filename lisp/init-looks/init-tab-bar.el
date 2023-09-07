(use-package tab-bar
  :ensure nil
  :custom
  (tab-bar-format '(tab-bar-format-tabs tab-bar-separator))
  (tab-bar-close-button-show nil)
  (tab-bar-new-tab-choice "*scratch*")
  (tab-bar-show 1)
  (tab-bar-tab-hints t)
  (tab-bar-auto-width nil)
  :config
  (tab-bar-mode 1))

(provide 'init-tab-bar)
