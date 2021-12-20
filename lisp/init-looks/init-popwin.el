(use-package popwin
  :demand
  :diminish popwin-mode
  :bind
  ("M-g h" . popwin:popup-last-buffer)
  :config
  (push '("*quickrun*" :noselect t) popwin:special-display-config)
  (push 'xref--xref-buffer-mode popwin:special-display-config)
  (push '(help-mode :noselect t) popwin:special-display-config)
  (push "*Flycheck errors*" popwin:special-display-config)
  (push "*Youdao Dictionary*" popwin:special-display-config)
  (push "*Org Select*" popwin:special-display-config)
  (push " *Agenda Commands*" popwin:special-display-config)
  (push '("\\*Org Agenda.*\\*" :regexp t) popwin:special-display-config)
  (push '("CAPTURE-.*\\.org" :regexp t) popwin:special-display-config)

  (popwin-mode 1)
  )

(provide 'init-popwin)
