(use-package popwin
  :demand
  :diminish popwin-mode
  :bind
  ("M-g h" . popwin:popup-last-buffer)
  :custom
  (popwin:special-display-config
   '(;; Emacs
     ("*Miniedit Help*" :noselect t)
     ;;help-mode
     (completion-list-mode :noselect t)
     (compilation-mode :noselect t)
     ;;(grep-mode :noselect t)
     ;;(occur-mode :noselect t)
     ("*Pp Macroexpand Output*" :noselect t)
     "*Shell Command Output*"
     ;; VC
     "*vc-diff*"
     "*vc-change-log*"
     ;; Undo-Tree
     (" *undo-tree*" :width 60 :position right)
     ;; Anything
     ("^\\*anything.*\\*$" :regexp t)
     ;; SLIME
     "*slime-apropos*"
     "*slime-macroexpansion*"
     "*slime-description*"
     ("*slime-compilation*" :noselect t)
     "*slime-xref*"
     (sldb-mode :stick t)
     slime-repl-mode
     slime-connection-list-mode
     ("*quickrun*" :noselect t)
     xref--xref-buffer-mode
     (help-mode :noselect t)
     "*Flycheck errors*"
     "*Org Select*"
     " *Agenda Commands*"
     ("\\*Org Agenda.*\\*" :regexp t)
     ("CAPTURE-.*\\.org" :regexp t)
     ))
  :config
  (popwin-mode 1)
  )

(provide 'init-popwin)
