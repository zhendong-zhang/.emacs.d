(use-package keycast
  :after doom-modeline
  :preface
  (define-minor-mode keycast-global-mode
    "Show current command and its key binding in the mode line."
    :global t
    :group 'keycast-mode
    (if keycast-global-mode
        (progn
          (add-hook 'pre-command-hook 'keycast--update t)
          (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
      (remove-hook 'pre-command-hook 'keycast--update)
      (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string))))
  :demand
  :custom
  (keycast-mode-line-format "%k%c")
  :config
  (keycast-global-mode))

(provide 'init-keycast)
