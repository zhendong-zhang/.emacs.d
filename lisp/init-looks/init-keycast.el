(use-package keycast
  :after doom-modeline
  :commands keycast-mode
  :demand
  :custom
  (keycast-mode-line-format "%k%c")
  :config
  (with-no-warnings
    (define-minor-mode keycast-mode
      "Show current command and its key binding in the mode line."
      :global t
      :group 'keycast-mode
      (if keycast-mode
          (progn
            (add-hook 'pre-command-hook 'keycast--update t)
            (add-to-list 'global-mode-string '("" keycast-mode-line " ")))
        (remove-hook 'pre-command-hook 'keycast--update)
        (setq global-mode-string (remove '("" keycast-mode-line " ") global-mode-string)))))
  (keycast-mode))

(provide 'init-keycast)
