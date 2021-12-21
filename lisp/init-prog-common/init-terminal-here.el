(use-package terminal-here
  :bind
  (("C-<f5>" . terminal-here-launch)
   ("C-<f6>" . terminal-here-project-launch))
  :config
  (setq terminal-here-linux-terminal-command 'tilix)
  )

(provide 'init-terminal-here)
