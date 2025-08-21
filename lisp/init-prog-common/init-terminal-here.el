;;; init-terminal-here.el --- 调用外部终端 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package terminal-here
  :bind
  (("C-<f5>" . terminal-here-launch)
   ("C-<f6>" . terminal-here-project-launch))
  :config
  (setq terminal-here-linux-terminal-command 'tilix)
  )

(provide 'init-terminal-here)

;;; init-terminal-here.el ends here
