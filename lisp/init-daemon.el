;;; init-daemon.el --- emacsclient支持 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package server
  :commands server-running-p
  :config
  (unless (or (daemonp) (server-running-p))
    (server-start)))

(provide 'init-daemon)

;;; init-daemon.el ends here
