;;; init-tramp.el --- 远程编辑 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package tramp
  :defer t
  :init
  (setq tramp-auto-save-directory "~/tmp/tramp/")
  (setq tramp-default-method (if (equal system-type 'windows-nt) "plink" "ssh")))

(provide 'init-tramp)

;;; init-tramp.el ends here
