;;; init-scratch.el --- scratch -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package scratch
  :commands (scratch))

(use-package persistent-scratch
  :functions persistent-scratch-setup-default
  :config
  (persistent-scratch-setup-default))

(provide 'init-scratch)

;;; init-scratch.el ends here
