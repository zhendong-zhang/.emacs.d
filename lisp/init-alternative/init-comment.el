;;; init-comment.el --- 注释 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package evil-nerd-commenter
  :bind
  ([remap comment-dwim] . evilnc-comment-or-uncomment-lines))

(provide 'init-comment)

;;; init-comment.el ends here
