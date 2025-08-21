;;; init-yasnippet.el --- yasnippet配置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package yasnippet
  :defer-incrementally eldoc easymenu help-mode
  :magic-fallback ("%snippet" . snippet-mode)
  :config
  (yas-global-mode 1)
  :bind
  ("M-s y" . yas-insert-snippet))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)

;;; init-yasnippet.el ends here
