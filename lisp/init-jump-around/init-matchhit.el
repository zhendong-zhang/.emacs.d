;;; init-matchhit.el --- 在匹配符号间跳转 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package evil-matchit
  :bind
  ("M-g ." . evilmi-jump-items-native))

(provide 'init-matchhit)

;;; init-matchhit.el ends here
