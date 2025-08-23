;;; init-cc-mode.el --- c/c++支持 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package find-file
    :config
    (add-to-list 'cc-search-directories "/usr/include/c++/*"))

(use-package cc-mode
    :mode
    ("\\.h\\'" . c++-mode))

(provide 'init-cc-mode)

;;; init-cc-mode.el ends here
