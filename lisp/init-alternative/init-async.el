;;; init-async.el --- 异步相关便利函数 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defvar async-current-window nil)

(defun package-list-packages-async()
  (interactive)
  (eval-when-compile
    (declare-function async-start "async"))
  (use-package async)
  (setq async-current-window (selected-window))
  (async-start
   (lambda ()
     (list-packages))
   (lambda (_)
     (save-excursion
       (when async-current-window
         (select-window async-current-window))
       (package-list-packages-no-fetch)))))

(provide 'init-async)

;;; init-async.el ends here
