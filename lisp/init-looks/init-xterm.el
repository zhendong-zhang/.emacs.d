;;; init-xterm.el --- 终端下使用设置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(add-hook 'after-make-console-frame-hooks
          (lambda ()
            ;; 鼠标操作支持
            (xterm-mouse-mode 1)))

(provide 'init-xterm)

;;; init-xterm.el ends here
