(add-hook 'after-make-console-frame-hooks
          (lambda ()
            ;; 鼠标操作支持
            (xterm-mouse-mode 1)))

(provide 'init-xterm)
