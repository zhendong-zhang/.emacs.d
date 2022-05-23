(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))

;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)

;; 解决windows emacs调用外部进程的中文乱码问题
(when is-windows-nt
  (w32-set-system-coding-system 'gbk)
  (add-to-list 'process-coding-system-alist '("[cC][mM][dD][pP][rR][oO][xX][yY]" . (utf-8 . utf-8))))

(provide 'init-locales)
