;; (when (fboundp 'set-charset-priority)
;;   (set-charset-priority 'unicode))

;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)

(when is-windows-nt
  ;; 解决windows emacs调用外部进程的中文乱码问题
  (defadvice projectile-files-via-ext-command (around my-projectile-files-via-ext-command activate)
    (let ((cmdproxy-old-encoding (cdr (assoc "[cC][mM][dD][pP][rR][oO][xX][yY]" process-coding-system-alist))))
      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" '(utf-8 . utf-8))
      ad-do-it
      (modify-coding-system-alist 'process "[cC][mM][dD][pP][rR][oO][xX][yY]" cmdproxy-old-encoding)
      ))
  (add-to-list 'process-coding-system-alist '("[eE][sS]" . (gbk . gbk)))
  (add-to-list 'process-coding-system-alist '("[gG][iI][tT]" . (utf-8 . utf-8)))
  ;; emacs client 中文文件名乱码问题
  (setq w32-unicode-filenames nil)
  (setq file-name-coding-system 'gb18030)
  ;(w32-set-system-coding-system 'gb18030)
  ;(set-language-environment 'gb18030)
  )

(provide 'init-locales)
