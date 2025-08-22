;;; init-locales.el --- 本地化 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

;; set coding config, last is highest priority.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Recognize-Coding.html#Recognize-Coding
(prefer-coding-system 'cp950)
(prefer-coding-system 'gb2312)
(prefer-coding-system 'cp936)
(prefer-coding-system 'gb18030)
(prefer-coding-system 'utf-16)
(prefer-coding-system 'utf-8)

;; 同时设置`buffer-file-coding-system', `default-file-name-coding-system',
;; `default-terminal-coding-system', `default-keyboard-coding-system'
(set-language-environment "UTF-8")

(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(setq system-time-locale "C")

(when (equal system-type 'windows-nt)
  ;; 解决windows emacs调用外部进程的中文乱码问题
  (add-to-list 'process-coding-system-alist '("cmdproxy" utf-8 . gbk)))

(provide 'init-locales)

;;; init-locales.el ends here
