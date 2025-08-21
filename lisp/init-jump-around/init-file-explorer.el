;;; init-file-explorer.el --- 调用外部文件浏览器 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defun show-this-file-external ()
  "Show current file in OS explorer."
  (interactive)
  (declare-function dired-file-name-at-point "dired")
  (let ((current-file (or (buffer-file-name)
                          (expand-file-name (dired-file-name-at-point))
                          default-directory)))
    (cond
     ((executable-find "explorer.exe")
      (start-process-shell-command "explorer" nil
                                   (concat "explorer.exe /e,/select,"
                                           (file-name-nondirectory current-file))))
     (t
      (error "No browser found.")))))

(global-set-key (kbd "M-g e") 'show-this-file-external)

(provide 'init-file-explorer)

;;; init-file-explorer.el ends here
