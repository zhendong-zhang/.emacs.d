;;; init-projectile.el --- 项目管理 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package projectile
  :commands (projectile-project-root)
  :functions projectile-current-project-files
  :custom
  (projectile-mode-line-prefix " P")
  (projectile-require-project-root nil)
  (projectile-use-git-grep t)
  (projectile-indexing-method 'hybrid)
  :bind
  (("M-g f" . projectile-find-file)
   ("M-g P" . projectile-switch-open-project)
   :map projectile-mode-map
   ("C-c p" . projectile-command-map))
  :config
  (defun check-project-encoding ()
    "检查当前project下所有文件的编码是否一致"
    (interactive)
    (let ((project-dir (projectile-project-root))
          (files (projectile-current-project-files))
          (encodings (make-hash-table :test 'equal)))
      (dolist (file files)
        (setq file (concat project-dir file))
        (when (file-regular-p file)
          (with-temp-buffer
            (insert-file-contents file)
            (let ((coding (symbol-name buffer-file-coding-system)))
              (unless (equal coding "no-conversion")
                (puthash file coding encodings)
                (message "文件: %s — 编码: %s" file coding))))))
      (let ((unique-encodings (cl-reduce (lambda (acc x) (cl-adjoin x acc :test 'equal))
                                         (hash-table-values encodings) :initial-value '())))
        (if (> (length unique-encodings) 1)
            (message "发现多种编码: %s" (mapconcat 'identity unique-encodings ", "))
          (message "所有文件编码一致: %s" (car unique-encodings))))))

  (defun correct-project-encoding (to-coding)
    "将当前project下所有文件的编码设置为`to-coding'"
    (interactive (list (read-buffer-file-coding-system)))
    (let ((inhibit-message t)               ; 禁止消息
          (enable-local-variables nil)      ; 禁止本地变量
          (find-file-hook nil)              ; 禁止find-file-hook
          (after-insert-file-functions nil) ; 禁止after-insert-file-functions
          (project-dir (projectile-project-root))
          (files (projectile-current-project-files)))
      (dolist (file files)
        (setq file (concat project-dir file))
        (when (file-regular-p file)
          (let ((buffer (find-file-noselect file)))
            (with-current-buffer buffer
              (let ((coding (symbol-name buffer-file-coding-system)))
                (unless (or (equal coding "no-conversion")
                            (equal coding (symbol-name to-coding)))
                  (set-buffer-file-coding-system to-coding)
                  (message "文件: %s — 原编码: %s" file coding)
                  (save-buffer)))))))))

  (cond
   ((executable-find "fd")
    (setq-default projectile-generic-command "fd . -0 --type f --color=never"))
   ((executable-find "rg")
    (setq projectile-generic-command "rg -0 --files --follow --color=never --hidden")))

  (when (equal system-type 'windows-nt)
    (setq projectile-git-submodule-command nil))

  (projectile-mode))

(use-package ibuffer-projectile
  :after (ibuffer projectile)
  :init
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                project-relative-file)))
  :functions (ibuffer-projectile-set-filter-groups ibuffer-do-sort-by-filename/process)
  :hook
  (ibuffer . (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'filename/process)
                 (ibuffer-do-sort-by-filename/process)))))

(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package project
  :defer-incrementally t
  :config
  (add-to-list 'project-find-functions 'projectile-project-find-function))

(provide 'init-projectile)

;;; init-projectile.el ends here
