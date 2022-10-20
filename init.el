;; 调试用
(setq debug-on-quit nil)
(setq debug-on-error nil)
(defcustom my-debug-switch nil
  "Turn on to show debug log."
  :group 'my-private
  :type 'boolean)

;; speed up
(setq auto-mode-case-fold nil)

(defvar old-file-name-handler-alist file-name-handler-alist)
(unless (or (daemonp) noninteractive init-file-debug)
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
            (lambda ()
              "Recover file name handlers."
              (setq file-name-handler-alist
                    (delete-dups (append file-name-handler-alist
                                         old-file-name-handler-alist))))))

;; Defer garbage collection further back in the startup process
(setq gc-cons-threshold most-positive-fixnum)
(add-hook 'emacs-startup-hook
          (lambda ()
            "Recover GC values after startup."
            (setq gc-cons-threshold 800000)))

(defvar is-windows-nt (equal system-type 'windows-nt))

(let ((minver "29.0"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defun loop-on-all-sub-directory (dir cmd)
  (when (and dir (file-directory-p dir))
    (funcall cmd dir)
    (dolist (file (directory-files dir t "^\\w+"))
      (loop-on-all-sub-directory file cmd))))

(defun add-directory-to-path (dir)
  "Add subdirectories in dir to load-path."
  (add-to-list 'load-path dir t)
  (let ((default-directory dir))
    (normal-top-level-add-subdirs-to-load-path))
  )
(add-directory-to-path (expand-file-name "lisp" user-emacs-directory))

;; 以目录为单位加载包
(defun require-dir (dir)
  "Ask require on all files in dir."
  (let ((count 0) file-path)
    (dolist (path load-path)
      (when (string-equal dir (file-name-base path))
        (setq file-path path)
        (setq count (+ 1 count))))
    (if (= count 1)
        (dolist (file (directory-files file-path))
          (unless (or (file-directory-p (expand-file-name file))
                      (string-match "^[.]*#+" (file-name-base file))
                      (string-match "~+$" (expand-file-name file))
			          (string-match "^flycheck_" (file-name-base file)))
            (require (intern (file-name-base file)))))
      (error "no one or more than one dir match, do nothing for require-dir %s." dir))))

(if (and (fboundp 'native-comp-available-p)
         (native-comp-available-p))
    (message "Native compilation is available")
  (message "Native complation is *not* available"))
(if (functionp 'json-serialize)
    (message "Native JSON is available")
  (message "Native JSON is *not* available"))

;; 配置

;; 分析耗时
(when my-debug-switch
  (require 'init-benchmarking)
  (setq use-package-verbose t))

;; 公用接口或包
(require-dir "init-common-utils")
;; 替换部分内置功能以提升体验
(require-dir "init-alternative")
;; 外观相关配置
(require-dir "init-looks")
;; 项目管理
(require-dir "init-project")
;; 编辑功能相关
(require-dir "init-editing")
;; 编程通用配置
(require-dir "init-prog-common")
;; modes配置
(require-dir "init-modes")
;; 支持emacs client
(require 'init-daemon)
;; emacs替换外部应用相关
(require-dir "init-all-in-emacs")

(provide 'init)
