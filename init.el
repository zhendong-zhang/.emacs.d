;; 调试用
(setq debug-on-quit nil)
(setq debug-on-error nil)
(defcustom my-debug-switch nil
  "Turn on to show debug log."
  :group 'my-private
  :type 'boolean)

(let ((minver "27.1"))
  (when (version< emacs-version minver)
    (error "Your Emacs is too old -- this config requires v%s or higher" minver)))

(defun loop-on-all-sub-directory (dir cmd)
  (when (and dir (file-directory-p dir))
    (funcall cmd dir)
    (dolist (file (directory-files dir t "^\\w+"))
      (loop-on-all-sub-directory file cmd))))

(defvar my-lisp-load-path nil)
(defun add-directory-to-path (dir)
  "Add all directories in dir to load-path."
  (loop-on-all-sub-directory dir
                             '(lambda (dir)
                                (add-to-list 'my-lisp-load-path (expand-file-name dir))))
  )
(add-directory-to-path (expand-file-name "lisp" user-emacs-directory))
(add-directory-to-path (expand-file-name "elisp" user-emacs-directory))
(setq load-path (nconc load-path my-lisp-load-path))

;; 以目录为单位加载包
(defun require-dir (dir)
  "Ask require on all files in dir."
  (let ((count 0) file-path)
    (dolist (path my-lisp-load-path)
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

(setq gc-cons-threshold (* 100 1024 1024))
(setq package-native-compile t)
(if (and (fboundp 'native-comp-available-p)
       (native-comp-available-p))
  (message "Native compilation is available")
(message "Native complation is *not* available"))
(if (functionp 'json-serialize)
  (message "Native JSON is available")
(message "Native JSON is *not* available"))

;; 配置
(let (
      ;; 加载的时候临时增大`gc-cons-threshold'以加速启动速度。
      (gc-cons-threshold most-positive-fixnum)
      ;; 清空避免加载远程文件的时候分析文件。
      (file-name-handler-alist nil))

  ;; elpa 配置
  (require 'init-elpa)

  ;; 分析耗时
  (when my-debug-switch
    (require 'init-benchmarking))

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
)

(provide 'init)

