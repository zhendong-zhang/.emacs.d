(require 'package)

;; 基础安装包接口
(defun require-package (package &optional min-version no-refresh)
  "Ask elpa to install given PACKAGE."
  (if (package-installed-p package min-version)
      t
    (if (or (assoc package package-archive-contents) no-refresh)
        (package-install package)
      (progn
        (package-refresh-contents)
        (require-package package min-version t)))))


;; melpa
;; (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; 国内elpa源
(setq package-archives '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "https://mirrors.163.com/elpa/gnu/")
	                     ("org" . "https://mirrors.163.com/elpa/org/")))

(package-initialize)

(require 'benchmark)
(defun find-fastest-mirror-for-me ()
  (interactive)
  (pp
   (seq-sort-by
    #'cdr
    #'<
    (mapcar
     (lambda (pair)
       (let ((name (car pair))
             (url  (cdr pair)))
         (cons
          name
          (benchmark-elapse
            (url-copy-file
             (concat url "archive-contents")
             null-device
             'OK-IF-ALREADY-EXISTS)))))
     '((163         . "https://mirrors.163.com/elpa/melpa/")
       (emacs-china . "https://elpa.emacs-china.org/melpa/")
       (sjtu        . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
       (tencent     . "https://mirrors.cloud.tencent.com/elpa/melpa/")
       (tuna        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))

(provide 'init-elpa)
