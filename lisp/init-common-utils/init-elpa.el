(defun find-fastest-mirror-for-me ()
  (interactive)
  (require 'benchmark)
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

(require 'package)
;; 国内elpa源
(setq package-archives '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "https://mirrors.163.com/elpa/gnu/")
                         ("org" . "https://mirrors.163.com/elpa/org/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package diminish)

(use-package quelpa
  :init
  (setq quelpa-checkout-melpa-p nil))
(use-package quelpa-use-package
  :config
  (quelpa-use-package-activate-advice))

(provide 'init-elpa)
