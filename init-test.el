;; emacs -q -l ~/.emacs.d/init-test.el

(require 'package)
;; 国内elpa源
(setq package-archives '(("melpa" . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "http://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/gnu/")))
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir t))
(package-initialize)
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)

;; test code
;; (use-package vertico
;;   :config
;;   (vertico-mode t))
;; (use-package projectile
;;   :config
;;   (projectile-mode))

(provide 'init-test)
