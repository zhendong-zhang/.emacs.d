;; emacs -q -l ~/.emacs.d/init-test.el

(require 'package)
(package-initialize)
(require 'use-package)

;; test code
;; (use-package vertico
;;   :config
;;   (vertico-mode t))
;; (use-package projectile
;;   :config
;;   (projectile-mode))

(provide 'init-test)
