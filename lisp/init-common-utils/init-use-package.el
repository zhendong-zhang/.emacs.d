(require 'init-elpa)
(require-package 'use-package)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)

;; example
;; (use-package color-moccur
;;   :diminish
;;   :hook ((prog-mode . ace-jump-mode)
;;          (text-mode . ace-jump-mode))
;;   :bind (("M-s O" . moccur)
;;          :map isearch-mode-map
;;          ("M-o" . isearch-moccur)
;;          ("M-O" . isearch-moccur-all))
;;   :init
;;   (setq isearch-lazy-highlight t)
;;   :config
;;   (use-package moccur-edit))

(provide 'init-use-package)
