(use-package cc-mode
  :config
  (add-to-list 'c-default-style '(c-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (use-package find-file
    :config
    (add-to-list 'cc-search-directories "/usr/include/c++/*"))
  )

;; (with-eval-after-load 'cmake-ide
;;   (when (executable-find "rdm")
;;     (require-package 'rtags)
;;     (require 'rtags))
;;   (setq cmake-ide-build-dir "build")
;;   (defadvice cmake-ide-set-compiler-flags
;;       (after newbie/append-system-include-path activate)
;;     (let ((sysroot nil))
;;       (dolist (flag flags)
;;         (when (string-match "--sysroot=.*" flag)
;;           (setq sysroot t)))
;;       (unless sysroot
;;         (setq sys-includes (append sys-includes (newbie/guess-c++-header-path)))
;;         (when (featurep 'company-c-headers)
;;           (setq company-c-headers-path-system sys-includes))))
;;     (setq newbie/system-header-cache sys-includes)
;;     (add-hook 'ff-pre-find-hook
;;               '(lambda ()(when newbie/system-header-cache
;;                  (dolist (path newbie/system-header-cache)
;;                    (add-to-list 'cc-search-directories path)))))))

;; (with-eval-after-load 'find-file
;;   (dolist (path (newbie/guess-c++-header-path))
;;     (add-to-list 'cc-search-directories path)))

(provide 'init-cc-mode)
