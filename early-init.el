;; speed up
(setq load-prefer-newer noninteractive)
(setq package-enable-at-startup nil)
;; you need run `M-x package-quickstart-refresh' at least once
;; to generate file "package-quickstart.el'.
;; It contains the `autoload' statements for all packages.
(setq package-quickstart t)
(setq frame-resize-pixelwise t)
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5
      read-process-output-max (* 1024 1024)) ;; 1mb

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(when (featurep 'ns)
  (push '(ns-transparent-titlebar . t) default-frame-alist))
(push '(fullscreen . maximized) default-frame-alist)
(push '(font . "LXGW WenKai Mono-14") default-frame-alist)

(provide 'early-init)
