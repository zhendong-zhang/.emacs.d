(setq package-enable-at-startup nil)
(setq frame-resize-pixelwise t)

(when (fboundp 'startup-redirect-eln-cache)
  (startup-redirect-eln-cache
   (convert-standard-filename
      (expand-file-name  "var/eln-cache/" user-emacs-directory))))

(provide 'early-init)
