(use-package corfu
  :custom
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  (corfu-preselect-first nil) ;; Disable candidate preselection
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode)
  )

(provide 'init-corfu)
