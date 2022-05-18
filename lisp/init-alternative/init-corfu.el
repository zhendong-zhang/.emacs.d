(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  :init
  (global-corfu-mode))

(provide 'init-corfu)
