(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  :init
  (require 'corfu-info)
  (require 'corfu-history)
  (global-corfu-mode)
  (corfu-history-mode t))

(provide 'init-corfu)
