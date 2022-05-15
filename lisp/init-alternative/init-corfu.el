(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles . (partial-completion))))))

(use-package corfu
  :custom
  (corfu-auto t)
  (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
  :init
  (setq tab-always-indent 'complete)
  (global-corfu-mode))

(provide 'init-corfu)
