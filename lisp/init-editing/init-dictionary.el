(use-package fanyi
  :bind
  ("M-s d" . fanyi-dwim))

(use-package corfu-english-helper
  :load-path "site-lisp/corfu-english-helper"
  :init
  (require 'corfu-english-helper)
  :bind
  ("M-s t" . toggle-corfu-english-helper))

(provide 'init-dictionary)
