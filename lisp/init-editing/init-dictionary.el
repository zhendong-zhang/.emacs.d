(use-package fanyi
  :bind
  ("M-s d" . fanyi-dwim))

(use-package acm
  :after (lsp-bridge)
  :ensure nil
  :bind
  ("M-s t" . lsp-bridge-toggle-english-helper))

(use-package corfu-english-helper
  :github "manateelazycat/corfu-english-helper"
  :after (corfu)
  :bind
  ("M-s t" . toggle-corfu-english-helper))

(provide 'init-dictionary)
