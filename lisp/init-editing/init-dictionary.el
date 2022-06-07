(use-package fanyi
  :bind
  ("M-s d" . fanyi-dwim))

(use-package acm
  :after (lsp-bridge)
  :ensure nil
  :bind
  ("M-s t" . lsp-bridge-toggle-english-helper))

(provide 'init-dictionary)
