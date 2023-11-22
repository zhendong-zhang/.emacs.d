(use-package maple-translate
  :github "honmaple/emacs-maple-translate"
  :bind
  ("M-s d" . maple-translate+))

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
