(use-package flyspell
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil))
  :config
  (require 'ispell))

(provide 'init-flyspell)
