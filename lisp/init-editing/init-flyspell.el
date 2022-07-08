(use-package flyspell
  :defer t
  :commands (flyspell-mode)
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil))
  :config
  (require 'ispell))

(provide 'init-flyspell)
