;;; init-flyspell.el --- 拼写检查 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package text-mode :ensure nil
  :defer t
  :config
  (require 'ispell)
  (defvar ispell-program-name)
  (unless (executable-find ispell-program-name)
    (setq text-mode-ispell-word-completion nil)))

(use-package flyspell
  :commands (flyspell-mode)
  :bind
  (:map flyspell-mode-map
        ("C-;" . nil))
  :config
  (require 'ispell))

(provide 'init-flyspell)

;;; init-flyspell.el ends here
