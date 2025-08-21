;;; init-themes.el --- theme -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package ef-themes
  :config
  (load-theme 'ef-maris-light :no-confirm))

(use-package rainbow-mode
  :commands rainbow-mode)

(provide 'init-themes)

;;; init-themes.el ends here
