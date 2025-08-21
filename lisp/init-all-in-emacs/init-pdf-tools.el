;;; init-pdf-tools.el --- pdf阅读器 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package pdf-tools
  :commands pdf-view-midnight-minor-mode
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :bind
  (:map pdf-view-mode-map
        ("o" . pdf-outline))
  :config
  (defun pdf-view-setup ()
    (when (equal 'pdf-view-mode major-mode)
      (pdf-view-midnight-minor-mode t)
      (display-line-numbers-mode -1)
      (when (and (featurep 'linum-relative)
                 (bound-and-true-p linum-relative-mode))
        (eval-when-compile
          (declare-function linum-relative-mode "linum-relative"))
        (linum-relative-mode -1))))
  (add-hook 'after-change-major-mode-hook 'pdf-view-setup 2)
  (use-package saveplace-pdf-view)
  (require 'pdf-outline))

(use-package org-pdftools
  :defer-incrementally t
  :hook
  (org-mode . org-pdftools-setup-link))

(provide 'init-pdf-tools)

;;; init-pdf-tools.el ends here
