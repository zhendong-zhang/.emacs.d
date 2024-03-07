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
      (when (featurep 'linum-relative)
        (linum-relative-mode -1))))
  (add-hook 'after-change-major-mode-hook 'pdf-view-setup 2)
  (use-package saveplace-pdf-view)
  (require 'pdf-outline))

(use-package org-pdftools
  :defer-incrementally t
  :hook
  (org-mode . org-pdftools-setup-link))

(provide 'init-pdf-tools)
