(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode)
  :config
  (defun pdf-view-setup ()
    (when (equal 'pdf-view-mode major-mode)
      (pdf-view-midnight-minor-mode t)
      (display-line-numbers-mode -1)
      (when (featurep 'linum-relative)
        (linum-relative-mode -1))))
  (add-hook 'after-change-major-mode-hook 'pdf-view-setup 2))

(use-package org-pdftools
  :hook
  (org-mode . org-pdftools-setup-link))
(use-package saveplace-pdf-view
  :after pdf-view)

(provide 'init-pdf-tools)
