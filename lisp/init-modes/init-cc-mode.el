(use-package cc-mode
  :config
  (add-to-list 'c-default-style '(c-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

  (use-package find-file
    :config
    (add-to-list 'cc-search-directories "/usr/include/c++/*"))
  )

(provide 'init-cc-mode)
