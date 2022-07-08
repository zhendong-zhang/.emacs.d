(use-package cc-mode
  :demand
  :mode
  ("\\.h\\'" . c++-mode)
  :config
  (add-to-list 'c-default-style '(c-mode . "stroustrup"))
  (add-to-list 'c-default-style '(c++-mode . "stroustrup"))

  (use-package find-file
    :config
    (add-to-list 'cc-search-directories "/usr/include/c++/*"))
  )

(provide 'init-cc-mode)
