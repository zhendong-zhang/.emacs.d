(use-package ibuffer
  :defer-incrementally t
  :bind
  ([remap list-buffers] . ibuffer-list-buffers)
  :custom
  (ibuffer-expert t)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-display-summary nil)
  (ibuffer-filter-group-name-face 'font-lock-doc-face)
  :config
  (require 'ibuf-ext)
  (define-ibuffer-column size-h
    (:name "Size" :inline t)
    (cond
     ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
     ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
     (t (format "%8d" (buffer-size))))))

(provide 'init-ibuffer)
