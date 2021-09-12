(use-package projectile
  :demand
  :init
  (setq projectile-require-project-root nil)
  (setq projectile-use-git-grep t)
  (setq projectile-indexing-method 'hybrid)
  :bind
  ("M-g f" . projectile-find-file)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode))

(use-package ibuffer-projectile
  :init
  (setq ibuffer-formats
        '((mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                project-relative-file)))
  :functions (ibuffer-projectile-set-filter-groups ibuffer-do-sort-by-filename/process)
  :hook
  (ibuffer . (lambda ()
               (ibuffer-projectile-set-filter-groups)
               (unless (eq ibuffer-sorting-mode 'filename/process)
                 (ibuffer-do-sort-by-filename/process)))))

(provide 'init-projectile)