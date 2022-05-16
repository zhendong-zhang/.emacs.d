(use-package projectile
  :demand
  :custom
  (projectile-mode-line-prefix " P")
  (projectile-require-project-root nil)
  (projectile-use-git-grep t)
  (projectile-indexing-method 'hybrid)
  :bind
  (("M-g f" . projectile-find-file)
   ("M-g P" . projectile-switch-open-project))
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

(defun projectile-project-find-function (dir)
  (let* ((root (projectile-project-root dir)))
    (and root (cons 'transient root))))

(use-package project
  :config
  (add-to-list 'project-find-functions 'projectile-project-find-function))

(provide 'init-projectile)
