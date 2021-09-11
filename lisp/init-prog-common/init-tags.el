(use-package citre
  :init
  (require 'citre-config)
  (setq citre-tags-file-global-cache-dir (expand-file-name ".tags/" user-emacs-directory)
        citre-use-project-root-when-creating-tags t
        citre-default-create-tags-file-location 'global-cache
        citre-ctags-program "ctags-universal"
        citre-prompt-language-for-ctags-command t)
  :config
  (declare-function projectile-project-root "projectile")
  (require 'projectile)
  (setq citre-project-root-function #'projectile-project-root)
  (defalias 'create-tag-file 'citre-update-this-tags-file))

(provide 'init-tags)
