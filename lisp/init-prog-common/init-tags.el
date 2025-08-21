;;; init-tags.el --- tag file -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package citre
  :after projectile
  :defer t
  :commands (citre-jump citre-update-this-tags-file)
  :defines citre-use-project-root-when-creating-tags citre-prompt-language-for-ctags-command
  :config
  (require 'citre-config)
  (setq citre-tags-file-global-cache-dir (expand-file-name ".tags/" user-emacs-directory)
        citre-use-project-root-when-creating-tags t
        citre-default-create-tags-file-location 'global-cache
        citre-ctags-program "ctags-universal"
        citre-prompt-language-for-ctags-command t)

  (eval-when-compile
    (declare-function projectile-project-root "projectile"))
  (setq citre-project-root-function #'projectile-project-root)
  (defalias 'create-tag-file 'citre-update-this-tags-file))

(provide 'init-tags)

;;; init-tags.el ends here
