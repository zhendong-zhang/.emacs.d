(setq-default use-short-answers t
              buffers-menu-max-size 30
              make-backup-files nil
              scroll-preserve-screen-position 'always
              show-trailing-whitespace nil
              blink-cursor-interval 0.4
              case-fold-search t
              compilation-scroll-output t
              ediff-split-window-function 'split-window-horizontally
              ediff-window-setup-function 'ediff-setup-windows-plain
              save-interprogram-paste-before-kill t
              grep-highlight-matches t
              grep-scroll-output t
              indent-tabs-mode nil
              line-spacing 0.2
              mouse-yank-at-point t
              set-mark-command-repeat-pop t
              tooltip-delay 1.5
              truncate-lines nil
              truncate-partial-width-windows nil
              ring-bell-function #'ignore
              tab-width 4
              next-line-add-newlines nil
              warning-suppress-types '((comp))
              confirm-kill-processes nil
              enable-recursive-minibuffers t
              large-file-warning-threshold nil
              ;; 大文件提速
              bidi-display-reordering nil
              bidi-inhibit-bpa t
              long-line-threshold 1000
              large-hscroll-threshold 1000
              syntax-wholeline-max 1000
              warning-minimum-level :error
              buffer-file-coding-system 'utf-8-unix)

;; recentf
(use-package recentf
  :defer-incrementally t
  :bind
  ("M-g r" . recentf-open-files)
  :init
  (setq recentf-exclude '("/tmp/"
                          "/ssh:"
                          "/sudo:"
                          "autoloads.el$"
                          no-littering-etc-directory
                          no-littering-var-directory)
        recentf-max-saved-items 1000
        recentf-keep '(file-remote-p file-readable-p))
  :config
  (use-package sync-recentf
    :defer-incrementally t
    :init
    (setq recentf-auto-cleanup 60))
  (recentf-mode 1))

(setq custom-file (no-littering-expand-etc-file-name "custom.el"))
(when (file-exists-p custom-file)
  (load custom-file))

;; store all backup and autosave files in the var dir
(setq backup-directory-alist
      `((".*" . ,(no-littering-expand-var-file-name "backup/"))))
(setq auto-save-file-name-transforms
      `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

(use-package unfill :defer-incrementally t)
;; type text replaces the selection
(delete-selection-mode t)
;; many commands will change their behavior, such as undo changes within current region.
(transient-mark-mode t)
;; auto revert when disk files changed.
(use-package autorevert
  :defer-incrementally t
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(save-place-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
;(global-display-fill-column-indicator-mode 1)
(midnight-mode 1)

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package browse-kill-ring
  :defer-incrementally t
  :init
  (setq browse-kill-ring-display-duplicates nil)
  (setq browse-kill-ring-show-preview nil)
  :config
  (browse-kill-ring-default-keybindings))

(use-package whitespace-cleanup-mode
  :defer-incrementally t
  :diminish
  :config
  (global-whitespace-cleanup-mode t))

;; Cut/copy the current line if no region is active
(use-package whole-line-or-region
  :diminish whole-line-or-region-local-mode
  :bind
  (:map whole-line-or-region-local-mode-map
        ([remap comment-dwim] . nil))
  :config
  (whole-line-or-region-global-mode t))

(use-package goto-chg
  :bind
  (("C--" . goto-last-change)
   ("C-_" . goto-last-change-reverse)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (add-to-list 'hungry-delete-except-modes 'minibuffer-mode)
  (global-hungry-delete-mode t))

(use-package undohist
  :defer-incrementally t
  :config
  (add-to-list 'undohist-ignored-files "COMMIT_EDITMSG")
  (undohist-initialize))
(when (version<= "28.1" emacs-version)
  (use-package vundo :defer-incrementally t))

(use-package repeat
  :defer-incrementally t
  :ensure nil
  :custom
  (repeat-exit-key (kbd "RET"))
  :config
  (when (fboundp 'repeat-mode)
    (repeat-mode)))

(use-package wgrep
  :after grep
  :bind
  (:map grep-mode-map
        ("C-x C-q" . wgrep-change-to-wgrep-mode)))

(use-package abbrev
  :diminish
  :ensure nil)

(use-package mwim
  :bind
  ([remap move-beginning-of-line] . mwim-beginning)
  ([remap move-end-of-line] . mwim-end))

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "M-g i") 'imenu)

(provide 'init-basic-setting)
