(setq-default buffers-menu-max-size 30
              bookmark-default-file (expand-file-name ".bookmarks.el" user-emacs-directory)
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
              visible-bell nil
              view-read-only t
              tab-width 4
              next-line-add-newlines nil
              warning-suppress-types '((comp)))

(fset 'yes-or-no-p 'y-or-n-p)

;; recentf
(use-package recentf
  :demand
  :bind
  ("M-g r" . recentf-open-files)
  :init
  (setq recentf-exclude '("/tmp/"
                          "/ssh:"
                          "/sudo:"
                          "autoloads.el$")
        recentf-max-saved-items 1000
        recentf-keep '(file-remote-p file-readable-p))
  :config
  (use-package sync-recentf
    :init
    (setq recentf-auto-cleanup 60))
  (recentf-mode 1))

(use-package unfill)
;; type text replaces the selection
(delete-selection-mode t)
;; many commands will change their behavior, such as undo changes within current region.
(transient-mark-mode t)
;; auto revert when disk files changed.
(use-package autorevert
  :init
  (setq global-auto-revert-non-file-buffers t
        auto-revert-verbose nil)
  :config
  (global-auto-revert-mode))

;; When you visit a file, point goes to the last place where it was when you previously visited the same file.
(save-place-mode 1)
(savehist-mode 1)
(show-paren-mode 1)
(global-display-fill-column-indicator-mode 1)
(midnight-mode 1)

;; store all backup and autosave files in the tmp dir
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(use-package browse-kill-ring
  :init
  (setq browse-kill-ring-display-duplicates nil)
  (setq browse-kill-ring-show-preview nil)
  :config
  (browse-kill-ring-default-keybindings))

(use-package auto-highlight-symbol
  :diminish
  :init
  (setq ahs-default-range 'ahs-range-whole-buffer)
  :bind
  (:map auto-highlight-symbol-mode-map
        ("M-<left>" . nil)
        ("M-<right>" . nil)
        ("M-S-<left>" . nil)
        ("M-S-<right>" . nil)
        ("M--" . nil)
        ("C-x C-'" . nil)
        ("M-p" . ahs-backward)
        ("M-n" . ahs-forward))
  :hook
  (after-init . global-auto-highlight-symbol-mode))

(use-package whitespace-cleanup-mode
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

(use-package page-break-lines
  :diminish
  :config
  (global-page-break-lines-mode))

(use-package goto-chg
  :bind
  (("C--" . goto-last-change)
   ("C-_" . goto-last-change-reverse)))

(use-package expand-region
  :bind
  ("C-=" . er/expand-region))

(use-package iedit
  :bind
  ("C-'" . iedit-mode))

(use-package avy
  :init
  (setq avy-all-windows nil
        avy-case-fold-search nil)
  :bind
  (("M-g ;" . avy-goto-word-1)
   ("M-g :" . avy-goto-subword-1)
   ("C-;" . avy-goto-word-1)
   ("C-:" . avy-goto-subword-1)
   :map isearch-mode-map
   ("C-;" . avy-isearch))
  :config
  (use-package ace-pinyin
    :diminish ace-pinyin-mode
    :config
    (ace-pinyin-global-mode 1)))

(use-package ace-link
  :bind
  ("M-g L" . ace-link)
  :config
  (ace-link-setup-default))

(use-package move-text
  :config
  (move-text-default-bindings))

(use-package undo-tree
  :diminish undo-tree-mode
  :custom
  (undo-tree-history-directory-alist
      `((".*" . ,temporary-file-directory)))
  :config
  (global-undo-tree-mode))

(use-package hungry-delete
  :diminish hungry-delete-mode
  :config
  (add-to-list 'hungry-delete-except-modes 'minibuffer-mode)
  (global-hungry-delete-mode t))

(use-package repeat
  :ensure nil
  :hook (after-init . repeat-mode)
  :custom
  (repeat-exit-key (kbd "RET")))

(use-package wgrep)

(use-package abbrev
  :diminish
  :ensure nil)

(global-set-key (kbd "C-h C-f") 'find-function)
(global-set-key (kbd "C-x C-m") 'execute-extended-command)
(global-set-key (kbd "C-h C-k") 'find-function-on-key)
(global-set-key (kbd "M-g i") 'imenu)
(global-set-key (kbd "C-<tab>") 'previous-buffer)

(provide 'init-basic-setting)
