;;; init-completion.el --- 补全相关配置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :custom
  (orderless-component-separator "[ |]+")
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp)))

(use-package pinyinlib
  :after orderless
  :functions orderless-regexp pinyinlib-build-regexp-string
  :preface
  (defun orderless-regexp-pinyin (str)
    "Match COMPONENT as a pinyin regex."
    (orderless-regexp (pinyinlib-build-regexp-string str)))
  :config
  (add-to-list 'orderless-matching-styles 'orderless-regexp-pinyin))

(use-package vertico
  :functions vertico--remote-p
  :custom
  (vertico-cycle t)
  (vertico-sort-function 'vertico-sort-history-length-alpha)
  :hook (after-init . vertico-mode)
  :config
  (use-package vertico-sort :ensure nil)

  (use-package vertico-directory
    :ensure nil
    :bind (:map vertico-map
                ("RET" . vertico-directory-enter)
                ("DEL" . vertico-directory-delete-char)
                ("M-DEL" . vertico-directory-delete-word))
    ;; Tidy shadowed file names
    :hook (rfn-eshadow-update-overlay . vertico-directory-tidy))

  (use-package vertico-quick
    :ensure nil
    :bind (:map vertico-map
                ([remap avy-goto-word-1] . vertico-quick-insert)))

  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-category-overrides '((file (styles basic-remote partial-completion)))))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package consult
  :custom
  (consult-async-refresh-delay 0.5)
  (consult-async-min-input 1)
  :bind
  (("M-g b" . consult-buffer)
   ("M-g s" . consult-line)
   ("M-g o" . consult-ripgrep)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-bookmark)
   ("M-g l" . consult-locate)
   ([remap recentf-open-files] . consult-recent-file)
   ([remap imenu] . consult-imenu)
   ("M-s f" . consult-focus-lines)
   ("M-s k" . consult-keep-lines))
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep :preview-key "C-<return>"
   consult-bookmark consult-recent-file :preview-key "C-<return>")
  (when (and (equal system-type 'windows-nt) (executable-find "es"))
    (setq consult-locate-args "es -sort date-modified-descending"))
  (require 'consult-imenu))

(use-package embark
  :defer-incrementally t
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings)
   :map embark-region-map
   ("=" . quick-calc)
   :map embark-general-map
   ("S" . nil)
   ("C" . embark-collect))
  :init
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package embark-consult
  :after (embark consult)
  :demand t
  :bind
  (:map embark-general-map
        ("S" . embark-consult-search-map))
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)

;;; init-completion.el ends here
