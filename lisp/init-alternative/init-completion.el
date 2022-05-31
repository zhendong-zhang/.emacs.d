(use-package orderless
  :init
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil)
  :custom
  (orderless-component-separator #'orderless-escapable-split-on-space)
  (orderless-matching-styles '(orderless-initialism orderless-literal orderless-regexp))
  :config
  (defun my-orderless-dispatcher (pattern index _total)
    (cond
     ((string-suffix-p "~" pattern)
      `(orderless-flex . ,(substring pattern 0 -1)))
     ((and (= index 0) (string-suffix-p "^" pattern))
      `(orderless-initialism . ,(substring pattern 0 -1)))
     ((equal "!" pattern)
      '(orderless-literal . ""))
     ((string-prefix-p "!" pattern)
      `(orderless-without-literal . ,(substring pattern 1)))
     ))

  (setq orderless-style-dispatchers '(my-orderless-dispatcher))
  )

(use-package vertico
  :custom
  (vertico-cycle t)
  :config
  (vertico-mode t)

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
                ("C-;" . vertico-quick-insert)))

  (defun basic-remote-try-completion (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-try-completion string table pred point)))
  (defun basic-remote-all-completions (string table pred point)
    (and (vertico--remote-p string)
         (completion-basic-all-completions string table pred point)))
  (add-to-list
   'completion-styles-alist
   '(basic-remote basic-remote-try-completion basic-remote-all-completions nil))
  (setq completion-category-overrides '((file (styles basic-remote partial-completion))))
  )

(use-package marginalia
  :config
  (marginalia-mode t))

(defun my-consult-line (consult-line-function &rest rest)
  (interactive)
  (if (use-region-p)
      (let ((thing (buffer-substring-no-properties
                    (region-beginning) (region-end))))
        (deactivate-mark)
        (apply consult-line-function (list thing)))
    (apply consult-line-function
           rest)))

(use-package consult
  :init
  (advice-add #'consult-line :around #'my-consult-line '((depth . -1)))
  (setq consult-async-refresh-delay 0.5)
  :bind
  (("M-g b" . consult-buffer)
   ("M-g l" . consult-line)
   ("M-g o" . consult-ripgrep)
   ("M-g I" . consult-imenu-multi)
   ("M-g m" . consult-mark)
   ("M-g M" . consult-bookmark)
   )
  :config
  (consult-customize
   consult-ripgrep consult-git-grep consult-grep
   consult-bookmark consult-recent-file consult-xref
   consult--source-bookmark consult--source-recent-file
   consult--source-project-recent-file
   :preview-key (kbd "C-<return>"))
  )

(use-package embark
  :bind
  (("C-." . embark-act)
   ("M-." . embark-dwim)
   ("C-h B" . embark-bindings))

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
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(provide 'init-completion)
