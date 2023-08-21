(use-package persp-mode
  :after tab-bar
  :demand
  :diminish
  :defines (recentf-exclude)
  :functions (recentf-include-p set-persp-parameter persp-parameter)
  :commands (get-current-persp persp-contain-buffer-p)
  :hook ((after-init . persp-mode))
  :init (setq persp-keymap-prefix (kbd "C-x p")
              persp-nil-name "default"
              persp-kill-foreign-buffer-behaviour 'kill
              persp-auto-resume-time 0.1)
  :config
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore dead, temporary, not visible or not recently buffers."
              (or (not (buffer-live-p b))
                  (string-prefix-p "*" (buffer-name b))
                  (not (or (get-buffer-window b)
                           (tab-bar-get-buffer-tab b)
                           (< (cl-position b (buffer-list)) 10))))))
  (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore temporary buffers."
              (let ((bname (file-name-nondirectory (buffer-name b))))
                (or (string-prefix-p ".newsrc" bname)
                    (string-prefix-p "magit" bname)
                    (string-prefix-p "COMMIT_EDITMSG" bname)
                    (string-prefix-p "Pfuture-Callback" bname)
                    (string-prefix-p "treemacs-persist" bname)
                    (string-match-p "\\.eln\\|\\.elc\\|\\.tar\\|\\.gz\\|\\.zip\\'" bname)
                    (string-match-p "\\.bin\\|\\.so\\|\\.dll\\|\\.exe\\'" bname)))))

  ;; Don't save persp configs in `recentf'
  (with-eval-after-load 'recentf
    (push persp-save-dir recentf-exclude)
    (add-hook 'persp-filter-save-buffers-functions
            (lambda (b)
              "Ignore buffers exclude by recentf."
              (let ((fname (buffer-file-name b)))
                (or (not fname)
                    (not (recentf-include-p fname)))))))

  ;; Eshell integration
  (persp-def-buffer-save/load
   :mode 'eshell-mode :tag-symbol 'def-eshell-buffer
   :save-vars '(major-mode default-directory))

  ;; Shell integration
  (persp-def-buffer-save/load
   :mode 'shell-mode :tag-symbol 'def-shell-buffer
   :mode-restore-function (lambda (_) (shell))
   :save-vars '(major-mode default-directory))

  (with-no-warnings
    ;; Tab-Bar-Mode integration
    (defun my-save-tabs (&rest _)
      (set-persp-parameter 'tab-bar-tabs (frameset-filter-tabs (tab-bar-tabs) nil nil t)))

    (defun my-load-tabs (&rest _)
      (tab-bar-tabs-set (persp-parameter 'tab-bar-tabs))
      (tab-bar--update-tab-bar-lines t)))

  (add-hook 'persp-before-deactivate-functions 'my-save-tabs)
  (add-hook 'persp-activated-functions 'my-load-tabs)
  (add-hook 'persp-before-save-state-to-file-functions 'my-save-tabs)
  (add-hook 'persp-after-load-state-functions 'my-load-tabs))

(provide 'init-persp)
