;; insert links form clipboard.
(use-package org-cliplink)

(defvar my-org-directory (expand-file-name ".notes/" user-emacs-directory))

(use-package org
  :bind
  (("C-c c" . org-capture)
   ("C-c a" . org-agenda)
   ("C-c l" . org-store-link))
  :init
  (setq org-log-done 'time
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-catch-invisible-edits 'smart
        org-tags-column -80
        org-fast-tag-selection-single-key 'expert
        org-use-fast-todo-selection 'expert
        org-link-file-path-type 'relative
        org-support-shift-select t
        org-directory my-org-directory
        org-default-notes-file (concat my-org-directory "gtd.org")
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "HANGUP(h@/!)" "|" "DONE(d!)" "CANCEL(c@/@)"))
        org-todo-keyword-faces '(("HANGUP" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t))
        org-confirm-babel-evaluate nil
        org-adapt-indentation nil)
  :config
  (use-package org-clock :ensure nil
    :init
    (setq org-clock-persist t)
    (setq org-clock-in-resume t)
    ;; Save state changes in the LOGBOOK drawer
    (setq org-log-into-drawer t)
    (setq org-clock-out-remove-zero-time-clocks t)
    :config
    ;; Save the running clock and all clock history when exiting Emacs, load it on startup
    (org-clock-persistence-insinuate))

  (use-package org-agenda :ensure nil
    :init
    (setq org-agenda-files `(,my-org-directory)
          org-agenda-compact-blocks t
          org-agenda-sticky t
          org-agenda-window-setup 'other-window))

  (use-package org-capture :ensure nil
    :config
    (setq org-capture-templates
          `(("t" "todo" entry (file "")
             "* TODO %?\n%U\n" :clock-resume t)
            ("n" "note" entry (file "")
             "* %? :NOTE:\n%U\n%a\n" :clock-resume t))))

  (use-package org-refile :ensure nil
    :init
    ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
    (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
          org-refile-use-outline-path t
          org-outline-path-complete-in-steps nil
          org-refile-allow-creating-parent-nodes 'confirm))

  (use-package org-pomodoro
    :bind
    (:map org-agenda-mode-map
          ("P" . org-pomodoro)))

  (use-package org-preview-html :diminish)
  )

(provide 'init-org-mode)
