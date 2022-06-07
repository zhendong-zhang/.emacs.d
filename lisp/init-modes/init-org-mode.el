;; insert links form clipboard.
(use-package org-cliplink)

(defvar my-org-directory "~/notes")

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
        org-default-notes-file (concat my-org-directory "/gtd.org")
        org-todo-keywords
        '((sequence "TODO(t)" "DOING(i!)" "HANGUP(h@/!)" "|" "DONE(d!)" "CANCEL(c@/@)"))
        org-todo-keyword-faces '(("HANGUP" . warning))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t))
        org-confirm-babel-evaluate nil
        org-adapt-indentation nil
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)
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
          org-agenda-window-setup 'other-window
          org-agenda-todo-ignore-with-date t
          org-agenda-show-all-dates nil
          org-agenda-show-future-repeats 'next))

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

  ;; RESET_CHECK_BOXES
  (setq org-default-properties (cons "RESET_CHECK_BOXES" org-default-properties))
  (defun my-org-reset-checkbox-state-maybe ()
    "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
    (interactive "*")
    (if (org-entry-get (point) "RESET_CHECK_BOXES")
        (org-reset-checkbox-state-subtree)))

  (defun my-org-reset-checkbox-when-done ()
    (when (member org-state org-done-keywords)
      (my-org-reset-checkbox-state-maybe)))

  (add-hook 'org-after-todo-state-change-hook 'my-org-reset-checkbox-when-done)

  ;; RESET_SUBTASKS
  (setq org-default-properties (cons "RESET_SUBTASKS" org-default-properties))
  (defun my-org-reset-subtask-state-subtree ()
    "Reset all subtasks in an entry subtree."
    (interactive "*")
    (if (org-before-first-heading-p)
        (error "Not inside a tree")
      (save-excursion
        (save-restriction
          (org-narrow-to-subtree)
          (org-show-subtree)
          (goto-char (point-min))
          (beginning-of-line 2)
          (narrow-to-region (point) (point-max))
          (org-map-entries
           '(when (member (org-get-todo-state) org-done-keywords)
              (org-todo (car org-todo-keywords))))
          ))))

  (defun my-org-reset-subtask-state-maybe ()
    "Reset all subtasks in an entry if the `RESET_SUBTASKS' property is set"
    (interactive "*")
    (if (org-entry-get (point) "RESET_SUBTASKS")
        (my-org-reset-subtask-state-subtree)))

  (defun my-org-reset-subtask-when-done ()
    (when (member org-state org-done-keywords)
      (my-org-reset-subtask-state-maybe)))

  (add-hook 'org-after-todo-state-change-hook 'my-org-reset-subtask-when-done)

   ;; SUMMARY_SUBTASKS
  (setq org-default-properties (cons "SUMMARY_SUBTASKS" org-default-properties))
  (defun my-org-summary-subtask (n-done n-not-done)
  "Switch entry to DONE when the `SUMMARY_SUBTASKS' property is set and all subentries are done."
  (when (and (org-entry-get (point) "SUMMARY_SUBTASKS")
             (= n-not-done 0))
        (let (org-log-done org-log-states)
          (org-todo (car org-done-keywords)))))

  (add-hook 'org-after-todo-statistics-hook #'my-org-summary-subtask)
  )

(provide 'init-org-mode)
