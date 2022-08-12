(defvar my-org-directory "~/notes")

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :init
  (setq org-log-done 'time
        org-log-into-drawer t
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

  (add-hook 'org-after-todo-statistics-hook #'my-org-summary-subtask))

;; insert links form clipboard.
(use-package org-cliplink
  :bind
  ("C-c l" . org-store-link))

(use-package org-refile
  :defer t
  :ensure nil
  :init
  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-capture :ensure nil
  :bind
  ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        `(("t" "todo" entry (file "")
           "* TODO %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t))))

(use-package org-agenda :ensure nil
  :bind
  ("C-c a" . org-agenda)
  :init
  (setq org-agenda-files `(,my-org-directory)
        org-agenda-compact-blocks t
        org-agenda-sticky t
        org-agenda-window-setup 'other-window
        org-agenda-todo-ignore-with-date t
        org-agenda-show-all-dates nil
        org-agenda-show-future-repeats 'next))

(use-package org-clock
  :defer t
  :ensure nil
  :init
  (setq org-clock-persist t
        org-clock-in-resume t
        org-clock-out-remove-zero-time-clocks t)
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persistence-insinuate))

(use-package org-pomodoro
  :after org-agenda
  :bind
  (:map org-agenda-mode-map
        ("P" . org-pomodoro)))

(use-package org-preview-html :after org :diminish)

(use-package org-download
  :after org
  :config
  (when is-windows-nt
    (defun yank-image-from-win-clipboard(&optional basename)
      (interactive)
      (let* ((file-name (or basename (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))))
        (shell-command (concat "powershell -command \"(get-clipboard -format image).Save(\\\"" file-name "\\\")\""))
        (insert (concat "[[file:" file-name "]] "))
        ))
    (advice-add 'org-download-screenshot :override 'yank-image-from-win-clipboard)))

(use-package org-roam
  :custom
  (org-roam-directory my-org-directory)
  (org-roam-dailies-directory my-org-directory)
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-gc-threshold most-positive-fixnum)
  (org-roam-completion-everywhere t)
  (org-roam-capture-templates '(("m" "main" plain
                                 "%?"
                                 :target (file+head "main/${slug}.org"
                                                    "#+title: ${title}\n#+date: %t\n#+filetags:\n")
                                 :immediate-finish t
                                 :unnarrowed t)
                                ("r" "reference" plain "%?"
                                 :target
                                 (file+head "reference/${title}.org" "#+title: ${title}\n")
                                 :immediate-finish t
                                 :unnarrowed t)
                                ("a" "article" plain "%?"
                                 :target
                                 (file+head "articles/${title}.org" "#+title: ${title}\n#+date: %t\n#+filetags: :article:\n")
                                 :immediate-finish t
                                 :unnarrowed t)))
  (org-roam-node-display-template (concat "${type:15} ${title:*} "
                                          (propertize "${tags:*}" 'face 'org-tag)))
  (org-roam-list-files-commands '(fd fdfind rg find))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (use-package emacsql-sqlite-builtin)
  (org-roam-db-autosync-mode)
  (add-hook 'org-roam-capture-new-node-hook (lambda()
                                              (org-roam-tag-add '("draft"))))
  (cl-defmethod org-roam-node-type ((node org-roam-node))
    "Return the TYPE of NODE."
    (condition-case nil
        (file-name-nondirectory
         (directory-file-name
          (file-name-directory
           (file-relative-name (org-roam-node-file node) org-roam-directory))))
      (error "")))
  (add-to-list 'display-buffer-alist
               '("\\*org-roam\\*"
                 (display-buffer-in-side-window)
                 (side . right)
                 (slot . 0)
                 (window-width . 0.33)
                 (window-parameters . ((no-other-window . t)
                                       (no-delete-other-windows . t)))))
  (defun org-roam-node-from-pdf ()
    (interactive)
    (let* ((file-path (org-pdftools-complete-link))
           (file-name (file-name-base file-path)))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${title}.org"
                                       ":PROPERTIES:
:ROAM_REFS: ${pdf-key}
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :pdf-key file-path)
                         :node (org-roam-node-create :title file-name)
                         :props '(:finalize find-file))))
  )
(use-package org-roam-ui :after org-roam)

(use-package oc
  :defer t
  :ensure nil
  :custom
  (org-cite-global-bibliography `(,(concat my-org-directory "/references.bib"))))
(use-package citar
  :commands (org-roam-node-from-cite)
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths `(,(concat my-org-directory "/books")))
  :config
  (defun org-roam-node-from-cite (keys-entries)
    (interactive (list (citar-select-ref :multiple nil :rebuild-cache t)))
    (let ((title (citar--format-entry-no-widths (cdr keys-entries)
                                                "${author editor} :: ${title}")))
      (org-roam-capture- :templates
                         '(("r" "reference" plain "%?" :if-new
                            (file+head "reference/${citekey}.org"
                                       ":PROPERTIES:
:ROAM_REFS: [cite:@${citekey}]
:END:
#+title: ${title}\n")
                            :immediate-finish t
                            :unnarrowed t))
                         :info (list :citekey (car keys-entries))
                         :node (org-roam-node-create :title title)
                         :props '(:finalize find-file)))))

(provide 'init-org-mode)
