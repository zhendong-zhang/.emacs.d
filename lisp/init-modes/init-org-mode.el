;;; init-org-mode.el --- org支持 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defvar my-org-directory "~/notes")

(use-package org
  :mode ("\\.org\\'" . org-mode)
  :defines org-state
  :functions org-entry-get org-reset-checkbox-state-subtree
  :preface
  (defun my-org-reset-checkbox-state-maybe ()
    "Reset all checkboxes in an entry if the `RESET_CHECK_BOXES' property is set"
    (interactive "*")
    (if (org-entry-get (point) "RESET_CHECK_BOXES")
        (org-reset-checkbox-state-subtree)))

  (defun my-org-reset-checkbox-when-done ()
    (when (member org-state org-done-keywords)
      (my-org-reset-checkbox-state-maybe)))

  (defun my-org-auto-load-file ()
    "Auto load file."
    (when-let* ((file (org-entry-get (point) "AUTO_LOAD_FILE")))
      (load-file file)))
  :bind
  (:map org-mode-map
        ("C-'" . nil))
  :init
  (setq org-log-done 'time
        org-log-into-drawer t
        org-edit-timestamp-down-means-later t
        org-hide-emphasis-markers t
        org-fold-catch-invisible-edits 'smart
        org-tags-column -80
        org-fast-tag-selection-single-key 'expert
        org-use-fast-todo-selection 'expert
        org-link-file-path-type 'relative
        org-support-shift-select t
        org-directory my-org-directory
        org-default-notes-file (concat my-org-directory "/gtd.org")
        ;; inbox/todo -> next -> done
        ;;            -> waiting -> next -> done
        ;;            -> someday -> next -> done
        ;;            -> cancel
        org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d!/!)")
          (sequence "PROJECT(p)" "|" "DONE(d!/!)" "CANCELLED(c@/!)")
          (sequence "WAITING(w@/!)" "SOMEDAY(s)" "|" "CANCELLED(c@/!)")
          (sequence "RECORD(r)" "|" "CANCELLED(c)"))
        org-todo-repeat-to-state "NEXT"
        org-todo-keyword-faces '(("NEXT" . warning)
                                 ("PROJECT" :inherit font-lock-string-face))
        org-priority-faces '((?A . error)
                             (?B . warning)
                             (?C . success))
        org-babel-load-languages '((emacs-lisp . t)
                                   (shell . t))
        org-confirm-babel-evaluate nil
        org-adapt-indentation nil
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-startup-with-inline-images t
        org-image-actual-width '(600))
  :config
  ;; RESET_CHECK_BOXES
  (setq org-default-properties (cons "RESET_CHECK_BOXES" org-default-properties))
  (add-hook 'org-after-todo-state-change-hook 'my-org-reset-checkbox-when-done)

  ;; AUTO_LOAD_FILE
  (setq org-default-properties (cons "AUTO_LOAD_FILE" org-default-properties))
  (add-hook 'org-mode-hook #'my-org-auto-load-file))

;; insert links form clipboard.
(use-package org-cliplink
  :bind
  ("C-c l" . org-store-link))

(use-package org-appear
  :after org
  :custom
  (org-appear-autolinks t)
  :hook (org-mode . org-appear-mode))

(use-package org-preview-html :after org :diminish :defer-incrementally t)

(use-package org-download
  :after org
  :defer-incrementally t
  :config
  (when (equal system-type 'windows-nt)
    (defun yank-image-from-win-clipboard(&optional basename)
      (interactive)
      (let* ((file-name (or basename (format-time-string "screenshot_%Y%m%d_%H%M%S.png"))))
        (shell-command (concat "powershell -command \"(get-clipboard -format image).Save(\\\"" file-name "\\\")\""))
        (insert (concat "[[file:" file-name "]] "))
        ))
    (advice-add 'org-download-screenshot :override 'yank-image-from-win-clipboard)))

(use-package org-roam
  :commands org-roam-tag-add
  :custom
  (org-roam-directory my-org-directory)
  (org-roam-dailies-directory my-org-directory)
  (org-roam-database-connector 'sqlite-builtin)
  (org-roam-db-gc-threshold most-positive-fixnum)
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
  :defer-incrementally t
  :ensure nil
  :custom
  (org-cite-global-bibliography `(,(concat my-org-directory "/references.bib"))))
(use-package citar
  :defer-incrementally t
  :after oc
  :custom
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar)
  (citar-bibliography org-cite-global-bibliography)
  (citar-library-paths `(,(concat my-org-directory "/books")))
  :preface
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

;;; init-org-mode.el ends here
