;;; init-gtd.el --- gtd支持 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package org-capture :ensure nil
  :bind
  ("C-c c" . org-capture)
  :config
  (setq org-capture-templates
        `(("i" "important" entry (file "")
           "* TODO [#A] %?\n%U\n" :clock-resume t)
          ("t" "todo" entry (file "")
           "* TODO [#B] %?\n%U\n" :clock-resume t)
          ("n" "note" entry (file "")
           "* %? :NOTE:\n%U\n%a\n" :clock-resume t))))

(use-package org-agenda :ensure nil
  :bind
  ("C-c a" . org-agenda)
  :custom
  (org-agenda-files `(,my-org-directory))
  (org-agenda-compact-blocks t)
  (org-agenda-sticky t)
  (org-agenda-span 'day)
  (org-agenda-include-diary t)
  (org-agenda-window-setup 'current-window)
  (org-agenda-show-all-dates nil)
  (org-agenda-show-future-repeats 'next)
  (org-agenda-start-with-log-mode t)
  (org-agenda-include-deadlines t)
  (org-agenda-block-separator nil)
  (org-agenda-custom-commands `(("n" "Notes" tags "NOTE"
                                 ((org-agenda-overriding-header "Notes")))))
  (org-agenda-time-grid '((daily today require-timed) nil " ----- " ""))
  (org-agenda-current-time-string "now ---")
  (org-agenda-log-mode-items '(closed clock state))
  :config
  (require 'org-super-agenda))

(use-package org-super-agenda
  :custom
  (org-super-agenda-groups '((:name "Today" :date today :scheduled past :scheduled today :deadline past :deadline today)
                             (:name "Next to do" :todo "NEXT")
                             (:name "Stuck Projects" :and (:todo "PROJECT" :not (:children todo)))
                             (:name "Projects" :and (:todo "PROJECT" :children todo))
                             (:name "Important" :priority "A")
                             (:name "Due Soon" :deadline future)
                             (:name "Almost Done" :effort< "0:30")
                             (:name "Inbox" :and (:tag "INBOX" :todo t))
                             (:name "Unimportant" :todo ("WAITING" "RECORD") :priority<= "B" :scheduled future)
                             (:name "Unspecified" :anything)))
  :hook
  (org-agenda-mode . org-super-agenda-mode)
  :config
  (push '("g" "GTD"
          ((agenda "" ((org-agenda-span 'day)))
           (alltodo "" ((org-agenda-overriding-header "")
                        (org-agenda-todo-ignore-with-date t)
                        (org-super-agenda-groups
                         (cdr org-super-agenda-groups))))))
        org-agenda-custom-commands))

(use-package org-habit :ensure nil
  :after org-agenda
  :config
  (add-to-list 'org-modules 'org-habit))

(use-package org-pomodoro
  :after org-agenda
  :bind
  (:map org-agenda-mode-map
        ("P" . org-pomodoro)))

(use-package org-refile :ensure nil
  :defer t
  :init
  ;; Targets include this file and any file contributing to the agenda - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5) (org-agenda-files :maxlevel . 5))
        org-refile-use-outline-path t
        org-outline-path-complete-in-steps nil
        org-refile-allow-creating-parent-nodes 'confirm))

(use-package org-clock :ensure nil
  :defer t
  :custom
  (org-clock-persist t)
  (org-clock-in-resume t)
  (org-clock-out-remove-zero-time-clocks t)
  (org-clock-mode-line-total 'today)
  :config
  ;; Save the running clock and all clock history when exiting Emacs, load it on startup
  (org-clock-persistence-insinuate))

(provide 'init-gtd)

;;; init-gtd.el ends here
