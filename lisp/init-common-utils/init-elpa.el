(defun find-fastest-mirror-for-me ()
  (interactive)
  (require 'benchmark)
  (pp
   (seq-sort-by
    #'cdr
    #'<
    (mapcar
     (lambda (pair)
       (let ((name (car pair))
             (url  (cdr pair)))
         (cons
          name
          (benchmark-elapse
            (url-copy-file
             (concat url "archive-contents")
             null-device
             'OK-IF-ALREADY-EXISTS)))))
     '((163         . "https://mirrors.163.com/elpa/melpa/")
       (emacs-china . "https://elpa.emacs-china.org/melpa/")
       (sjtu        . "https://mirrors.sjtug.sjtu.edu.cn/emacs-elpa/melpa/")
       (tencent     . "https://mirrors.cloud.tencent.com/elpa/melpa/")
       (tuna        . "https://mirrors.tuna.tsinghua.edu.cn/elpa/melpa/"))))))

(require 'package)
;; 国内elpa源
(setq package-archives '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "https://mirrors.163.com/elpa/gnu/")
                         ("org" . "https://mirrors.163.com/elpa/org/")))
(package-initialize 'noactivate)
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir t))
(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package)
)
(require 'use-package)
(require 'use-package-ensure)
(setq use-package-always-ensure t)

(use-package diminish)
(use-package no-littering)

(use-package quelpa
  :defer t
  :init
  (setq quelpa-checkout-melpa-p nil)
  (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

(use-package quelpa-use-package
  :config
  (quelpa-use-package-activate-advice))

;; from doom-emacs
(defvar incremental-packages-list '(t)
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (load-packages-incrementally
     '(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))")

(defvar incremental-first-idle-timer 5.0
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading.")

(defvar incremental-idle-timer 0.75
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defun load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

  If NOW is non-nil, load PACKAGES incrementally, in `incremental-idle-timer'
  intervals."
  (if (not now)
      (setq incremental-packages-list (append incremental-packages-list packages ))
    (while packages
      (let* ((gc-cons-threshold most-positive-fixnum)
             (req (pop packages)))
        (unless (featurep req)
          (message "Incrementally loading %s" req)
          (condition-case-unless-debug e
              (or (while-no-input
                    ;; If `default-directory' is a directory that doesn't exist
                    ;; or is unreadable, Emacs throws up file-missing errors, so
                    ;; we set it to a directory we know exists and is readable.
                    (let ((default-directory user-emacs-directory)
                          (inhibit-message t)
                          file-name-handler-alist)
                      (require req nil t))
                    t)
                  (push req packages))
            (error
             (message "Failed to load %S package incrementally, because: %s"
                      req e)))
          (if (not packages)
              (message "Finished incremental loading")
            (run-with-idle-timer incremental-idle-timer
                                 nil #'load-packages-incrementally
                                 packages t)
            (setq packages nil)))))))

(defun start-load-packages-incrementally ()
  "Begin incrementally loading packages in `incremental-packages-list'.

If this is a daemon session, load them all immediately instead."
  (if incremental-load-immediately
      (mapc #'require (cdr incremental-packages-list))
    (when (numberp incremental-first-idle-timer)
      (run-with-idle-timer incremental-first-idle-timer
                           nil #'load-packages-incrementally
                           (cdr incremental-packages-list) t))))
(add-hook 'emacs-startup-hook #'start-load-packages-incrementally)

(push :defer-incrementally use-package-deferring-keywords)
(setq use-package-keywords
      (use-package-list-insert :defer-incrementally use-package-keywords :after))

(defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
(defun use-package-handler/:defer-incrementally (name _keyword targets rest state)
  (use-package-concat
   `((load-packages-incrementally
      ',(if (equal targets '(t))
            (list name)
          (append targets (list name)))))
   (use-package-process-keywords name rest state)))

(provide 'init-elpa)
