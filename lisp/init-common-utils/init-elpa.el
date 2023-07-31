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

(defun install-package-from-github (package repo)
  (unless (package-installed-p package)
    (require 'init-proxy)
    (with-proxy (quelpa `(,package :fetcher github :repo ,repo :files ("*"))))))

(require 'package)
;; 国内elpa源
(setq package-archives '(("melpa" . "https://mirrors.163.com/elpa/melpa/")
                         ;; ("melpa-stable" . "https://mirrors.163.com/elpa/melpa-stable/")
                         ("gnu" . "https://mirrors.163.com/elpa/gnu/")))
(package-initialize 'noactivate)
(unless (file-exists-p package-user-dir)
  (make-directory package-user-dir t))
(let ((default-directory package-user-dir))
  (normal-top-level-add-subdirs-to-load-path))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; (setq use-package-always-demand t)
(require 'use-package-ensure)

(use-package diminish)
(use-package no-littering)

(use-package quelpa
  :commands (quelpa quelpa-upgrade)
  :init
  (setq quelpa-checkout-melpa-p nil)
  (setq quelpa-dir (no-littering-expand-var-file-name "quelpa")))

;; from doom-emacs
(defvar incremental-packages-list '()
  "A list of packages to load incrementally after startup. Any large packages
  here may cause noticeable pauses, so it's recommended you break them up into
  sub-packages. For example, `org' is comprised of many packages, and can be
  broken up into:

    (load-packages-incrementally
     '(calendar find-func format-spec org-macs org-compat
       org-faces org-entities org-list org-pcomplete org-src
       org-footnote org-macro ob org org-clock org-agenda
       org-capture))")

(defvar incremental-first-idle-timer 3.0
  "How long (in idle seconds) until incremental loading starts.

 Set this to nil to disable incremental loading.")

(defvar incremental-idle-timer 2.0
  "How long (in idle seconds) in between incrementally loading packages.")

(defvar incremental-load-immediately (daemonp)
  "If non-nil, load all incrementally deferred packages immediately at startup.")

(defun load-packages-incrementally (packages &optional now)
  "Registers PACKAGES to be loaded incrementally.

  If NOW is non-nil, load PACKAGES incrementally, in `incremental-idle-timer'
  intervals."
  (if (not now)
      (setq incremental-packages-list (append incremental-packages-list packages))
    (when packages
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
                      (require req))
                    t)
                  (push req packages))
            (error
             (message "Failed to load %S package incrementally, because: %s"
                      req e))))
        (if (not packages)
            (message "Finished incremental loading")
          (run-with-idle-timer incremental-idle-timer
                               nil #'load-packages-incrementally
                               packages t)
          (setq packages nil))))))

(defun start-load-packages-incrementally ()
  "Begin incrementally loading packages in `incremental-packages-list'.

If this is a daemon session, load them all immediately instead."
  (if incremental-load-immediately
      (mapc #'require incremental-packages-list)
    (when (numberp incremental-first-idle-timer)
      (run-with-idle-timer incremental-first-idle-timer
                           nil #'load-packages-incrementally
                           incremental-packages-list t))))
(add-hook 'emacs-startup-hook #'start-load-packages-incrementally)

(push :defer-incrementally use-package-deferring-keywords)
(setq use-package-keywords
      (use-package-list-insert :defer-incrementally use-package-keywords :after))

(defalias 'use-package-normalize/:defer-incrementally #'use-package-normalize-symlist)
(defun use-package-handler/:defer-incrementally (name _keyword _arg rest state)
  (use-package-concat
   `((load-packages-incrementally
      ',(if (equal _arg '(t))
            (list name)
          (append _arg (list name)))))
   (use-package-process-keywords name rest state)))

(defun require-package (package &optional min-version no-refresh)
  "Install given PACKAGE, optionally requiring MIN-VERSION.
If NO-REFRESH is non-nil, the available package lists will not be
re-downloaded in order to locate PACKAGE."
  (if (package-installed-p package min-version)
      (let* ((desc (cadr (assq package package-alist)))
             (pkg-dir (if desc (package-desc-dir desc))))
        (require package (concat pkg-dir "/" (symbol-name package) ".el")))
    (let* ((known (cdr (assoc package package-archive-contents)))
           (best (car (sort known (lambda (a b)
                                    (version-list-<= (package-desc-version b)
                                                     (package-desc-version a)))))))
      (if (and best (version-list-<= min-version (package-desc-version best)))
          (unless (package-installed-p best)
            (progn
              (package-download-transaction
               (package-compute-transaction (list best) (package-desc-reqs best)))
              (package--quickstart-maybe-refresh)))
        (if no-refresh
            (error "No version of %s >= %S is available" package min-version)
          (package-refresh-contents)
          (require-package package min-version t)))
      (package-installed-p package min-version))))

(setq use-package-keywords
      (use-package-list-insert :min-version use-package-keywords :pin))
(defun use-package-normalize/:min-version (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label arg)
        (cond
         ((stringp arg)
          (mapcar #'(lambda (str) (string-to-number str))
                  (split-string arg "\\.")))
         ((use-package-non-nil-symbolp arg)
          (mapcar #'(lambda (str) (string-to-number str))
                  (split-string (symbol-name arg) "\\.")))
         (t
          (use-package-error
           ":min-version wants a version number"))))))
(defun use-package-handler/:min-version (name _keyword _arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (min-versin-form `(require-package ',name ',_arg)))
    (if (bound-and-true-p byte-compile-current-file)
        (eval min-versin-form)              ; Eval when byte-compiling,
      (push min-versin-form body))          ; or else wait until runtime.
    body))

(setq use-package-keywords
      (use-package-list-insert :github use-package-keywords :ensure))
(defun use-package-normalize/:github (_name keyword args)
  (use-package-only-one (symbol-name keyword) args
    #'(lambda (_label arg)
        (cond
         ((stringp arg)
          arg)
         ((use-package-non-nil-symbolp arg)
          (symbol-name arg))
         (t
          (use-package-error
           ":github wants a short url(string)"))))))
(defun use-package-handler/:github (name _keyword _arg rest state)
  (let ((body (use-package-process-keywords name rest state))
        (github-form `(install-package-from-github ',name ',_arg)))
    (if (bound-and-true-p byte-compile-current-file)
        (eval github-form)              ; Eval when byte-compiling,
      (push github-form body))          ; or else wait until runtime.
    body))

(provide 'init-elpa)
