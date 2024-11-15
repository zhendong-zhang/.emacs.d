(defun create-compile-commands-json ()
  "For quickly use."
  (interactive)
  (declare-function projectile-project-root "projectile")
  (require 'projectile)
  (let ((project-root (projectile-project-root)))
    (cond ((and (executable-find "cmake") (file-exists-p (concat project-root "CMakeLists.txt")))
           (shell-command (concat "cd " project-root "; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 &")))
          ((and (executable-find "make") (executable-find "bear"))
           (require 'compile)
           (shell-command (concat "cd " project-root "; bear --append -- " (compilation-read-command "make") " -j8 &")))
          (t
           (with-temp-buffer
             (erase-buffer)
             (insert "-xc++\n")
             (insert "-I.")
             (write-file (concat project-root "compile_flags.txt")))))))

(defun create-dot-clang-format ()
  (interactive)
  (declare-function projectile-project-root "projectile")
  (with-temp-buffer
    (erase-buffer)
    (insert "# google 编程风格
BasedOnStyle: Google
IndentWidth: 4
# 访问说明符(public、private等)的偏移(缩进或者对齐)
AccessModifierOffset: -4")
    (write-file (concat (projectile-project-root) ".clang-format"))
    ))

(use-package dumb-jump
  :preface
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  :after (xref)
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 2)
  (setq dumb-jump-force-searcher 'ag))

(defvar use-lsp-bridge nil)
(if (not use-lsp-bridge)
    (progn
      (use-package corfu
        :custom
        (corfu-auto t)
        (corfu-cycle t)             ;; Enable cycling for `corfu-next/previous'
        (corfu-separator ?|)
        :init
        (require 'corfu-info)
        (require 'corfu-history)
        (global-corfu-mode t)
        (corfu-history-mode t))
      (use-package eglot
        :after corfu
        :hook
        ((c-mode-common
          bash-mode
          python-mode
          cmake-mode
          lua-mode
          python-mode
          web-mode) . eglot-ensure)
        :init
        (setq eglot-events-buffer-size 0))
      (use-package eglot-booster
        :github "jdtsmith/eglot-booster"
        :after eglot
        :config	(eglot-booster-mode)))
  ;; else
  (use-package posframe)
  (use-package lsp-bridge
    :github "manateelazycat/lsp-bridge"
    :commands (lsp-bridge-find-def lsp-bridge-find-references global-lsp-bridge-mode)
    :init
    (setq lsp-bridge-enable-signature-help nil)
    (setq lsp-bridge-disable-backup nil)
    (setq acm-enable-icon nil)
    ;;(setq acm-candidate-match-function 'orderless-flex)
    (setq acm-enable-search-words nil)
    (setq acm-enable-quick-access t)
    :bind
    (:map lsp-bridge-ref-mode-map
          ("n" . lsp-bridge-ref-jump-next-keyword)
          ("p" . lsp-bridge-ref-jump-prev-keyword)
          ("M-n" . lsp-bridge-ref-jump-next-file)
          ("M-p" . lsp-bridge-ref-jump-prev-file)
          ("C-x C-q" . lsp-bridge-ref-switch-to-edit-mode)
          :map lsp-bridge-ref-mode-edit-map
          ("C-x C-q" . lsp-bridge-ref-apply-changed)
          ("C-x C-s" . lsp-bridge-ref-apply-changed)
          ("C-c C-k" . lsp-bridge-ref-quit)
          ("M-n" . lsp-bridge-ref-jump-next-file)
          ("M-p" . lsp-bridge-ref-jump-prev-file))
    :hook
    (lsp-bridge-mode . (lambda () (flycheck-mode -1)))
    (after-init . global-lsp-bridge-mode)
    :config
    (remove-hook 'lsp-bridge-default-mode-hooks 'org-mode-hook)
    ;; For Xref support
    (defun lsp-bridge-xref-backend ()
      "lsp-bridge backend for Xref."
      (when lsp-bridge-mode
        'lsp-bridge))

    (cl-defmethod xref-backend-identifier-at-point ((_backend (eql lsp-bridge)))
      (let ((current-symbol (symbol-at-point)))
        (when current-symbol
          (symbol-name current-symbol))))

    (cl-defmethod xref-backend-definitions ((_backend (eql lsp-bridge)) symbol)
      (lsp-bridge-find-def)
      nil)

    (cl-defmethod xref-backend-references ((_backend (eql lsp-bridge)) symbol)
      (lsp-bridge-find-references)
      nil)

    (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql lsp-bridge)))
      nil)
    (add-hook 'lsp-bridge-mode-hook (lambda ()
                                      (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend nil))))
  )

(provide 'init-lsp)
