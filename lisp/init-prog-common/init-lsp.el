;;; init-lsp.el --- lsp相关 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(eval-when-compile
  (declare-function projectile-project-root "projectile")
  (declare-function compilation-read-command "compile"))

(defun create-compile-commands-json ()
  "For quickly use."
  (interactive)
  (require 'projectile)
  (require 'compile)
  (let ((project-root (projectile-project-root)))
    (cond ((and (executable-find "cmake") (file-exists-p (concat project-root "CMakeLists.txt")))
           (shell-command (concat "cd " project-root "; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 &")))
          ((and (executable-find "make") (executable-find "bear"))
           (shell-command (concat "cd " project-root "; bear --append -- " (compilation-read-command "make") " -j8 &")))
          (t
           (with-temp-buffer
             (erase-buffer)
             (insert "-xc++\n")
             (insert "-I.")
             (write-file (concat project-root "compile_flags.txt")))))))

(defun create-dot-clang-format ()
  (interactive)
  (require 'projectile)
  (with-temp-buffer
    (erase-buffer)
    (insert "# google 编程风格
BasedOnStyle: Google
IndentWidth: 4
# 访问说明符(public、private等)的偏移(缩进或者对齐)
AccessModifierOffset: -4")
    (write-file (concat (projectile-project-root) ".clang-format"))
    ))

(use-package xref
  :functions xref-show-definitions-completing-read
  :config
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (use-package dumb-jump
    :functions dumb-jump-xref-activate
    :config
    (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 2)
    (setq dumb-jump-force-searcher 'ag)))

(use-package corfu
  :commands global-corfu-mode corfu-history-mode
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
  :hook
  ((prog-mode . (lambda ()
                  (unless (derived-mode-p
                           'emacs-lisp-mode 'lisp-mode
                           'makefile-mode 'snippet-mode
                           'ron-mode)
                    (eglot-ensure))))))

(use-package eglot-booster
  :github "jdtsmith/eglot-booster"
  :after eglot
  :config	(eglot-booster-mode))

(provide 'init-lsp)

;;; init-lsp.el ends here
