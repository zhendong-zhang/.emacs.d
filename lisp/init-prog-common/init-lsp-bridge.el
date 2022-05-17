(defun create-compile-commands-json ()
  "For quickly use."
  (interactive)
  (declare-function projectile-project-root "projectile")
  (require 'projectile)
  (let ((project-root (projectile-project-root)))
    (when (and (executable-find "cmake")
               (file-exists-p (concat project-root "CMakeLists.txt")))
      (shell-command (concat "cd " project-root "; cmake -DCMAKE_EXPORT_COMPILE_COMMANDS=1 &")))
    (when (executable-find "make")
      (shell-command (concat "cd " project-root "; bear --append -- make -j8 &")))
    ))

(defun create-dot-clang-format ()
  (interactive)
  (declare-function projectile-project-root "projectile")
  (with-temp-buffer
    (erase-buffer)
    (insert "clang-format -style=\"{BasedOnStyle: llvm, IndentWidth: 4}\" -dump-config")
    (write-file (concat (projectile-project-root) ".clang-format"))
    ))

(use-package lsp-bridge
  :load-path "site-lisp/lsp-bridge"
  :commands (lsp-bridge-find-def lsp-bridge-find-references lsp-bridge-mode)
  :init
  (require 'lsp-bridge)
  :config
  (require 'lsp-bridge-orderless)

  ;; xref集成
  (defun lsp-bridge-xref-backend ()
    "lsp-bridge backend for Xref."
    (when lsp-bridge-mode
      'lsp-bridge))

  (cl-defmethod xref-backend-identifier-at-point ((_backend (eql lsp-bridge)))
    (let ((current-symbol (symbol-at-point)))
      (when current-symbol
        (symbol-name current-symbol))))

  (cl-defmethod xref-backend-definitions ((_backend (eql lsp-bridge)) symbol)
    (lsp-bridge-find-def))

  (cl-defmethod xref-backend-references ((_backend (eql lsp-bridge)) symbol)
    (lsp-bridge-find-references))

  (cl-defmethod xref-backend-identifier-completion-table ((_backend (eql lsp-bridge)))
    nil)

  (dolist (hook lsp-bridge-default-mode-hooks)
    (add-hook hook (lambda ()
                     (setq-local corfu-auto nil) ;; let lsp-bridge control when popup completion frame
                     (add-to-list 'xref-backend-functions 'lsp-bridge-xref-backend) ;; xref
                     (lsp-bridge-mode 1)
                     )))
  )

(provide 'init-lsp-bridge)
