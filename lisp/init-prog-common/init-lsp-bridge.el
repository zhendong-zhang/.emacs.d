(defun create-compile-flags-file ()
  "For quickly use."
  (interactive)
  (declare-function loop-on-all-sub-directory "init")
  (declare-function projectile-project-root "projectile")
  (require 'projectile)
  (with-temp-buffer
    (erase-buffer)
    (insert "-xc++")
    (let ((project-root (projectile-project-root)))
      (loop-on-all-sub-directory project-root
                                 '(lambda (dir)
                                    (insert "-I\n" dir "\n")))
      (write-file (concat project-root "compile_flags.txt")))
    ))

(defun create-dot-clang-format ()
  (interactive)
  (declare-function projectile-project-root "projectile")
  (with-temp-buffer
    (erase-buffer)
    (insert "clang-format -style=\"{BasedOnStyle: llvm, IndentWidth: 4}\" -dump-config")
    (write-file (concat (projectile-project-root) ".clang-format"))
    ))

(use-package all-the-icons)

(use-package lsp-bridge
  :ensure nil
  :bind
  (:map lsp-bridge-mode-map
        ([remap xref-find-definitions] . lsp-bridge-find-define)
        ([remap xref-pop-marker-stack] . lsp-bridge-return-from-def)
        ([remap xref-find-references] . lsp-bridge-find-references))
  :config
  (require 'lsp-bridge-orderless)   ;; make lsp-bridge support fuzzy match, optional
  ;(require 'lsp-bridge-icon)        ;; show icon for completion items, optional

  (defun start-lsp-bridge ()
    (setq-local corfu-auto nil)  ;; let lsp-bridge control when popup completion frame
    (lsp-bridge-mode 1))

  :hook
  ((c-mode
    c++-mode
    java-mode
    python-mode
    ruby-mode
    rust-mode
    elixir-mode
    go-mode
    haskell-mode
    haskell-literate-mode
    dart-mode
    scala-mode
    typescript-mode
    js2-mode
    js-mode
    tuareg-mode
    latex-mode
    Tex-latex-mode
    texmode
    context-mode
    texinfo-mode
    bibtex-mode) . start-lsp-bridge)
  )

(provide 'init-lsp-bridge)
