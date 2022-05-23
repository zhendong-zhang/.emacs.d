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

(use-package dumb-jump
  :config
  (require 'xref)
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate 2)
  (setq xref-show-definitions-function #'xref-show-definitions-completing-read)
  (setq dumb-jump-force-searcher 'ag))

(if is-windows-nt
    (use-package eglot
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
  (use-package posframe)
  (use-package all-the-icons)
  (use-package lsp-bridge
    :quelpa (lsp-bridge :fetcher github :repo "manateelazycat/lsp-bridge")
    :commands (lsp-bridge-find-def lsp-bridge-find-references global-lsp-bridge-mode)
    :init
    (setq lsp-bridge-enable-signature-help t)
    :bind
    (:map lsp-bridge-ref-mode-map
          ([remap next-line] . lsp-bridge-ref-jump-next-keyword)
          ([remap previous-line] . lsp-bridge-ref-jump-prev-keyword)
          ("M-n" . lsp-bridge-ref-jump-next-file)
          ("M-p" . lsp-bridge-ref-jump-prev-file))
    :config
    (require 'lsp-bridge-ui)
    (require 'lsp-bridge-ui-history)
    (require 'lsp-bridge-orderless)

    (global-lsp-bridge-ui-mode)       ;; use lsp-bridge-ui as completion ui
    (lsp-bridge-ui-history-mode t)
    (global-lsp-bridge-mode)

    ;; For Xref support
    (add-hook 'lsp-bridge-mode-hook (lambda ()
                                      (add-hook 'xref-backend-functions #'lsp-bridge-xref-backend 1 t)))))

(provide 'init-lsp)
