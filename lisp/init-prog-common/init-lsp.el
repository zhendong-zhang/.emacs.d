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

(use-package lsp-mode
  :hook ((c-mode-common . maybe-start-lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :init
  (setq read-process-output-max (* 8 1024 1024))
  (setq lsp-progress-prefix " "
        lsp-display-inline-image nil
        lsp-keymap-prefix "C-c l"
        lsp-keep-workspace-alive nil
        lsp-signature-auto-activate nil
        lsp-modeline-code-actions-enable nil
        lsp-modeline-diagnostics-enable nil
        lsp-modeline-workspace-status-enable nil
        lsp-enable-file-watchers nil
        lsp-enable-folding nil
        lsp-enable-symbol-highlighting nil
        lsp-enable-text-document-color nil
        lsp-enable-indentation nil
        lsp-enable-on-type-formatting nil
        lsp-headerline-breadcrumb-enable nil
        lsp-auto-guess-root t)
  :config
  (defvar lsp-config-files '("compile_commands.json" "compile_flags.txt"))

  (use-package projectile
    :config
    (setq projectile-project-root-files-top-down-recurring
          (append lsp-config-files projectile-project-root-files-top-down-recurring)))

  (defun maybe-start-lsp-deferred ()
    (declare-function lsp--suggest-project-root "lsp-mode")
    (let ((project-root (lsp--suggest-project-root)))
      (dolist (conf-file lsp-config-files)
        (when (file-exists-p conf-file)
          (lsp-deferred)))))
  )

(provide 'init-lsp)
