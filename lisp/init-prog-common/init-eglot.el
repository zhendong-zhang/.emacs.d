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

(use-package eglot
  :hook
  ((c-mode-common
    bash-mode
    python-mode
    cmake-mode
    lua-mode
    python-mode
    web-mode) . eglot-ensure))

(provide 'init-eglot)
