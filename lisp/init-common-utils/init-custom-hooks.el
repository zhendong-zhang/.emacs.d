;;; init-custom-hooks.el --- hooks -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defvar after-make-console-frame-hooks '()
  "Hooks to run after creating a new TTY frame")
(defvar after-make-window-system-frame-hooks '()
  "Hooks to run after creating a new window-system frame")

(defun run-after-make-frame-hooks (frame)
  "Selectively run either `after-make-console-frame-hooks' or
`after-make-window-system-frame-hooks'"
  (select-frame frame)
  (run-hooks (if window-system
                 'after-make-window-system-frame-hooks
               'after-make-console-frame-hooks)))

(add-hook 'after-make-frame-functions 'run-after-make-frame-hooks)

(defvar first-file-hook '()
  "Transient hooks run after the first interactively opened file.")

(defmacro add-hook-run-once (hook function &optional append local)
  "Like add-hook, but remove the hook after it is called"
  (let ((sym (make-symbol (format "%s%s" function "#once"))))
    `(progn
       (defun ,sym ()
         (remove-hook ,hook ',sym ,local)
         (run-hooks ,function))
       (add-hook ,hook ',sym ,append ,local))))

(add-hook-run-once 'find-file-hook 'first-file-hook)

(provide 'init-custom-hooks)

;;; init-custom-hooks.el ends here
