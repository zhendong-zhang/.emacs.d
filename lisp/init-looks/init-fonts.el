(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (eq system-type 'gnu/linux)
  (unless (file-exists-p "~/.local/share/fonts")
    (make-directory "~/.local/share/" t)
    (shell-command (concat "ln -sf " user-emacs-directory "fonts/ ~/.local/share/fonts"))))

(let ((emacs-font-size 14)
      (emacs-font-name "LXGW WenKai Mono"))
  (set-frame-font (format "%s-%s" (eval emacs-font-name) (eval emacs-font-size))))

(provide 'init-fonts)
