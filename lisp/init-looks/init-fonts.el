(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (eq system-type 'gnu/linux)
  (unless (file-exists-p "~/.local/share/fonts")
    (make-directory "~/.local/share/" t)
    (shell-command (concat "ln -sf " user-emacs-directory "etc/fonts/ ~/.local/share/fonts"))))

(cl-loop for font in '("LXGW WenKai Mono")
           when (font-installed-p font)
           return (set-frame-font (format "%s-%s" font 14)))

(provide 'init-fonts)
