(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (eq system-type 'gnu/linux)
  (unless (file-exists-p "~/.local/share/fonts")
    (make-directory "~/.local/share/")
    (shell-command (concat "ln -sf " user-emacs-directory "fonts/ ~/.local/share/fonts"))))

(when (display-graphic-p)
  (cl-loop for font in '("Sarasa Mono SC Nerd" "Sarasa Mono SC" "DejaVu Sans Mono")
           when (font-installed-p font)
           return (set-face-attribute 'default nil
                                      :font font
                                      :height 100))

  ;; Specify font for Chinese characters
  (cl-loop for font in '("Sarasa Mono SC Nerd" "Sarasa Mono SC" "WenQuanYi Micro Hei Mono")
           when (font-installed-p font)
           return (set-fontset-font t '(#x4e00 . #x9fff) font)))

(provide 'init-fonts)
