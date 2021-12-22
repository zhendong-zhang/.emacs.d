(defun font-installed-p (font-name)
  "Check if font with FONT-NAME is available."
  (find-font (font-spec :name font-name)))

(when (eq system-type 'gnu/linux)
  (unless (file-exists-p "~/.local/share/fonts")
    (make-directory "~/.local/share/" t)
    (shell-command (concat "ln -sf " user-emacs-directory "fonts/ ~/.local/share/fonts"))))

(use-package cnfonts
  :demand
  :init
  (setq cnfonts-personal-fontnames '(
                                     nil
                                     ("Sarasa Mono SC Nerd")
                                     nil
                                     nil
                                     nil))
  :bind
  ("C-+" . cnfonts-increase-fontsize)
  :config
  (cnfonts-mode 1))

(provide 'init-fonts)
