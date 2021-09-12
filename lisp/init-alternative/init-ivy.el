(use-package vc)
(use-package tramp)
(use-package bookmark)
(use-package flx)
(use-package amx)
(use-package ivy-avy)

(use-package ivy
  :demand
  :diminish ivy-mode
  :init
  (setq ivy-extra-directories '("./")
        ivy-use-virtual-buffers t
        enable-recursive-minibuffers t
        ivy-wrap t
        ivy-count-format "%d/%d ")
  :bind
  (("M-g b" . ivy-switch-buffer)
   :map ivy-minibuffer-map
   ("C-;" . ivy-avy)
   ("C-w" . ivy-yank-word)
   ("C-o" . ivy-occur)
   ("C-S-o" . hydra-ivy/body)
   ("M-r" . ivy-toggle-regexp-quote)
   :map ivy-occur-mode-map
   ("n" . ivy-occur-next-line)
   ("p" . ivy-occur-previous-line)
   ("b" . backward-char)
   ("f" . forward-char)
   ("x" . ivy-occur-press))
  :config
  (ivy-mode))

(use-package imenu-anywhere
  :bind
  ("M-g I" . ivy-imenu-anywhere))

(defun my-swiper-thing-at-point ()
  (interactive)
  (if (use-region-p)
      (swiper-thing-at-point)
    (swiper)))

(use-package swiper
  :bind
  (("M-g s" . my-swiper-thing-at-point)
   ("M-g S" . swiper-thing-at-point)
   :map swiper-map
   ("M-%" . swiper-query-replace)
   ("C->" . swiper-mc)))

(use-package counsel
  :demand
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ("M-g l" . counsel-locate)
   ("M-g M" . counsel-bookmark)
   ("M-g m" . counsel-mark-ring)
   ("M-s a" . counsel-ag)
   ("M-s r" . counsel-rg)
   ("M-g o" . counsel-rg)))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind
  ("M-s g" . counsel-projectile-grep))

(provide 'init-ivy)
