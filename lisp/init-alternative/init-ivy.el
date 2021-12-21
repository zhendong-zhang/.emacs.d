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
  (ivy-mode)

  ;; FIXME: 解决find-file、load-file未默认选中当前文件，但会导致补全列表里当前文件有两个
  ;; 传入`ivy-read'的def值有问题，应置为nil，可考虑如`counsel-find-file'另加接口
  (setq insert-default-directory nil)
  )

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
  :init
  (setq counsel-preselect-current-file t)
  :bind
  (([remap execute-extended-command] . counsel-M-x)
   ([remap find-file] . counsel-find-file)
   ("M-g l" . counsel-locate)
   ("M-g M" . counsel-bookmark)
   ("M-g m" . counsel-mark-ring)
   ("M-s a" . counsel-ag)
   ("M-s r" . counsel-rg)
   ("M-g o" . counsel-rg)
   ("M-g F" . counsel-file-jump)))

(use-package counsel-projectile
  :after (counsel projectile)
  :bind
  ("M-s g" . counsel-projectile-grep))

(use-package counsel-tramp
  :bind
  ("M-g t" . counsel-tramp))

(provide 'init-ivy)
