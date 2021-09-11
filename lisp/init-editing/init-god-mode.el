(use-package god-mode
  :config
  (defun my-update-cursor-type ()
    (setq cursor-type
	  (if god-local-mode 'hollow 't)))
  (add-hook 'god-mode-enabled-hook 'my-update-cursor-type)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor-type)
  :bind
  (("C-z" . god-local-mode)
   :map god-local-mode-map
   ("z" . repeat)
   ("i" . god-local-mode)))

(provide 'init-god-mode)
