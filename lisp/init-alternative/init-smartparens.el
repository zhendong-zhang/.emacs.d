(use-package smartparens
  :diminish (smartparens-strict-mode smartparens-mode)
  :bind
  (:map smartparens-strict-mode-map
        ([remap kill-region] . nil)
        :map smartparens-mode-map
        ("C-M-f" . sp-forward-sexp)
        ("C-M-b" . sp-backward-sexp)
        ("C-M-a" . sp-beginning-of-sexp)
        ("C-M-e" . sp-end-of-sexp)
        ("C-M-k" . sp-kill-sexp)
        ("C-M-w" . sp-copy-sexp)
        ("C-M-<backspace>" . sp-splice-sexp)
        ("C-M-<return>" . sp-split-sexp)
        ("M-]" . sp-forward-slurp-sexp)
        ("M-[" . sp-forward-barf-sexp))
  :hook
  (emacs-lisp-mode . smartparens-strict-mode)
  :config
  (require 'smartparens-config)
  (push 'org-mode sp-ignore-modes-list)
  (smartparens-global-mode 1))

(provide 'init-smartparens)
