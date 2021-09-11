(use-package company
  :demand
  :diminish
  :bind
  (:map company-mode-map
        ([tab] . nil)
        ("TAB" . nil)
        ("M-/" . 'my-company-complete)
        ([remap completion-at-point] . company-complete)
        :map company-active-map
        ("C-o" . company-filter-candidates)
        ("C-h" . nil)
        ("C-d" . company-show-doc-buffer)
        ("M-/" . company-other-backend)
        ("M-." . company-show-location)
        ("C-n" . company-select-next)
        ("C-p" . company-select-previous))
  :init
  (setq-default company-dabbrev-other-buffers 'all
                company-tooltip-align-annotations t
                company-show-quick-access t
                company-selection-wrap-around t
                company-dabbrev-char-regexp "[\\0-9a-z-_/]"
                company-idle-delay 0.5)
  (setq company-backends '(company-capf
                           company-yasnippet
                           (company-dabbrev-code company-keywords company-files company-dabbrev)))
  :config
  (use-package company-statistics :config (company-statistics-mode t))
  (use-package abbrev :ensure nil :diminish)
  (global-company-mode))

(defun my-company-complete ()
  "Complete with `company-manual-begin', and try with `company-other-backend' when no completions found."
  (interactive)
  (company-manual-begin)
  (unless company-candidates
    (company-other-backend)))

(provide 'init-company)
