(use-package popper
  :demand
  :bind
  (:map popper-mode-map
        ("M-g h"   . popper-toggle)
        ("C-`"   . popper-cycle)
        ("M-`" . popper-toggle-type))
  :custom
  (popper-mode-line nil)
  (popper-reference-buffers '("\\*Warnings\\*" "\\*Backtrace\\*" "Output\\*$" "\\*Pp Eval Output\\*$"
                              help-mode helpful-mode
                              grep-mode occur-mode rg-mode deadgrep-mode ag-mode pt-mode ivy-occur-mode ivy-occur-grep-mode
                              "\\*Completions\\*"
                              "\\*lsp-help\\*$" "\\*lsp session\\*$"
                              lsp-bridge-ref-mode xref--xref-buffer-mode
                              "\\*Compile-Log\\*" compilation-mode
                              "\\*Flycheck errors\\*$" " \\*Flycheck checker\\*$" "\\*Flymake diagnostics.*\\*"
                              "\\*gud-debug\\*$" "\\*DAP Templates\\*$" dap-server-log-mode
                              "\\*quickrun\\*$"
                              "\\*Async Shell Command\\*"
                              "^\\*eshell.*\\*.*$" eshell-mode
                              "^\\*shell.*\\*.*$"  shell-mode
                              "^\\*terminal.*\\*.*$" term-mode
                              "^\\*vterm.*\\*.*$"  vterm-mode
                              "\\*vc-.*\\*$" comint-mode
                              "\\*Apropos\\*"
                              "\\*prodigy\\*"
                              "\\*Finder\\*"
                              "\\*Kill Ring\\*"
                              "\\*Edit Annotation.*\\*"
                              "\\*Flutter\\*"
                              "\\*Embark Actions\\*" "\\*Embark Export:.*\\*"
                              bookmark-bmenu-mode tabulated-list-mode Buffer-menu-mode
                              "\\*eldoc\\*"
                              gnus-article-mode devdocs-mode
                              process-menu-mode list-environment-mode cargo-process-mode
                              youdao-dictionary-mode osx-dictionary-mode fanyi-mode maple-translate-mode
                              "\\*ELP Profiling Restuls\\*" profiler-report-mode
                              "\\*Paradox Report\\*$" "\\*package update results\\*$" "\\*Package-Lint\\*$"
                              "\\*[Wo]*Man.*\\*$"
                              "\\*ert\\*$" overseer-buffer-mode
                              "\\*tldr\\*$"
                              "^\\*elfeed-entry\\*$"
                              "^\\*macro expansion\\**"
                              "\\*Calendar\\*"
                              "\\*Agenda Commands\\*" "\\*Org Select\\*" "\\*Capture\\*" "^CAPTURE-.*\\.org*"
                              "\\*Gofmt Errors\\*$" "\\*Go Test\\*$" godoc-mode
                              "\\*docker-containers\\*" "\\*docker-images\\*" "\\*docker-networks\\*" "\\*docker-volumes\\*"
                              "\\*prolog\\*" inferior-python-mode inf-ruby-mode swift-repl-mode
                              "\\*rustfmt\\*$" rustic-compilation-mode rustic-cargo-clippy-mode
                              rustic-cargo-outdated-mode rustic-cargo-test-mode))
  (popper-display-function #'my-popper-select-popup-at-bottom)
  :preface
  (defun my-popper-fit-window-height (win)
    "Determine the height of popup window WIN by fitting it to the buffer's content."
    (fit-window-to-buffer
     win
     (floor (frame-height) 4)
     (floor (frame-height) 4)))
  (defun popper-close-window-hack (&rest _)
    "Close popper window via `C-g'."
    ;; `C-g' can deactivate region
    (when (and (called-interactively-p 'interactive)
               (not (region-active-p))
               popper-open-popup-alist)
      (let ((window (caar popper-open-popup-alist)))
        (when (window-live-p window)
          (delete-window window)))))
  (defun my-popper-select-popup-at-bottom (buffer &optional alist)
    "Display and switch to popup-buffer BUFFER at the bottom of the screen."
    (let ((window (my-popper-display-popup-at-bottom buffer alist)))
      (select-window window)))
  (defun my-popper-display-popup-at-bottom (buffer &optional alist)
    "Display popup-buffer BUFFER at the bottom of the screen."
    (display-buffer-at-bottom
     buffer
     (append alist
             `((window-height . ,popper-window-height)
               (side . bottom)
               (slot . 1)))))
  :config
  (popper-mode 1)
  (require 'popper-echo)
  (popper-echo-mode 1)
  (setq popper-window-height #'my-popper-fit-window-height)
  (advice-add #'keyboard-quit :before #'popper-close-window-hack))

(provide 'init-popper)
