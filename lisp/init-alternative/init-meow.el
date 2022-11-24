(use-package meow
  :custom
  (meow-char-thing-table '((?\( . round)
                           (?\[ . square)
                           (?\{ . curly)
                           (?\" . string)
                           (?s . symbol)
                           (?w . word)
                           (?W . window)
                           (?b . buffer)
                           (?p . paragraph)
                           (?l . line)
                           (?f . defun)
                           (?. . sentence)))
  (meow-mode-state-list '((authinfo-mode . insert)
                          (beancount-mode . insert)
                          (bibtex-mode . insert)
                          (cider-repl-mode . insert)
                          (cider-test-report-mode . insert)
                          (cider-browse-spec-view-mode . motion)
                          (cargo-process-mode . insert)
                          (conf-mode . insert)
                          (deadgrep-edit-mode . insert)
                          (deft-mode . insert)
                          (diff-mode . insert)
                          (ediff-mode . motion)
                          (gud-mode . insert)
                          (haskell-interactive-mode . insert)
                          (help-mode . insert)
                          (json-mode . insert)
                          (jupyter-repl-mode . insert)
                          (mix-mode . insert)
                          (occur-edit-mode . insert)
                          (pass-view-mode . insert)
                          (prog-mode . insert)
                          (py-shell-mode . insert)
                          (restclient-mode . insert)
                          (telega-chat-mode . insert)
                          (term-mode . insert)
                          (text-mode . insert)
                          (vterm-mode . insert)
                          (Custom-mode . insert)))
  :config
  (defun meow-setup ()
    (meow-leader-define-key
     '("1" . meow-digit-argument)
     '("2" . meow-digit-argument)
     '("3" . meow-digit-argument)
     '("4" . meow-digit-argument)
     '("5" . meow-digit-argument)
     '("6" . meow-digit-argument)
     '("7" . meow-digit-argument)
     '("8" . meow-digit-argument)
     '("9" . meow-digit-argument)
     '("0" . meow-digit-argument)
     '("/" . meow-keypad-describe-key)
     '("?" . meow-cheatsheet))
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("0" . meow-expand-0)
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("-" . negative-argument)
     '("p" . meow-prev)
     '("n" . meow-next)
     '("b" . meow-left)
     '("f" . meow-right)
     '("P" . meow-prev-expand)
     '("N" . meow-next-expand)
     '("B" . meow-left-expand)
     '("F" . meow-right-expand)
     '("q" . meow-quit)
     '("g" . meow-cancel-selection) ;; C-g
     '("w" . meow-save)             ;; M-w
     '("k" . meow-kill)             ;; C-k
     '("y" . meow-yank)             ;; C-y
     '("u" . meow-undo)             ;; C-x u
     '("U" . meow-undo-in-selection)
     '("x" . meow-reverse)          ;; C-x C-x
     '("z" . repeat)                ;; C-x z
     '("d" . meow-delete)           ;; C-d
     '("s" . meow-search)           ;; C-s
     '("r" . meow-replace)
     '("v" . meow-visit)
     '("l" . meow-goto-line)        ;; M-g M-g
     '("= i" . meow-inner-of-thing) ;; C-=
     '("= o" . meow-bounds-of-thing)
     '("= b" . meow-beginning-of-thing)
     '("= e" . meow-end-of-thing)

     '("G" . meow-grab)
     '("m w" . meow-mark-word)
     '("m W" . meow-next-word)
     '("m s" . meow-mark-symbol)
     '("m S" . meow-next-symbol)
     '("m l" . meow-line)
     '("m b" . meow-block)
     '("m B" . meow-to-block)
     '("m f" . meow-find)
     '("m F" . meow-till)
     '("m i" . meow-join)           ;; indentation

     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("c" . meow-change)

     '("T" . meow-swap-grab)        ;; M-t
     '("W" . meow-sync-grab)        ;; overwrite
     ))

  (meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)
