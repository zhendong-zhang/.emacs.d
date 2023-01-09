(use-package meow
  :github "zhendong-zhang/meow"
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
  (meow-cheatsheet-physical-layout meow-cheatsheet-physical-layout-ansi)
  (meow-cheatsheet-layout meow-cheatsheet-layout-dvp)
  (meow-keypad-ctrl-meta-prefix ?M)
  (meow-text-edit-state 'insert)
  :config
  (defun meow-setup ()
    (meow-leader-define-key
     '("?" . meow-cheatsheet)
     '("1" . "M-1")
     '("2" . "M-2")
     '("3" . "M-3")
     '("4" . "M-4")
     '("5" . "M-5")
     '("6" . "M-6")
     '("7" . "M-7")
     '("8" . "M-8")
     '("9" . "M-9")
     '("0" . "M-0")
     '("s" . "M-g s")
     '("i" . "M-g i")
     '("," . "M-,")
     '("." . "M-.")
     )
    (meow-motion-overwrite-define-key
     ;; custom keybinding for motion state
     '("<escape>" . ignore))
    (meow-normal-define-key
     '("1" . meow-expand-1)
     '("2" . meow-expand-2)
     '("3" . meow-expand-3)
     '("4" . meow-expand-4)
     '("5" . meow-expand-5)
     '("6" . meow-expand-6)
     '("7" . meow-expand-7)
     '("8" . meow-expand-8)
     '("9" . meow-expand-9)
     '("0" . meow-expand-0)
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
     '("G" . meow-cancel-selection) ;; C-g
     '("<escape>" . meow-cancel-selection)
     '("U" . meow-undo-in-selection)
     '("s" . meow-search)           ;; C-s
     '("r" . meow-replace)
     '("v" . meow-visit)
     '("/" . meow-pop-selection)

     '("g" . meow-grab)
     '("m w" . meow-mark-word)
     '("e w" . meow-next-word)
     '("m s" . meow-mark-symbol)
     '("e s" . meow-next-symbol)
     '("m p" . meow-mark-sexp)
     '("e p" . meow-next-sexp)
     '("m n" . meow-mark-number)
     '("e n" . meow-next-number)
     '("m d" . meow-mark-defun)
     '("e d" . meow-next-defun)
     '("m l" . meow-line)
     '("e l" . meow-line-expand)
     '("m b" . meow-block)
     '("e b" . meow-to-block)
     '("m f" . meow-find)
     '("e f" . meow-find-expand)
     '("m u" . meow-till)
     '("e u" . meow-till-expand)
     '("m t" . meow-join)           ;; indentation
     '("e t" . meow-join)
     '("m i" . meow-inner-of-thing) ;; C-=
     '("m o" . meow-bounds-of-thing)
     '("m a" . meow-beginning-of-thing)
     '("m e" . meow-end-of-thing)

     '("a" . meow-append)
     '("A" . meow-open-below)
     '("i" . meow-insert)
     '("I" . meow-open-above)
     '("c" . meow-change)
     '("C" . meow-insert-only)

     '("T" . meow-swap-grab)        ;; M-t
     '("W" . meow-sync-grab)        ;; overwrite

     '(">" . "M->")
     '("<" . "M-<")
     '("}" . "M-}")
     '("{" . "M-{")
     '("d" . "C-d")
     '("w" . "M-w")
     '("k" . "C-k")
     '("y" . "C-y")
     '("u" . "C-x u")
     '("x" . "C-x C-x")
     '("z" . "C-x z")
     ))

  (meow-setup)
  (meow-global-mode 1))

(provide 'init-meow)
