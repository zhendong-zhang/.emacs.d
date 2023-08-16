(defun narrow-or-widen-dwim ()
  "If the buffer is narrowed, it widens. Otherwise, it narrows to region
, or Org subtree."
  (interactive)
  (cond ((buffer-narrowed-p) (widen))
        ((region-active-p) (narrow-to-region (region-beginning) (region-end)))
        ((equal major-mode 'org-mode) (org-narrow-to-subtree))
        (t (error "Please select a region to narrow to"))))
;; remap C-x n n to `narrow-or-widen-dwim'
(global-set-key [remap narrow-to-region] 'narrow-or-widen-dwim)

(defun delete-this-file ()
  "Delete the current file, and kill the buffer."
  (interactive)
  (or (buffer-file-name) (error "No file is currently being edited"))
  (when (yes-or-no-p (format "Really delete '%s'?"
                             (file-name-nondirectory buffer-file-name)))
    (delete-file (buffer-file-name))
    (kill-this-buffer)))

(defun rename-this-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (unless filename
      (error "Buffer '%s' is not visiting a file!" name))
    (if (get-buffer new-name)
        (message "A buffer named '%s' already exists!" new-name)
      (progn
        (rename-file filename new-name 1)
        (rename-buffer new-name)
        (set-visited-file-name new-name)
        (set-buffer-modified-p nil)))))

(defun copy-file-path ()
  "Copy file path to king ring"
  (interactive)
  (kill-new (buffer-file-name)))

(defun smarter-move-beginning-of-line (&optional arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

(defun smarter-mark-current-line (&optional arg)
  "Mark current line."
  (interactive "P")
  (push-mark (point) nil t)
  (smarter-move-beginning-of-line)
  (push-mark nil nil)
  (end-of-line (prefix-numeric-value arg)))

(defalias 'mark-line 'smarter-mark-current-line "Mark current line.")

(defun smarter-zap-up-to-char (arg char)
  "Smarter zap up to char, to support mc."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Zap up to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(global-set-key (kbd "M-Z") 'smarter-zap-up-to-char)

(defun align-repeat (start end regexp)
  "Repeat alignment with respect to
     the given regular expression."
      (interactive "r\nsAlign regexp: ")
      (align-regexp start end
                    (concat "\\(\\s-*\\)" regexp) 1 1 t))

(defun jump-to-char (arg char)
  "Like Zap-to-char, but not kill."
  (interactive (list (prefix-numeric-value current-prefix-arg)
             (read-char "Jump to char: " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
    (setq char (or (aref translation-table-for-input char) char))))
  (search-forward (char-to-string char) nil nil arg))
(global-set-key (kbd "M-g c") 'jump-to-char)
(global-set-key (kbd "M-g C") 'goto-char)

(defun copy-to-char (arg char)
  "Like Zap-to-char, but not kill, default is copy to end of line."
  (interactive (list (prefix-numeric-value current-prefix-arg)
                     (read-char "Copy to char(default to end of line): " t)))
  ;; Avoid "obsolete" warnings for translation-table-for-input.
  (with-no-warnings
    (if (char-table-p translation-table-for-input)
        (setq char (or (aref translation-table-for-input char) char))))

  (if (= char 13)
      (save-excursion
        (copy-region-as-kill (point) (point-at-eol))
        (message "copied to end of line."))
    (save-excursion
      (copy-region-as-kill (point) (search-forward (char-to-string char) nil nil arg)))
    (message "copied to %dth %s" arg (char-to-string char))))
(global-set-key (kbd "C-x c c") 'copy-to-char)

(defun copy-to-end-of-line ()
  "Copy to end of line."
  (interactive)
  (save-excursion
    (copy-region-as-kill (point) (point-at-eol))
    (message "copied to end of line.")))
(global-set-key (kbd "C-x c e") 'copy-to-end-of-line)

(defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "%Y-%m-%d")
                   ((equal prefix '(4)) "%d.%m.%Y")
                   ((equal prefix '(16)) "%d %B %Y")))
          )
      (insert (format-time-string format))))

(defun insert-line (&optional arg)
  "Insert string in current line and then make newline."
  (interactive "P")
  (when (not arg)
    (setq arg 1))
  (let* ((s (read-from-minibuffer "insert string : ")))
    (dotimes (v arg)
      (insert s)
      (newline-and-indent))))

;; Random line sorting
(defun sort-lines-random (beg end)
  "Sort lines in region randomly."
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (let ;; To make `end-of-line' and etc. to ignore fields.
          ((inhibit-field-text-motion t))
        (sort-subr nil 'forward-line 'end-of-line nil nil
                   (lambda (s1 s2) (eq (random 2) 0)))))))

(defun shuffle-buffer ()
  "Shuffle lines in current buffer"
  (interactive)
  (sort-lines-random (point-min) (point-max)))

(defun endless/capitalize ()
  "Capitalize region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'capitalize-region)
    (call-interactively 'subword-capitalize)))

(defun endless/downcase ()
  "Downcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'downcase-region)
    (call-interactively 'subword-downcase)))

(defun endless/upcase ()
  "Upcase region or word."
  (interactive)
  (if (use-region-p)
      (call-interactively 'upcase-region)
    (call-interactively 'subword-upcase)))

(require 'subword)
;; these bindings are fine
(global-set-key "\M-c" 'endless/capitalize)
(global-set-key "\M-l" 'endless/downcase)
(global-set-key "\M-u" 'endless/upcase)

(define-key global-map (kbd "RET") 'newline-and-indent)
(global-set-key [remap just-one-space] 'cycle-spacing)

(defun sanityinc/newline-at-end-of-line ()
  "Move to end of line, enter a newline, and reindent."
  (interactive)
  (move-end-of-line 1)
  (newline-and-indent))

(global-set-key (kbd "S-<return>") 'sanityinc/newline-at-end-of-line)

(defun kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(global-set-key (kbd "C-M-<backspace>") 'kill-back-to-indentation)

;; Fix backward-up-list to understand quotes, see http://bit.ly/h7mdIL
(defun backward-up-sexp (arg)
  "Jump up to the start of the ARG'th enclosing sexp."
  (interactive "p")
  (let ((ppss (syntax-ppss)))
    (cond ((elt ppss 3)
           (goto-char (elt ppss 8))
           (backward-up-sexp (1- arg)))
          ((backward-up-list arg)))))

(global-set-key [remap backward-up-list] 'backward-up-sexp) ; C-M-u, C-M-up

(defun open-line-with-reindent (n)
  "A version of `open-line' which reindents the start and end positions.
If there is a fill prefix and/or a `left-margin', insert them
on the new line if the line would have been blank.
With arg N, insert N newlines."
  (interactive "*p")
  (let* ((do-fill-prefix (and fill-prefix (bolp)))
     (do-left-margin (and (bolp) (> (current-left-margin) 0)))
     (loc (point-marker))
     ;; Don't expand an abbrev before point.
     (abbrev-mode nil))
    (delete-horizontal-space t)
    (newline n)
    (indent-according-to-mode)
    (when (eolp)
      (delete-horizontal-space t))
    (goto-char loc)
    (while (> n 0)
      (cond ((bolp)
         (if do-left-margin (indent-to (current-left-margin)))
         (if do-fill-prefix (insert-and-inherit fill-prefix))))
      (forward-line 1)
      (setq n (1- n)))
    (goto-char loc)
    (end-of-line)
    (indent-according-to-mode)))

(global-set-key [remap open-line] 'open-line-with-reindent)

(defun eval-and-replace ()
  "Replace the preceding sexp with its value."
  (interactive)
  (backward-kill-sexp)
  (condition-case nil
      (prin1 (eval (read (current-kill 0)))
             (current-buffer))
    (error (message "Invalid expression")
           (insert (current-kill 0)))))

(global-set-key [remap elisp-eval-region-or-buffer] 'eval-and-replace)
(global-set-key (kbd "C-c C-e") 'eval-and-replace)

(provide 'init-common-funcs)
