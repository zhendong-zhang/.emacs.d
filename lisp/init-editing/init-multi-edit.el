(defvar multi-edit-guide-mode-keymap
  (let ((keymap (make-keymap)))
    (suppress-keymap keymap t)
    (define-key keymap (kbd "C-g") 'multi-edit-guide-mode-toggle)
    (define-key keymap (kbd "g") 'multi-edit-try-grab)
    (define-key keymap (kbd "s") 'multi-edit-search)
    (define-key keymap (kbd "m w") 'multi-edit-mark-word)
    (define-key keymap (kbd "a w") 'multi-edit-mark-all-word)
    (define-key keymap (kbd "m s") 'multi-edit-mark-symbol)
    (define-key keymap (kbd "a s") 'multi-edit-mark-all-symbol)
    (define-key keymap (kbd "m p") 'multi-edit-mark-sexp)
    (define-key keymap (kbd "a p") 'multi-edit-mark-all-sexp)
    (define-key keymap (kbd "m n") 'multi-edit-mark-number)
    (define-key keymap (kbd "a n") 'multi-edit-mark-all-number)
    (define-key keymap (kbd "m f") 'multi-edit-mark-defun)
    (define-key keymap (kbd "a f") 'multi-edit-mark-all-defun)
    (define-key keymap (kbd "m l") 'multi-edit-mark-line)
    (define-key keymap (kbd "a l") 'multi-edit-mark-all-line)
    (define-key keymap (kbd "m L") 'multi-edit-mark-line-content)
    (define-key keymap (kbd "a L") 'multi-edit-mark-all-line-content)
    keymap)
  "Global keymap for multi edit guide mode.")

(defcustom multi-edit-mode-line-lighter " [ME]"
  "Mode-line lighter for Multi edit."
  :type 'string
  :group 'multi-edit)

(defcustom multi-edit-set-mode-line-p t
  "Set nil if you use your own mode-line setting"
  :type 'boolean
  :group 'multi-edit)

(defvar multi-edit-need-update-mode-line nil)

(defconst multi-edit--mode-line-format '(:eval (multi-edit--update-mode-line)))

(defun multi-edit--update-mode-line ()
  (let* ((ol (multi-edit--get-nearest-overlay))
         (before (multi-edit--overlays-in (point-min) (or (and (overlayp ol) (overlay-start ol)) (point))))
         (after (multi-edit--overlays-in (or (and (overlayp ol) (1+ (overlay-end ol))) (point)) (point-max)))
         (count (+ (length before) (if (overlayp ol) 1 0))))
    (format " %d/%d " count (+ count (length after)))))

(defun multi-edit-set-mode-line ()
  (setq multi-edit-need-update-mode-line t)
  (when (and multi-edit-set-mode-line-p (not (member multi-edit--mode-line-format mode-line-format)))
    (setq mode-line-format (cons multi-edit--mode-line-format mode-line-format))))

(defun multi-edit-remove-mode-line ()
  (setq multi-edit-need-update-mode-line nil)
  (when (and multi-edit-set-mode-line-p (member multi-edit--mode-line-format mode-line-format))
    (setq mode-line-format (delete multi-edit--mode-line-format mode-line-format))))

(define-minor-mode multi-edit-guide-mode
  "Multi edit guide mode. "
  :init-value nil
  :lighter multi-edit-mode-line-lighter
  :keymap multi-edit-guide-mode-keymap)

(defun multi-edit-guide-mode-toggle (n)
  (interactive "p")
  (if multi-edit-guide-mode
      (progn
        (multi-edit-quit)
        (multi-edit-guide-mode -1))
    (multi-edit-guide-mode 1)
    (multi-edit-grab n)))

(define-key global-map (kbd "C-;") 'multi-edit-guide-mode-toggle)

(defvar multi-edit-quick-select-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-g") 'multi-edit-quick-select-mode)
    keymap)
  "Global keymap for multi edit mode.")

(define-minor-mode multi-edit-quick-select-mode
  "Multi edit quick select mode. "
  :init-value nil
  :lighter multi-edit-mode-line-lighter
  :keymap multi-edit-quick-select-mode-keymap
  (if multi-edit-quick-select-mode
      (add-hook 'pre-command-hook 'multi-edit-quick-select-actions nil t)
    (remove-hook 'pre-command-hook 'multi-edit-quick-select-actions t)))

(defvar multi-edit--select-direction nil)
(defvar multi-edit--select-column nil)
(defun multi-edit-quick-select (n)
  (interactive "p")
  (multi-edit-quick-select-mode 1)
  (when (not (multi-edit--overlays-exist))
    (if (region-active-p)
        (progn
          (multi-edit--add-overlay-at-region (region-beginning) (region-end))
          (setq multi-edit--select-column nil)
          (deactivate-mark t))
      (multi-edit--add-overlay-at-region (point) (point))
      (setq multi-edit--select-column (current-column))))
  (setq multi-edit--select-direction (>= n 0))
  (when-let (ol (multi-edit--get-front-overlay multi-edit--select-direction))
    (goto-char (if multi-edit--select-direction (overlay-end ol) (overlay-start ol)))
    (let* ((case-fold-search nil)
           (bounds (cons (overlay-start ol) (overlay-end ol)))
           (match (multi-edit--region-to-regexp (car bounds) (cdr bounds)))
           (times 0))
      (while (and bounds (< times (abs n)))
        (if (not multi-edit--select-column)
            (setq bounds (if (< n 0) (multi-edit--re-search-backward match) (multi-edit--re-search-forward match)))
          (if (< n 0) (multi-edit-beginning-of-line nil) (end-of-line))
          (multi-edit--forward-line nil (if (< n 0) -1 1))
          (move-to-column multi-edit--select-column)
          (setq bounds (cons (point) (point))))
        (cl-incf times)
        (multi-edit--add-overlay-at-region (car bounds) (cdr bounds))))))

(defun multi-edit-quick-cancel (n)
  (interactive "p")
  (if (or (not multi-edit-quick-select-mode) (not (multi-edit--overlays-exist)))
      (error "quick select mode is not active!")
    (when-let (ol (multi-edit--get-nearest-overlay (not multi-edit--select-direction)))
      (goto-char (if multi-edit--select-direction (overlay-start ol) (overlay-end ol)))
      (let* ((case-fold-search nil)
             (bounds (cons (overlay-start ol) (overlay-end ol)))
             (match (multi-edit--region-to-regexp (car bounds) (cdr bounds)))
             (times 0))
        (while (and bounds (< times (abs n)))
          (multi-edit--remove-overlay (multi-edit--get-nearest-overlay (not multi-edit--select-direction)))
          (setq bounds (if multi-edit--select-direction (multi-edit--re-search-backward match) (multi-edit--re-search-forward match)))
          (cl-incf times))))))

(defvar multi-edit-quick-select-commands '(multi-edit-quick-select multi-edit-quick-cancel
                                           universal-argument universal-argument-more negative-argument digit-argument
                                           previous-line next-line execute-extended-command)
  "which command should not change to multi-edit-mode.")

(defun multi-edit-quick-select-actions ()
  (when (equal this-command 'multi-edit-quick-select-mode)
    (multi-edit-quit))
  (unless (memq this-command multi-edit-quick-select-commands)
    (multi-edit-mode 1)
    (setq this-command #'ignore)
    (push last-input-event unread-command-events)))

(define-key global-map (kbd "C->") 'multi-edit-quick-select)
(define-key global-map (kbd "C-<") 'multi-edit-quick-cancel)

(defvar multi-edit-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-;") 'multi-edit-mode)
    (define-key keymap (kbd "C-g") 'multi-edit-mode)
    keymap)
  "Global keymap for multi edit mode.")

(define-minor-mode multi-edit-mode
  "Multi edit mode. "
  :init-value nil
  :lighter multi-edit-mode-line-lighter
  :keymap multi-edit-mode-keymap
  (if multi-edit-mode
      (cond (multi-edit-guide-mode
             (multi-edit-guide-mode -1)
             (multi-edit--maybe-start-macro))
            (multi-edit-quick-select-mode
             (multi-edit-quick-select-mode -1)
             (multi-edit--maybe-start-macro)))
    (multi-edit-apply-modification)
    (if (secondary-selection-exist-p)
        (multi-edit-guide-mode 1)
      (multi-edit-quit))))

(defun multi-edit-grab (n)
  "Create secondary selection if no region available."
  (interactive "p")
  (if (and (overlay-buffer mouse-secondary-overlay)
           (not (region-active-p)))
      (multi-edit--cancel-secondary-selection)
    (multi-edit--guess-secondary-selection n))
  (deactivate-mark t))

(defun multi-edit--cancel-secondary-selection ()
  (delete-overlay mouse-secondary-overlay))

(defvar multi-edit--grab-range-list (list 'defun 'paragraph 'buffer))
(defun multi-edit--guess-secondary-selection (n)
  (cond ((region-active-p)
         (move-overlay mouse-secondary-overlay (region-beginning) (region-end)))
        ((> n 1)
         (move-overlay mouse-secondary-overlay (line-beginning-position (- 2 n)) (line-end-position n)))
        (t
         (when-let (bounds
                    (cl-case (car multi-edit--grab-range-list)
                      ((defun)
                       (bounds-of-thing-at-point 'defun))
                      ((paragraph)
                       (bounds-of-thing-at-point 'paragraph))
                      (t
                       (cons (point-min) (point-max)))))
           (move-overlay mouse-secondary-overlay (car bounds) (cdr bounds))))))

(defun multi-edit-try-grab ()
  (interactive)
  (let* ((first (car multi-edit--grab-range-list)))
    (setq multi-edit--grab-range-list (cl-remove first multi-edit--grab-range-list))
    (add-to-list 'multi-edit--grab-range-list first t))
  (multi-edit--guess-secondary-selection 1))

(defun multi-edit-mark-word (n)
  "Mark current word under cursor.
Use negative argument to create a backward selection."
  (interactive "p")
  (multi-edit--mark-thing 'word n "\\<%s\\>"))

(defun multi-edit-mark-symbol (n)
  (interactive "p")
  (multi-edit--mark-thing 'symbol n "\\_<%s\\_>"))

(defun multi-edit-mark-sexp (n)
  (interactive "p")
  (multi-edit--mark-thing 'sexp n))

(defun multi-edit-mark-number (n)
  (interactive "p")
  (multi-edit--mark-thing 'number n))

(defun multi-edit-mark-defun (n)
  (interactive "p")
  (multi-edit--mark-thing 'defun n))

(defun multi-edit-mark-line (n)
  (interactive "p")
  (multi-edit--mark-thing 'line (- n)))

(defun multi-edit-mark-line-content (n)
  (interactive "p")
  (multi-edit--mark-thing 'line-content (- n)))

(defun multi-edit-mark-all-word (n)
  "Mark all word under cursor.
Use negative argument to create a backward selection."
  (interactive "p")
  (multi-edit--mark-all-thing 'word n))

(defun multi-edit-mark-all-symbol (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'symbol n))

(defun multi-edit-mark-all-sexp (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'sexp n))

(defun multi-edit-mark-all-number (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'number n))

(defun multi-edit-mark-all-defun (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'defun n))

(defun multi-edit-mark-all-line (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'line (- n)))

(defun multi-edit-mark-all-line-content (n)
  (interactive "p")
  (multi-edit--mark-all-thing 'line-content (- n)))

(defun multi-edit-beginning-of-line (skip-whitespaces &optional n)
  (beginning-of-line n)
  (when skip-whitespaces
    (skip-syntax-forward " \t" (line-end-position))))

(defun multi-edit-bounds-of-line-at-point (skip-whitespaces)
  (save-mark-and-excursion
    (multi-edit-beginning-of-line skip-whitespaces 1)
    (cons (point) (line-end-position))))

(defun multi-edit-bounds-of-thing-at-point (thing n)
  (let (bounds)
    (setq bounds (cl-case thing
                   ((line-content)
                    (multi-edit-bounds-of-line-at-point t))
                   ((line)
                    (multi-edit-bounds-of-line-at-point nil))
                   (t
                    (bounds-of-thing-at-point thing))))
    (when bounds
      (if (< n 0)
          (cons (cdr bounds) (car bounds))
        bounds))))

(defun multi-edit--mark-thing (thing n &optional format-str)
  (let* ((bounds (multi-edit-bounds-of-thing-at-point thing n))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (goto-char end)
      (let ((search (format (or format-str "%s")
                            (regexp-quote (buffer-substring-no-properties beg end)))))
        (unless (string-equal search (car regexp-search-ring))
          (add-to-history 'regexp-search-ring search regexp-search-ring-max))
        (multi-edit--update-overlays thing beg end)))))

(defun multi-edit--mark-all-thing (thing n)
  (when (multi-edit--forward-thing thing n)
    (let* ((bounds (multi-edit-bounds-of-thing-at-point thing n))
           (beg (car bounds))
           (end (cdr bounds)))
      (when beg
        (goto-char end)
        (multi-edit--update-overlays thing beg end t)))))

(defun multi-edit-search (arg)
  "Read a regexp from minibuffer, then search and select it."
  (interactive "P")
  (let* ((reverse arg)
         (beg (if (secondary-selection-exist-p)
                  (overlay-start mouse-secondary-overlay)
                (point-min)))
         (end (if (secondary-selection-exist-p)
                  (overlay-end mouse-secondary-overlay)
                (point-max)))
         (text (multi-edit--prompt-symbol-and-words
                "Search for: "
                beg end))
         bounds)
    (save-restriction
      (multi-edit--narrow-to-secondary-selection-if-exist)
      (if reverse
          (setq bounds (or (multi-edit--re-search-forward text reverse)
                           (multi-edit--re-search-backward text reverse)))
        (setq bounds (or (multi-edit--re-search-backward text reverse)
                         (multi-edit--re-search-forward text reverse)))))
    (when bounds
      (goto-char (cdr bounds))
      (unless (string-equal text (car regexp-search-ring))
        (add-to-history 'regexp-search-ring text regexp-search-ring-max))
      (multi-edit--update-overlays 'search (car bounds) (cdr bounds)))))

(defun multi-edit--update-overlays (thing beg end &optional all)
  (multi-edit--remove-overlays)
  (if (and (secondary-selection-exist-p)
           (< (overlay-start mouse-secondary-overlay)
              (overlay-end mouse-secondary-overlay))
           (<= (overlay-start mouse-secondary-overlay)
               (point)
               (overlay-end mouse-secondary-overlay)))
      (progn
        (if all
            (multi-edit--add-overlays-for-thing thing beg end)
          (multi-edit--add-overlays-for-match beg end))
        (multi-edit-mode 1))
    (push-mark beg t t)))

(defvar-local multi-edit--overlays nil)

(defun multi-edit--add-overlay-at-region (p1 p2)
  (let ((ol (make-overlay p1 p2)))
    (overlay-put ol 'multi-edit t)
    (if (= p1 p2)
        (overlay-put ol 'before-string (propertize "|" 'face 'region))
      (overlay-put ol 'face 'region))
    (overlay-put ol 'multi-edit-order (> p2 p1))
    (push ol multi-edit--overlays))
  (multi-edit-set-mode-line)
  (force-mode-line-update))

(defun multi-edit--remove-overlay (ol)
  (when (overlayp ol)
    (delete-overlay ol))
  (setq multi-edit--overlays (cl-remove ol multi-edit--overlays)))

(defun multi-edit--overlays-exist (&optional pos)
  (and multi-edit--overlays (> (length multi-edit--overlays) 0)))

(defun multi-edit--remove-overlays ()
  (mapc #'delete-overlay multi-edit--overlays)
  (setq multi-edit--overlays nil)
  (multi-edit-remove-mode-line))

(defun mult-edit--overlay< (a b)
  (< (overlay-start a) (overlay-start b)))

(defun multi-edit--get-front-overlay (&optional end)
  (setq multi-edit--overlays (sort multi-edit--overlays 'mult-edit--overlay<))
  (if end (car (last multi-edit--overlays)) (car multi-edit--overlays)))

(defun multi-edit--overlay-distance< (a b)
  (cond ((not (overlayp a)) nil)
        ((not (overlayp b)) t)
        (t (< (abs (- (overlay-start a) (point)))
              (abs (- (overlay-start b) (point)))))))

(defun multi-edit--get-nearest-overlay (&optional dir)
  (let* ((overlays (if dir (multi-edit--overlays-in (1- (point)) (point-max))
                     (multi-edit--overlays-in (point-min) (1+ (point)))))
         found ol)
    (while (setq ol (car overlays))
      (setq found (if (multi-edit--overlay-distance< ol found) ol found)
            overlays (cdr overlays)))
    found))

(defun multi-edit--overlays-in (beg end)
  (seq-filter (lambda (ol)
                (overlay-get ol 'multi-edit))
              (overlays-in beg end)))

(defun multi-edit--add-overlays-for-match (beg end)
  (save-restriction
    (multi-edit--narrow-to-secondary-selection-if-exist)
    (let ((step (if (> end beg) 1 -1))
          (match (multi-edit--region-to-regexp beg end t)))
      (save-mark-and-excursion
        (goto-char (point-min))
        (let ((case-fold-search nil)
              bounds)
          (while (setq bounds (multi-edit--re-search-forward match (<= end beg)))
            (multi-edit--add-overlay-at-region
             (car bounds)
             (cdr bounds))))))))

(defun multi-edit--add-overlays-for-thing (thing beg end)
  (save-restriction
    (multi-edit--narrow-to-secondary-selection-if-exist)
    (let* ((step (if (> end beg) 1 -1))
           (bounds))
      (save-mark-and-excursion
        (goto-char (if (> end beg) (point-min) (point-max)))
        (while (multi-edit--forward-thing thing step)
          (when (setq bounds (multi-edit-bounds-of-thing-at-point thing step))
            (multi-edit--add-overlay-at-region
             (car bounds)
             (cdr bounds))))))))

(defun multi-edit--narrow-to-secondary-selection-if-exist ()
  (when (secondary-selection-exist-p)
    (narrow-to-region (overlay-start mouse-secondary-overlay)
                      (overlay-end mouse-secondary-overlay))))

(defun multi-edit--region-to-regexp (beg end &optional whole-word)
  "Convert the word selected in region to a regexp."
  (let ((s (buffer-substring-no-properties beg end))
        (re (car regexp-search-ring)))
    (if (string-match-p (format "\\`%s\\'" re) s)
        re
      (format (if whole-word "\\<%s\\>" "%s") (regexp-quote s)))))

(defun multi-edit--forward-line (skip-whitespaces &optional n)
  (if (> n 0)
      (progn
        (when (= (point) (line-end-position))
          (forward-line 1))
        (end-of-line n))
    (let* ((orig (point)))
      (multi-edit-beginning-of-line skip-whitespaces 1)
      (when (= (point) orig)
        (forward-line -1)))
    (multi-edit-beginning-of-line skip-whitespaces (- n))))

(defun multi-edit--forward-thing (thing &optional n)
  (let* ((orig (point))
         bounds)
    (cl-case thing
      ((line-content)
       (multi-edit--forward-line t n))
      ((line)
       (multi-edit--forward-line nil n))
      (t
       (forward-thing thing n)))
    (not (= orig (point)))))

(defun multi-edit--re-search-forward (regexp &optional reverse)
  (when-let (match-data (re-search-forward regexp nil t))
    (let ((match (match-data)))
      (if reverse
          (cons (cadr match) (car match))
        (cons (car match) (cadr match))))))

(defun multi-edit--re-search-backward (regexp &optional reverse)
  (when-let (match-data (re-search-backward regexp nil t))
    (let ((match (match-data)))
      (if reverse
          (cons (cadr match) (car match))
        (cons (car match) (cadr match))))))

(defun multi-edit--prompt-symbol-and-words (prompt beg end)
  "Completion with PROMPT for symbols and words from BEG to END."
  (let ((completions))
    (save-mark-and-excursion
      (goto-char beg)
      (while (re-search-forward "\\_<\\(\\sw\\|\\s_\\)+\\_>" end t)
        (let ((result (match-string-no-properties 0)))
          (push (format "\\_<%s\\_>" (regexp-quote result)) completions))))
    (setq completions (delete-dups completions))
    (let ((selected (completing-read prompt completions nil nil)))
      selected)))

(defvar multi-edit--last-undo-length nil
  "fast undo after multi edit.")
(defvar multi-edit--last-modify-point nil)

(defun multi-edit--is-last-modify-overlay (ol)
  (and (<= (overlay-start ol) multi-edit--last-modify-point)
       (>= (1+ (overlay-end ol)) multi-edit--last-modify-point)))

(defun multi-edit--maybe-start-macro ()
  (when (and (not defining-kbd-macro)
             (not executing-kbd-macro))
    (setq multi-edit--last-undo-length (length buffer-undo-list))
    (setq multi-edit--last-modify-point (point))
    (funcall 'kmacro-start-macro nil)))

(defun multi-edit-apply-modification ()
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro))
  (atomic-change-group
    (save-mark-and-excursion
      (cl-loop for ol in multi-edit--overlays
               when (and (overlayp ol) (not (multi-edit--is-last-modify-overlay ol)))
               do (let* ((order (overlay-get ol 'multi-edit-order)))
                    (goto-char (if order (overlay-end ol) (overlay-start ol)))
                    (funcall 'kmacro-call-macro nil)))
      (setq multi-edit--last-undo-length (- (length buffer-undo-list)
                                            multi-edit--last-undo-length))
      (setq buffer-undo-list
            (cl-loop for v from 1 to multi-edit--last-undo-length
                     with list = buffer-undo-list
                     with new-list = '(nil)
                     unless (eq (car list) nil)
                     collect (car list) into new-list
                     do (setq list (cdr list))
                     finally return (append new-list list)))))
  (multi-edit--remove-overlays))

(defun multi-edit-quit ()
  (multi-edit--remove-overlays)
  (multi-edit--cancel-secondary-selection)
  (deactivate-mark t))

(provide 'init-multi-edit)
