;;; init-multi-edit.el --- 多点编辑 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defcustom multi-edit-mode-line-lighter-format " [ME%s]"
  "Mode-line lighter for Multi edit."
  :type 'string
  :group 'multi-edit)

(defcustom multi-edit-set-mode-line-p t
  "Set nil if you use your own mode-line setting."
  :type 'boolean
  :group 'multi-edit)

(defcustom multi-edit-grab-range-list (list 'defun 'paragraph 'buffer)
  "Selection range for `multi-edit-guide-mode-toggle' and `multi-edit-try-grab'."
  :type '(list symbol)
  :group 'multi-edit)

;; common funcs

(defun multi-edit-current-prefix-is-negative ()
  (< (prefix-numeric-value current-prefix-arg) 0))

(defun multi-edit-bounds (bounds)
  (if (multi-edit-current-prefix-is-negative)   ;;; edit at begin
      (cons (cdr bounds) (car bounds))
    bounds))

(defun multi-edit--overlay-bounds (ol)
  (multi-edit-bounds (cons (overlay-start ol) (overlay-end ol))))

(defun multi-edit-bounds-of-thing-at-point (thing)
  (multi-edit-bounds
   (cl-case thing
     ((line-content)
      (multi-edit-bounds-of-line-at-point t))
     ((line)
      (multi-edit-bounds-of-line-at-point))
     (t
      (bounds-of-thing-at-point thing)))))

(defun multi-edit-bounds-of-line-at-point (&optional skip-whitespaces)
  (save-mark-and-excursion
    (multi-edit-beginning-of-line skip-whitespaces 1)
    (cons (point) (line-end-position))))

(defun multi-edit-beginning-of-line (&optional skip-whitespaces n)
  (beginning-of-line n)
  (when skip-whitespaces
    (skip-syntax-forward " \t" (line-end-position))))

(defun multi-edit--forward-thing (thing &optional n)
  (let* ((orig (point)))
    (cl-case thing
      ((line-content)
       (multi-edit--forward-line t n))
      ((line)
       (multi-edit--forward-line nil n))
      (t
       (forward-thing thing n)))
    (not (= orig (point)))))

(defun multi-edit--forward-line (&optional skip-whitespaces n)
  (if (or (not n) (> n 0))
      (progn
        (when (= (point) (line-end-position))
          (forward-line 1))
        (end-of-line n))
    (let* ((orig (point)))
      (multi-edit-beginning-of-line skip-whitespaces 1)
      (when (= (point) orig)
        (forward-line -1)))
    (multi-edit-beginning-of-line skip-whitespaces (- n))))

(defun multi-edit--re-search (regexp)
  (let ((case-fold-search nil))
    (when-let* ((match-data (if (multi-edit-current-prefix-is-negative)
                                (re-search-backward regexp nil t)
                              (re-search-forward regexp nil t))))
      (setq match-data (match-data))
      (multi-edit-bounds (cons (car match-data) (cadr match-data))))))

(defun multi-edit--narrow-to-secondary-selection-if-exist ()
  (when (secondary-selection-exist-p)
    (narrow-to-region (overlay-start mouse-secondary-overlay)
                      (overlay-end mouse-secondary-overlay))))

(defun multi-edit--cancel-secondary-selection ()
  (delete-overlay mouse-secondary-overlay))

;; overlays

(defvar multi-edit-after-add-overlays-hook nil)
(defvar multi-edit-overlay-change-finished-hook nil)
(defvar multi-edit-overlay-change-hook nil)
(defvar-local multi-edit--overlays nil)

(defun multi-edit--reset-overlays (thing &optional match)
  (multi-edit--remove-overlays)
  (when (and (secondary-selection-exist-p)
             (< (overlay-start mouse-secondary-overlay)
                (overlay-end mouse-secondary-overlay))
             (<= (overlay-start mouse-secondary-overlay)
                 (point)
                 (overlay-end mouse-secondary-overlay)))
    (if match
        (multi-edit--add-overlays-for-match match)
      (multi-edit--add-overlays-for-thing thing))
    (run-hooks 'multi-edit-after-add-overlays-hook)
    (run-hooks 'multi-edit-overlay-change-finished-hook)))

(defun multi-edit--remove-overlays ()
  (mapc #'delete-overlay multi-edit--overlays)
  (setq multi-edit--overlays nil)
  (run-hooks 'multi-edit-overlay-change-hook)
  (run-hooks 'multi-edit-overlay-change-finished-hook))

(defun multi-edit--add-overlays-for-thing (thing)
  (save-restriction
    (multi-edit--narrow-to-secondary-selection-if-exist)
    (save-mark-and-excursion
      (goto-char (point-min))
      (let (bounds)
        (while (multi-edit--forward-thing thing)
          (when (setq bounds (multi-edit-bounds-of-thing-at-point thing))
            (multi-edit--add-overlay-at-region (car bounds) (cdr bounds))))))))

(defun multi-edit--add-overlays-for-match (match)
  (save-restriction
    (multi-edit--narrow-to-secondary-selection-if-exist)
    (save-mark-and-excursion
      (goto-char (point-min))
      (let ((current-prefix-arg 1)
            bounds)
        (while (setq bounds (multi-edit--re-search match))
          (multi-edit--add-overlay-at-region (car bounds) (cdr bounds)))))))

(defun multi-edit--add-overlay-at-region (p1 p2)
  (let ((ol (make-overlay p1 p2)))
    (overlay-put ol 'multi-edit t)
    (if (= p1 p2)
        (overlay-put ol 'before-string (propertize "|" 'face 'region))
      (overlay-put ol 'face 'region))
    (overlay-put ol 'multi-edit-order (> p2 p1))
    (push ol multi-edit--overlays))
  (run-hooks 'multi-edit-overlay-change-hook))

(defun multi-edit--remove-overlay (ol)
  (when (overlayp ol)
    (delete-overlay ol))
  (setq multi-edit--overlays (cl-remove ol multi-edit--overlays))
  (run-hooks 'multi-edit-overlay-change-hook))

(defun multi-edit--overlays-exist ()
  (and multi-edit--overlays (> (length multi-edit--overlays) 0)))

(defun mult-edit--overlay> (a b)
  (> (overlay-start a) (overlay-start b)))

(defun multi-edit--get-last-overlay (&optional first)
  (setq multi-edit--overlays (sort multi-edit--overlays 'mult-edit--overlay>))
  (if first
      (car (last multi-edit--overlays))
    (car multi-edit--overlays)))

(defun multi-edit--overlay-distance< (a b)
  (cond ((not (overlayp a)) nil)
        ((not (overlayp b)) t)
        ((<= (overlay-start b) (point) (overlay-end b)) nil)
        ((<= (overlay-start a) (point) (overlay-end a)) t)
        (t (< (abs (- (overlay-start a) (point)))
              (abs (- (overlay-start b) (point)))))))

(defun multi-edit--get-nearest-overlay ()
  (let* ((overlays (if (multi-edit-current-prefix-is-negative)
                       (multi-edit--overlays-in (point-min) (1+ (point)))
                     (multi-edit--overlays-in (1- (point)) (point-max))))
         found ol)
    (while (setq ol (car overlays))
      (setq found (if (multi-edit--overlay-distance< ol found) ol found)
            overlays (cdr overlays)))
    found))

(defun multi-edit--overlays-in (beg end)
  (seq-filter (lambda (ol)
                (overlay-get ol 'multi-edit))
              (overlays-in beg (1- end))))

;; multi-edit-mode

(defvar multi-edit-mode-keymap
  (let ((keymap (make-keymap)))
    (define-key keymap (kbd "C-;") 'multi-edit-guide-mode-toggle)
    (define-key keymap (kbd "C->") 'multi-edit-quick-select)
    (define-key keymap (kbd "C-<") 'multi-edit-quick-cancel)
    keymap)
  "Keymap for `multi-edit-mode'")

(define-minor-mode multi-edit-mode
  "Multi edit mode. "
  :init-value t
  :keymap multi-edit-mode-keymap)

(defun multi-edit-quit ()
  (multi-edit--remove-overlays)
  (multi-edit--cancel-secondary-selection)
  (deactivate-mark t))

;; multi-edit-guide-mode

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
  "Keymap for `multi-edit-guide-mode'.")

(define-minor-mode multi-edit-guide-mode
  "Multi edit guide mode. "
  :lighter (:eval (format multi-edit-mode-line-lighter-format "G"))
  :keymap multi-edit-guide-mode-keymap)

(defun multi-edit-guide-mode-toggle (n)
  (interactive "p")
  (if multi-edit-guide-mode
      (progn
        (multi-edit-quit)
        (multi-edit-guide-mode -1))
    (multi-edit-guide-mode 1)
    (multi-edit-grab n)))

(defun multi-edit-grab (n)
  "Create secondary selection if no region available."
  (interactive "p")
  (if (and (overlay-buffer mouse-secondary-overlay)
           (not (region-active-p)))
      (multi-edit--cancel-secondary-selection)
    (multi-edit--guess-secondary-selection n))
  (deactivate-mark t))

(defun multi-edit--guess-secondary-selection (n)
  (cond ((region-active-p)
         (move-overlay mouse-secondary-overlay (region-beginning) (region-end)))
        ((> n 1)
         (move-overlay mouse-secondary-overlay (line-beginning-position (- 2 n)) (line-end-position n)))
        (t
         (when-let* ((bounds
                      (cl-case (car multi-edit-grab-range-list)
                        ((defun)
                         (bounds-of-thing-at-point 'defun))
                        ((paragraph)
                         (bounds-of-thing-at-point 'paragraph))
                        (t
                         (cons (point-min) (point-max))))))
           (move-overlay mouse-secondary-overlay (car bounds) (cdr bounds))))))

(defun multi-edit-try-grab ()
  (interactive)
  (let* ((first (car multi-edit-grab-range-list)))
    (setq multi-edit-grab-range-list (cl-remove first multi-edit-grab-range-list))
    (add-to-list 'multi-edit-grab-range-list first t))
  (multi-edit--guess-secondary-selection 1))

(defun multi-edit-mark-word (_)
  "Mark current word under cursor.
Use negative argument to create a backward selection."
  (interactive "p")
  (multi-edit--mark-thing 'word "\\<%s\\>"))

(defun multi-edit-mark-symbol (_)
  (interactive "p")
  (multi-edit--mark-thing 'symbol "\\_<%s\\_>"))

(defun multi-edit-mark-sexp (_)
  (interactive "p")
  (multi-edit--mark-thing 'sexp))

(defun multi-edit-mark-number (_)
  (interactive "p")
  (multi-edit--mark-thing 'number))

(defun multi-edit-mark-defun (_)
  (interactive "p")
  (multi-edit--mark-thing 'defun))

(defun multi-edit-mark-line (_)
  (interactive "p")
  (multi-edit--mark-thing 'line))

(defun multi-edit-mark-line-content (_)
  (interactive "p")
  (multi-edit--mark-thing 'line-content))

(defun multi-edit--mark-thing (thing &optional format-str)
  (let* ((bounds (multi-edit-bounds-of-thing-at-point thing))
         (beg (car bounds))
         (end (cdr bounds)))
    (when beg
      (let ((search (format (or format-str "%s")
                            (regexp-quote (buffer-substring-no-properties beg end)))))
        (unless (string-equal search (car regexp-search-ring))
          (add-to-history 'regexp-search-ring search regexp-search-ring-max))
        (multi-edit--reset-overlays thing search)))))

(defun multi-edit-mark-all-word (_)
  "Mark all word under cursor.
Use negative argument to create a backward selection."
  (interactive "p")
  (multi-edit--mark-all-thing 'word))

(defun multi-edit-mark-all-symbol (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'symbol))

(defun multi-edit-mark-all-sexp (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'sexp))

(defun multi-edit-mark-all-number (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'number))

(defun multi-edit-mark-all-defun (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'defun))

(defun multi-edit-mark-all-line (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'line))

(defun multi-edit-mark-all-line-content (_)
  (interactive "p")
  (multi-edit--mark-all-thing 'line-content))

(defun multi-edit--mark-all-thing (thing)
  (multi-edit--reset-overlays thing))

(defun multi-edit-search (_)
  "Read a regexp from minibuffer, then search and select it."
  (interactive "p")
  (let* ((beg (if (secondary-selection-exist-p)
                  (overlay-start mouse-secondary-overlay)
                (point-min)))
         (end (if (secondary-selection-exist-p)
                  (overlay-end mouse-secondary-overlay)
                (point-max)))
         (text (multi-edit--prompt-symbol-and-words
                "Search for: "
                beg end)))
    (unless (string-equal text (car regexp-search-ring))
      (add-to-history 'regexp-search-ring text regexp-search-ring-max))
    (multi-edit--reset-overlays 'search text)))

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

(defun multi-edit--adjust-edit-pos ()
  (when-let* ((ol (multi-edit--get-nearest-overlay))
              (bounds (multi-edit--overlay-bounds ol)))
    (goto-char (cdr bounds))))

(add-hook 'multi-edit-after-add-overlays-hook 'multi-edit--adjust-edit-pos)

;; multi-edit-quick-select-mode

(defvar multi-edit-quick-select-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-;") 'multi-edit-quick-select-mode)
    (define-key keymap (kbd "C-g") 'multi-edit-quick-select-mode)
    keymap)
  "Keymap for `multi-edit-quick-select-mode'.")

(define-minor-mode multi-edit-quick-select-mode
  "Multi edit quick select mode. "
  :lighter (:eval (format multi-edit-mode-line-lighter-format "Q"))
  :keymap multi-edit-quick-select-mode-keymap
  (if multi-edit-quick-select-mode
      (progn
        (add-hook 'pre-command-hook 'multi-edit-quick-select-actions nil t)
        (multi-edit-set-mode-line))
    (remove-hook 'pre-command-hook 'multi-edit-quick-select-actions t)
    (multi-edit-remove-mode-line)))

(defvar multi-edit--quick-select-column nil)
(defvar multi-edit-quick-select-commands '(multi-edit-quick-select multi-edit-quick-cancel multi-edit-quick-select-mode
                                                                   universal-argument universal-argument-more negative-argument digit-argument

                                                                   previous-line next-line execute-extended-command)
  "Which commands should not change to `multi-edit-action-mode'.")

(defun multi-edit-quick-select-actions ()
  (when (equal this-command 'multi-edit-quick-select-mode)
    (multi-edit-quit))
  (unless (memq this-command multi-edit-quick-select-commands)
    (multi-edit-action-mode 1)
    (setq this-command #'ignore)
    (push last-input-event unread-command-events)))

(defun multi-edit-quick-select (n)
  (interactive "p")
  (multi-edit--quick-select-initialize)
  (when-let* ((ol (multi-edit--get-last-overlay (< n 0)))
              (bounds (multi-edit--overlay-bounds ol)))
    (goto-char (cdr bounds))
    (let* ((match (format "%s" (regexp-quote (buffer-substring-no-properties (car bounds) (cdr bounds)))))
           (times 0))
      (while (and bounds (< times (abs n)))
        (if (not multi-edit--quick-select-column)
            (setq bounds (multi-edit--re-search match))
          (forward-line (if (< n 0) -1 1))
          (move-to-column multi-edit--quick-select-column)
          (setq bounds (cons (point) (point))))
        (cl-incf times)
        (multi-edit--add-overlay-at-region (car bounds) (cdr bounds))))))

(defun multi-edit--quick-select-initialize ()
  (multi-edit-quick-select-mode 1)
  (when (not (multi-edit--overlays-exist))
    (if (region-active-p)
        (multi-edit--add-overlay-at-region (region-beginning) (region-end))
      (multi-edit--add-overlay-at-region (point) (point)))
    (setq multi-edit--quick-select-column (unless (region-active-p) (current-column)))
    (deactivate-mark t)))

(defun multi-edit-quick-cancel (n)
  (interactive "p")
  (if (or (not multi-edit-quick-select-mode) (not (multi-edit--overlays-exist)))
      (error "quick select mode is not active!")
    (when-let* ((ol (multi-edit--get-nearest-overlay))
                (bounds (multi-edit--overlay-bounds ol)))
      (goto-char (car bounds))
      (let* ((match (format "%s" (regexp-quote (buffer-substring-no-properties (car bounds) (cdr bounds)))))
             (times 0)
             (current-prefix-arg (- n)))
        (while (and bounds (< times (abs n)))
          (multi-edit--remove-overlay ol)
          (if (not multi-edit--quick-select-column)
              (setq bounds (multi-edit--re-search match))
            (forward-line (if (< n 0) 1 -1))
            (move-to-column multi-edit--quick-select-column)
            (setq bounds (cons (point) (point))))
          (setq ol (multi-edit--get-nearest-overlay))
          (cl-incf times))))))

;; multi-edit-action-mode

(defvar multi-edit-action-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "C-;") 'multi-edit-action-mode)
    (define-key keymap (kbd "C-g") 'multi-edit-action-mode)
    keymap)
  "Keymap for `multi-edit-action-mode'.")

(define-minor-mode multi-edit-action-mode
  "Multi edit action mode. "
  :lighter (:eval (format multi-edit-mode-line-lighter-format "A"))
  :keymap multi-edit-action-mode-keymap
  (if multi-edit-action-mode
      (progn
        (multi-edit-set-mode-line)
        (cond (multi-edit-guide-mode
               (multi-edit-guide-mode -1)
               (multi-edit--maybe-start-macro))
              (multi-edit-quick-select-mode
               (multi-edit-quick-select-mode -1)
               (multi-edit--maybe-start-macro))))
    (multi-edit-remove-mode-line)
    (multi-edit-apply-modification)
    (if (secondary-selection-exist-p)
        (multi-edit-guide-mode 1)
      (multi-edit-quit))))

(add-hook 'multi-edit-after-add-overlays-hook 'multi-edit-action-mode)

(defvar multi-edit--last-undo-length nil
  "Easy undo after multi edit.")
(defvar-local multi-edit--last-modify-overlay nil)

(defun multi-edit--is-last-modify-overlay (ol)
  (equal ol multi-edit--last-modify-overlay))

(defun multi-edit--maybe-start-macro ()
  (when (and (not defining-kbd-macro)
             (not executing-kbd-macro))
    (setq multi-edit--last-undo-length (length buffer-undo-list))
    (setq multi-edit--last-modify-overlay (multi-edit--get-nearest-overlay))
    (funcall 'kmacro-start-macro nil)))

(defun multi-edit-apply-modification ()
  (interactive)
  (when defining-kbd-macro
    (end-kbd-macro))
  (atomic-change-group
    (save-mark-and-excursion
      (let* ((overlays (reverse multi-edit--overlays))
             skip-overlays
             ol
             found)
        (while (and (not found) (setq ol (car overlays)))
          (setq found (multi-edit--is-last-modify-overlay ol)
                overlays (cdr overlays))
          (push ol skip-overlays))
        (cl-loop for ol in (append overlays (reverse (cdr skip-overlays)))
                 when (overlayp ol)
                 do (let* ((order (overlay-get ol 'multi-edit-order)))
                      (goto-char (if order (overlay-end ol) (overlay-start ol)))
                      (funcall 'kmacro-call-macro nil))))
      (setq multi-edit--last-undo-length (- (length buffer-undo-list)
                                            multi-edit--last-undo-length))
      (setq buffer-undo-list
            (cl-loop for v from 1 to multi-edit--last-undo-length
                     with list = buffer-undo-list
                     unless (eq (car list) nil)
                     collect (car list) into new-list
                     do (setq list (cdr list))
                     finally return (append new-list list)))))
  (multi-edit--remove-overlays))

;; mode line

(defconst multi-edit--mode-line-format '(:eval (multi-edit-mode-line-str)))

(defun multi-edit-mode-line-str ()
  (or
   (when (multi-edit--overlays-exist)
     (let* ((ol (multi-edit--get-nearest-overlay))
            (before (multi-edit--overlays-in (point-min) (or (and (overlayp ol) (overlay-start ol)) (point))))
            (after (multi-edit--overlays-in (or (and (overlayp ol) (1+ (overlay-end ol))) (point)) (point-max)))
            (count (+ (length before) (if (overlayp ol) 1 0))))
       (format " %d/%d " count (+ count (length after)))))
   ""))

(defun multi-edit-set-mode-line ()
  (when (and multi-edit-set-mode-line-p (not (member multi-edit--mode-line-format mode-line-format)))
    (setq mode-line-format (cons multi-edit--mode-line-format mode-line-format))))

(defun multi-edit-remove-mode-line ()
  (when (and multi-edit-set-mode-line-p (member multi-edit--mode-line-format mode-line-format))
    (setq mode-line-format (delete multi-edit--mode-line-format mode-line-format))))

(provide 'init-multi-edit)

;;; init-multi-edit.el ends here
