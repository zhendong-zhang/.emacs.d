;; Use regex to search by default
(global-set-key (kbd "M-%") 'query-replace-regexp)
(global-set-key (kbd "C-M-%") 'query-replace)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Show number of matches while searching
(use-package anzu
  :diminish
  :demand
  :bind
  (([remap query-replace-regexp] . anzu-query-replace-regexp)
   ([remap query-replace] . anzu-query-replace))
  :config
  (setq anzu-cons-mode-line-p nil)
  (global-anzu-mode t))

;; replace whole buffer when no region select
(defun perform-replace-in-buffer (oldfun from-string replacements
                                         query-flag regexp-flag delimited-flag
                                         &optional repeat-count map start end backward region-noncontiguous-p)
  (if (and (not start) (not end))
      (apply oldfun from-string replacements query-flag regexp-flag delimited-flag repeat-count map (point-min) (point-max) backward region-noncontiguous-p)
    (apply oldfun from-string replacements query-flag regexp-flag delimited-flag repeat-count map start end backward region-noncontiguous-p)))
(advice-add 'perform-replace :around 'perform-replace-in-buffer)

;; DEL during isearch should edit the search string, not jump back to the previous result
(define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)

(require 'thingatpt)
(defun isearch-yank-symbol-at-point ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((sym (symbol-at-point)))
    (if sym
        (progn
          (setq isearch-string (symbol-name sym)
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))
(define-key isearch-mode-map "\C-w" 'isearch-yank-symbol-at-point)

(defun isearch-yank-word-at-point ()
  "*Put symbol at current point into search string."
  (interactive)
  (let ((word (word-at-point)))
    (if word
        (progn
          (setq isearch-string word
                isearch-message (mapconcat 'isearch-text-char-description isearch-string "")
                isearch-yank-flag t))
      (ding)))
  (isearch-search-and-update))
(define-key isearch-mode-map "\M-\C-w" 'isearch-yank-word-at-point)

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun zap-to-isearch (rbeg rend)
  "Kill the region between the mark and the closest portion of
the isearch match string. The behaviour is meant to be analogous
to zap-to-char; let's call it zap-to-isearch. The deleted region
does not include the isearch word. This is meant to be bound only
in isearch mode.  The point of this function is that oftentimes
you want to delete some portion of text, one end of which happens
to be an active isearch word. The observation to make is that if
you use isearch a lot to move the cursor around (as you should,
it is much more efficient than using the arrows), it happens a
lot that you could just delete the active region between the mark
and the point, not include the isearch word."
  (interactive "r")
  (when (not mark-active)
    (error "Mark is not active"))
  (let* ((isearch-bounds (list isearch-other-end (point)))
         (ismin (apply 'min isearch-bounds))
         (ismax (apply 'max isearch-bounds))
         )
    (if (< (mark) ismin)
        (kill-region (mark) ismin)
      (if (> (mark) ismax)
          (kill-region ismax (mark))
        (error "Internal error in isearch kill function.")))
    (isearch-exit)
    ))

(define-key isearch-mode-map [(meta z)] 'zap-to-isearch)

;; http://www.emacswiki.org/emacs/ZapToISearch
(defun isearch-exit-other-end (rbeg rend)
  "Exit isearch, but at the other end of the search string.
This is useful when followed by an immediate kill."
  (interactive "r")
  (isearch-exit)
  (goto-char isearch-other-end))

(define-key isearch-mode-map [(control return)] 'isearch-exit-other-end)

(provide 'init-isearch)
