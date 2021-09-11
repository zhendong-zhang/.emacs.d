(use-package comment-dwim-2)

(use-package newcomment
  :ensure nil
  :init
  (setq comment-style 'multi-line))

;; intend to fix comments are different in c-mode and c++-mode
(defvar smarter-comment-styles
  '(
    ;; mode
    (c-mode
     ;; comment-start/end when region is active
     "/* " " */"
     ;; comment-continue, not empty string
     " * "
     ;; comment-start/end when region is not active
     "//" "")
    (c++-mode
     "/* " " */"
     " * "
     "//" "")
    (lua-mode
     "--[[ " " ]]" "    "
     "--" "")
    ))

(defun smarter-call-comment-dwim-2 (&optional arg)
  (if (eq last-command 'smarter-comment-dwim)
      (setq last-command 'comment-dwim-2))
  (comment-dwim-2 arg))

(defun smarter-comment-dwim (&optional arg)
  "Comment with two style."
  (interactive "P")
  (let* ((old-comment-start comment-start)
         (old-comment-end comment-end)
         (old-comment-continue comment-continue))
    (dolist (var smarter-comment-styles)
      (let* ((mode (car var))
             (region-comment-start (nth 1 var))
             (region-comment-end (nth 2 var))
             (region-comment-continue (nth 3 var))
             (not-region-comment-start (nth 4 var))
             (not-region-comment-end (nth 5 var)))
        (when (eq mode major-mode)
          (if (use-region-p)
              (progn
                (setq-local comment-start region-comment-start)
                (setq-local comment-end region-comment-end)
                (setq-local comment-continue region-comment-continue))
            (setq-local comment-start not-region-comment-start)
            (setq-local comment-end not-region-comment-end)))))
    (smarter-call-comment-dwim-2 arg)
    (setq-local comment-start old-comment-start)
    (setq-local comment-end old-comment-end)
    (setq-local comment-continue old-comment-continue)))

;; M-;
(global-set-key [remap comment-dwim] 'smarter-comment-dwim)

(provide 'init-comment)
