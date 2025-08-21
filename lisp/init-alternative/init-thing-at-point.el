;;; init-thing-at-point.el --- thingatpt -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package thingatpt
  :ensure nil
  :functions word-at-point
  :preface
  (defun forward-number (arg)
    "Move point to the next position that is the end of a number.
With prefix argument ARG, do it ARG times if positive, or move
backwards ARG times if negative."
    (interactive "^p")
    (if (natnump arg)
        (re-search-forward "\\(\\(0x\\|#x\\)\\([a-fA-F0-9]+\\)\\)\\|\\(-?[0-9]+\\.?[0-9]*\\)" nil 'move arg)
      (while (< arg 0)
        (if (re-search-backward "\\(\\(0x\\|#x\\)\\([a-fA-F0-9]+\\)\\)\\|\\(-?[0-9]+\\.?[0-9]*\\)" nil 'move)
            (skip-chars-backward "0-9a-fA-Fx\-"))
        (setq arg (1+ arg)))))

  (defun yank-symbol-at-point ()
    "Put symbol at current point into minibuffer."
    (interactive)
    (when-let* ((str (with-minibuffer-selected-window
                       (if (use-region-p)
                           (thing-at-point 'region)
                         (symbol-name (symbol-at-point))))))
      (insert str)))

  (defun yank-word-at-point ()
    "Put word at current point into buffer."
    (interactive)
    (when-let* ((str (with-minibuffer-selected-window
                       (if (use-region-p)
                           (thing-at-point 'region)
                         (word-at-point)))))
      (insert str)))
  :config
  (put 'number 'forward-op 'forward-number)
  (global-set-key (kbd "C-M-y") 'yank-symbol-at-point)
  (global-set-key (kbd "C-M-S-y") 'yank-word-at-point))

(provide 'init-thing-at-point)

;;; init-thing-at-point.el ends here
