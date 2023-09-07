(provide 'init-thing-at-point)

(use-package thingatpt
  :ensure nil
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
    (when-let ((sym (with-minibuffer-selected-window (symbol-at-point))))
      (insert (symbol-name sym))))
  (defun yank-word-at-point ()
    "Put word at current point into buffer."
    (interactive)
    (when-let (sym (with-minibuffer-selected-window (word-at-point)))
      (insert sym)))
  :config
  (put 'number 'forward-op 'forward-number)
  (global-set-key (kbd "C-M-y") 'yank-symbol-at-point)
  (global-set-key (kbd "C-M-Y") 'yank-word-at-point))
