(provide 'init-thing-at-point)

(use-package thingatpt
  :ensure nil
  :config
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

  (put 'number 'forward-op 'forward-number))
