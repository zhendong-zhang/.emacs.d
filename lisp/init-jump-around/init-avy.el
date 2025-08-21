;;; init-avy.el --- 快速移动光标 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(require 'ring)

(defun get-available-avy-keys (dispatch-alist)
  (let ((keys '(?q ?w ?e ?r ?u ?i ?o ?p ?a ?s ?d ?f ?g ?h ?j ?k ?l ?\;)))
    (dolist (x dispatch-alist)
      (when (memq (car x) keys)
        (setq keys (delq (car x) keys))))
    keys))

(use-package avy
  :functions avy-action-copy-whole-line avy-action-kill-whole-line flyspell-auto-correct-word
  :custom
  (avy-case-fold-search nil)
  (avy-style 'at)
  (avy-dispatch-alist '((?k . avy-action-kill-stay)
                        (?K . avy-action-kill-whole-line)
                        (?w . avy-action-copy)
                        (?W . avy-action-copy-whole-line)
                        (?y . avy-action-yank)
                        (?Y . avy-action-yank-whole-line)
                        (?t . avy-action-teleport)
                        (?T . avy-action-teleport-whole-line)
                        (?m . avy-action-mark)
                        (?  . avy-action-mark-to-char)
                        (?\; . avy-action-flyspell)
                        (?z . avy-action-zap-to-char)
                        (?. . avy-action-embark)))
  :bind
  (("M-g '" . avy-goto-char-timer)
   ("C-'" . avy-goto-char-timer)
   :map isearch-mode-map
   ("C-'" . avy-isearch))
  :config
  (setq avy-keys (get-available-avy-keys avy-dispatch-alist))
  ;; https://karthinks.com/software/avy-can-do-anything/#avy-actions
  (defun avy-action-kill-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (kill-whole-line))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-copy-whole-line (pt)
    (save-excursion
      (goto-char pt)
      (cl-destructuring-bind (start . end)
          (bounds-of-thing-at-point 'line)
        (copy-region-as-kill start end)))
    (select-window
     (cdr
      (ring-ref avy-ring 0)))
    t)

  (defun avy-action-yank-whole-line (pt)
    (avy-action-copy-whole-line pt)
    (save-excursion (yank))
    t)

  (defun avy-action-teleport-whole-line (pt)
    (avy-action-kill-whole-line pt)
    (save-excursion (yank)) t)

  (defun avy-action-mark-to-char (pt)
    (activate-mark)
    (goto-char pt))

  (defun avy-action-flyspell (pt)
    (save-excursion
      (goto-char pt)
      (when (require 'flyspell nil t)
        (flyspell-auto-correct-word)))
    (select-window
     (cdr (ring-ref avy-ring 0)))
    t)

  (defun avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  (use-package ace-pinyin
    :diminish ace-pinyin-mode
    :functions ace-pinyin--build-string-regexp
    :config
    (eval-when-compile
      (declare-function pinyinlib-build-regexp-string "pinyinlib")
      (declare-function avy--read-candidates "avy"))

    (defun ace-pinyin--build-string-regexp (string)
      (pinyinlib-build-regexp-string string
                                     (not ace-pinyin-enable-punctuation-translation)
                                     (not ace-pinyin-simplified-chinese-only-p)))

    (defun ace-pinyin-goto-char-timer (&optional arg)
      "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
      (interactive "P")
      (let ((avy-all-windows (if arg
                                 (not avy-all-windows)
                               avy-all-windows)))
        (avy-with avy-goto-char-timer
          (setq avy--old-cands
                (avy--read-candidates #'ace-pinyin--build-string-regexp))
          (avy-process avy--old-cands))))

    (fset 'avy-goto-char-timer 'ace-pinyin-goto-char-timer)

    (ace-pinyin-global-mode 1)))

(provide 'init-avy)

;;; init-avy.el ends here
