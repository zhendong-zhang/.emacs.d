;;; init-easy-nav.el --- 浏览模式 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(defvar easy-nav-mode-keymap-for-prev
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "w") #'backward-word)
    (define-key keymap (kbd "s") #'isearch-backward-regexp)
    (define-key keymap (kbd "v") #'scroll-down-command)
    (define-key keymap (kbd "f") #'beginning-of-defun)
    (define-key keymap (kbd "b") #'previous-buffer)
    keymap))

(defvar easy-nav-mode-keymap-for-next
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "w") #'forward-word)
    (define-key keymap (kbd "s") #'isearch-forward-regexp)
    (define-key keymap (kbd "v") #'scroll-up-command)
    (define-key keymap (kbd "f") #'end-of-defun)
    (define-key keymap (kbd "b") #'next-buffer)
    keymap))

(defvar easy-nav-mode-keymap
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap (kbd "p") #'previous-line)
    (define-key keymap (kbd "n") #'next-line)
    (define-key keymap (kbd "<") #'beginning-of-buffer)
    (define-key keymap (kbd ">") #'end-of-buffer)
    (define-key keymap (kbd "d") #'scroll-down)
    (define-key keymap (kbd "SPC") #'scroll-up)
    (define-key keymap (kbd "f") #'forward-char)
    (define-key keymap (kbd "b") #'backward-char)
    (define-key keymap (kbd "a") #'move-beginning-of-line)
    (define-key keymap (kbd "e") #'move-end-of-line)
    (define-key keymap (kbd "'") #'avy-goto-char-timer)
    (define-key keymap (kbd "i") #'consult-imenu)
    (define-key keymap (kbd "o") #'ff-find-other-file)
    (define-key keymap (kbd ".") #'embark-dwim)
    (define-key keymap (kbd ",") #'xref-go-back)
    (define-key keymap (kbd "[") easy-nav-mode-keymap-for-prev)
    (define-key keymap (kbd "]") easy-nav-mode-keymap-for-next)
    (define-key keymap (kbd "q") #'easy-nav-global-mode)
    keymap)
  "Global keymap for easy nav mode.")

(defun easy-nav-add-to-repeat-map (keymap)
  (map-keymap
   (lambda (_key cmd)
     (put cmd 'repeat-map keymap))
   (symbol-value keymap)))

(defun easy-nav-rm-from-repeat-map (keymap)
  (map-keymap
   (lambda (_key cmd)
     (put cmd 'repeat-map nil))
   (symbol-value keymap)))

(defun easy-nav-mode-on ()
  (interactive)
  (unless (or (minibufferp)
              (derived-mode-p 'special-mode 'view-mode 'comint-mode 'fundamental-mode))
    ;; (message "easy nav mode trun on %s %s" (buffer-name) major-mode)
    (easy-nav-mode t)))

(define-minor-mode easy-nav-mode
  "Easy nav mode. "
  :init-value nil
  :lighter "[N]"
  :keymap easy-nav-mode-keymap
  :group 'easy-nav-mode
  (if easy-nav-mode
      (progn
        (read-only-mode 1)
        (easy-nav-add-to-repeat-map 'easy-nav-mode-keymap-for-prev)
        (easy-nav-add-to-repeat-map 'easy-nav-mode-keymap-for-next))
    (read-only-mode -1)
    (easy-nav-rm-from-repeat-map 'easy-nav-mode-keymap-for-prev)
    (easy-nav-rm-from-repeat-map 'easy-nav-mode-keymap-for-next)))
(define-globalized-minor-mode easy-nav-global-mode easy-nav-mode easy-nav-mode-on
  :group 'easy-nav-mode)

(define-key global-map (kbd "C-z") 'easy-nav-global-mode)

(provide 'init-easy-nav)

;;; init-easy-nav.el ends here
