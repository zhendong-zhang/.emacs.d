(use-package window-numbering
  :config
  (window-numbering-mode 1))

(use-package ace-window
  :bind
  ("C-x o" . ace-window)
  :config
  (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)))

(use-package buffer-move
  :bind
  (("<C-S-up>" . buf-move-up)
   ("<C-S-down>" . buf-move-down)
   ("<C-S-left>" . buf-move-left)
   ("<C-S-right>" . buf-move-right)))

;;----------------------------------------------------------------------------
;; Navigate window layouts with "C-c <left>" and "C-c <right>"
;;----------------------------------------------------------------------------
(use-package winner
  :demand
  :bind
  (("M-g <left>" . winner-undo)
   ("M-g <right>" . winner-redo))
  :config
  (winner-mode 1))

;;----------------------------------------------------------------------------
;; When splitting window, show (other-buffer) in the new window
;;----------------------------------------------------------------------------
(defun split-window-with-other-buffer (split-function)
  (funcall split-function)
  (set-window-buffer (next-window) (other-buffer)))

(defun split-window-vertically-with-other-buffer ()
  (interactive)
  (split-window-with-other-buffer 'split-window-vertically))

(defun split-window-horizontally-with-other-buffer ()
  (interactive)
  (split-window-with-other-buffer 'split-window-horizontally))

(global-set-key "\C-x2" 'split-window-vertically-with-other-buffer)
(global-set-key "\C-x3" 'split-window-horizontally-with-other-buffer)
(global-set-key [C-_] 'split-window-vertically-with-other-buffer)
(global-set-key [C-|] 'split-window-horizontally-with-other-buffer)

;;----------------------------------------------------------------------------
;; Rearrange split windows
;;----------------------------------------------------------------------------
(defun split-window-horizontally-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (split-window-horizontally-with-other-buffer)))

(defun split-window-vertically-instead ()
  (interactive)
  (save-excursion
    (delete-other-windows)
    (split-window-vertically-with-other-buffer)))

(global-set-key "\C-x|" 'split-window-horizontally-instead)
(global-set-key "\C-x_" 'split-window-vertically-instead)

(defun display-buffer-2-windows (buffer alist)
  "If only one window is available split it and display BUFFER there.
ALIST is the option channel for display actions (see `display-buffer')."
  (when (eq (length (window-list nil 'no-minibuf)) 1)
    (display-buffer--maybe-pop-up-window buffer alist)))

(setq display-buffer-base-action
      '((display-buffer--maybe-same-window
         display-buffer-reuse-window
         display-buffer--maybe-pop-up-frame
         display-buffer-2-windows
         display-buffer-in-previous-window
         display-buffer-use-some-window
         display-buffer-pop-up-frame)))

(setq-default split-height-threshold nil)

(provide 'init-windows)
