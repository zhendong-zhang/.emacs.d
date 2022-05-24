(defun simple-buffer-encoding ()
  (let ((encoding (format "%s" buffer-file-coding-system)))
    (string-remove-prefix "prefer-" encoding)))

(defun simple-buffer-postion ()
  (require 'array)
  (concat (format "(%s:%s %s" (current-line) (current-column) (/ (* 100 (point)) (buffer-size))) "%%)")
  )

(use-package powerline
  :config
  (setq powerline-default-separator nil)
  (setq-default mode-line-format
                '("%e"
                  (:eval
                   (let* ((active (powerline-selected-window-active))
                          (mode-line-buffer-id (if active 'mode-line-buffer-id 'mode-line-buffer-id-inactive))
                          (face0 (if active 'powerline-active0 'powerline-inactive0))
                          (face1 (if active 'powerline-active1 'powerline-inactive1))
                          (face2 (if active 'powerline-active2 'powerline-inactive2))
                          (lhs (list (powerline-raw "%*" face0 'l)
                                     (powerline-buffer-id `(mode-line-buffer-id ,face0) 'l)
                                     (when (and (boundp 'which-func-mode) which-func-mode)
                                       (powerline-raw which-func-format face0 'l))
                                     (powerline-raw " " face0)
                                     (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                       (powerline-raw erc-modified-channels-object face1 'l))
                                     (powerline-major-mode face1 'l)
                                     (powerline-process face1)
                                     (powerline-minor-modes face1 'l)
                                     (powerline-narrow face1 'l)
                                     (powerline-raw " " face1)
                                     (powerline-vc face2 'l)
                                     (when (bound-and-true-p nyan-mode)
                                       (powerline-raw (list (nyan-create)) face2 'l))))
                          (rhs (list (unless window-system
                                       (powerline-raw (char-to-string #xe0a1) face1 'r))
                                     (powerline-raw (or current-input-method-title "ENG") face1 'r)
                                     (powerline-raw global-mode-string face1 'r)
                                     (powerline-raw (simple-buffer-postion) face1 'r)
                                     (powerline-raw (simple-buffer-encoding) face0 'r)
                                     )))
                     (concat (powerline-render lhs)
                             (powerline-fill face2 (powerline-width rhs))
                             (powerline-render rhs)))))))

(provide 'init-modeline)
