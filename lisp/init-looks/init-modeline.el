(require 'init-multi-edit)
(use-package doom-modeline
  :preface
  (defun doom-modeline--multi-edit ()
    (when (and (doom-modeline--active)
               multi-edit-need-update-mode-line)
      (propertize
       (multi-edit--update-mode-line)
       'face (doom-modeline-face 'doom-modeline-panel))))
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq multi-edit-set-mode-line-p nil)

  (doom-modeline-def-segment custom-matches
    "Displays multi edit."
    (let ((meta (concat (doom-modeline--macro-recording)
                        (doom-modeline--anzu)
                        (doom-modeline--phi-search)
                        (doom-modeline--evil-substitute)
                        (doom-modeline--iedit)
                        (doom-modeline--symbol-overlay)
                        (doom-modeline--multiple-cursors)
                        (doom-modeline--multi-edit))))
      (or (and (not (string-empty-p meta)) meta)
          (doom-modeline--buffer-size))))

  (declare-function doom-modeline-def-modeline "doom-modeline")
  (doom-modeline-def-modeline 'custom-modeline
    '(bar workspace-name window-number modals custom-matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
    '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check))

  (declare-function doom-modeline-set-modeline "doom-modeline")
  (add-hook 'doom-modeline-mode-hook
            (lambda ()
              (if doom-modeline-mode
                  (progn
                    (doom-modeline-set-modeline 'custom-modeline 'default)
                    (dolist (buf (buffer-list))
                      (with-current-buffer buf
                        (doom-modeline-set-modeline 'custom-modeline))))))))

(provide 'init-modeline)
