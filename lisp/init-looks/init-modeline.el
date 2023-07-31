(use-package doom-modeline
  :defines multi-edit-set-mode-line-p multi-edit-need-update-mode-line
  :functions doom-modeline-def-modeline doom-modeline--active multi-edit--update-mode-line doom-modeline-face
  :custom
  (doom-modeline-icon nil)
  (doom-modeline-minor-modes t)
  (doom-modeline-enable-word-count t)
  :hook
  (after-init . doom-modeline-mode)
  :config
  (setq multi-edit-set-mode-line-p nil)

  (defun doom-modeline--multi-edit ()
    (when (and (doom-modeline--active)
               multi-edit-need-update-mode-line)
      (propertize
       (multi-edit--update-mode-line)
       'face (doom-modeline-face 'doom-modeline-panel))))

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

  (doom-modeline-def-modeline 'custom-modeline
  '(bar workspace-name window-number modals custom-matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
  '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs checker time))

  (add-hook 'doom-modeline-mode-hook
          (lambda ()
            (doom-modeline-set-modeline 'custom-modeline 'default))))

(provide 'init-modeline)
