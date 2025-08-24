;;; init-modeline.el --- modeline配置 -*- lexical-binding: t -*-

;; Author: zhendong <zhendong.zhang.zh@gmail.com>

;;; Code:

(use-package init-multi-edit :ensure nil
  :functions multi-edit-mode-line-str
  :custom (multi-edit-set-mode-line-p nil)
  :config
  (use-package doom-modeline
    :functions doom-modeline--active doom-modeline-face doom-modeline-def-modeline doom-modeline-set-modeline
    :custom
    (doom-modeline-icon nil)
    (doom-modeline-minor-modes t)
    (doom-modeline-enable-word-count t)
    :hook
    (after-init . doom-modeline-mode)
    :preface
    (defun doom-modeline--multi-edit ()
      (when (doom-modeline--active)
        (propertize
         (multi-edit-mode-line-str)
         'face (doom-modeline-face 'doom-modeline-panel))))
    :config
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
      '(bar modals custom-matches follow buffer-info remote-host buffer-position word-count parrot selection-info)
      '(compilation objed-state misc-info persp-name battery grip irc mu4e gnus github debug repl lsp minor-modes input-method indent-info buffer-encoding major-mode process vcs check))

    (add-hook 'doom-modeline-mode-hook
              (lambda ()
                (if doom-modeline-mode
                    (progn
                      (doom-modeline-set-modeline 'custom-modeline 'default)
                      (dolist (buf (buffer-list))
                        (with-current-buffer buf
                          (doom-modeline-set-modeline 'custom-modeline)))))))))

(provide 'init-modeline)

;;; init-modeline.el ends here
