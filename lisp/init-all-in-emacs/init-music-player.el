(provide 'init-music-player)

(use-package emms
  :hook ((emms-player-started . emms-show)
         (kill-emacs . my-emms-history-save))
  :commands emms emms-history-load
  :defines emms-tag-editor-rename-format emms-playlist-mode-map
  :custom
  (emms-lyrics-scroll-p nil)
  (emms-info-functions '(emms-info-native))
  (emms-player-list '(emms-player-mplayer))
  (emms-playlist-buffer-name "*Music*")
  (emms-repeat-playlist t)
  :preface
  (defun my-emms-history-save ()
    (when (featurep 'emms)
      (emms-history-save)))
  (defun emms-delete-file-from-disk ()
    "Delete this file from disk."
    (interactive)
    (emms-with-inhibit-read-only-t
     (let ((file (emms-track-get (emms-playlist-track-at) 'name)))
       (when (and file
                  (y-or-n-p (format "Are you really want to delete \' %s \' from disk? " file)))
         (when (and emms-player-playing-p
                    (emms-playlist-selected-track-at-p))
           (emms-stop))
         (emms-playlist-mode-kill-entire-track)
         (dired-delete-file file)
         (message (format "Have delete \' %s \' from disk." file))))))

  (defun my-emms-tag-editor-rename-track (track &optional dont-apply)
    (if (emms-track-file-p track)
        (let* ((old-file (emms-track-name track))
               (path     (file-name-directory old-file))
               (suffix   (file-name-extension old-file))
               (new-file (concat
                          path
                          (format-spec
                           emms-tag-editor-rename-format
                           (apply #'format-spec-make
                                  (apply #'append
                                         (mapcar
                                          (lambda (tag)
                                            (list (string-to-char (cdr tag))
                                                  (string-replace "/" ","
                                                                  (or (emms-track-get track (car tag))
                                                                      ""))))
                                          emms-tag-editor-tags))))
                          "." suffix)))
          (emms-track-set track 'newname new-file)
          (emms-track-set track 'tag-modified t)
          (unless dont-apply
            (emms-tag-editor-apply (list track))))
      (message "Only files can be renamed.")))
  :bind (:map emms-playlist-mode-map
         ("D" . emms-delete-file-from-disk))
  :config
  (require 'emms-setup)
  (push "aac" emms-player-base-format-list)
  (put 'emms-browser-delete-files 'disabled nil)
  (setq-default emms-tag-editor-rename-format "%t - %a")
  (advice-add 'emms-tag-editor-rename-track :override #'my-emms-tag-editor-rename-track)
  (emms-all)
  (emms-history-load))
