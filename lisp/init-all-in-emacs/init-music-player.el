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

  :bind (:map emms-playlist-mode-map
         ("D" . emms-delete-file-from-disk))
  :config
  (require 'emms-setup)
  (push "aac" emms-player-base-format-list)
  (put 'emms-browser-delete-files 'disabled nil)
  (setq-default emms-tag-editor-rename-format "%t - %a")
  (emms-all)
  (emms-history-load))

