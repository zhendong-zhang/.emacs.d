;;; init-music-player.el --- 音乐播放器 -*- lexical-binding: t -*-

;; Author: (zhendong zhendong.zhang.zh@gmail.com)

;;; Code:

(use-package emms
  :hook ((emms-player-started . emms-show)
         (kill-emacs . my-emms-history-save))
  :commands emms emms-history-load emms-history-save emms-stop
  :bind (:map emms-playlist-mode-map
              ("D" . emms-delete-file-from-disk))
  :defines emms-playlist-mode-map
  :functions emms-track-get emms-playlist-track-at emms-playlist-selected-track-at-p emms-playlist-mode-kill-entire-track emms-all emms-track-simple-description
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

  :config
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
         (delete-file file)
         (message (format "Have delete \' %s \' from disk." file))))))

  (defun emms-info-track-simple-description (track)
    "Return a simple description of TRACK."
    (let ((artist    (emms-track-get track 'info-artist))
          (title     (emms-track-get track 'info-title)))
      (cond
       ((and artist title)
        (concat artist " - " title))
       (title
        title)
       (t
        (emms-track-simple-description track)))))

  (require 'emms-setup)
  (push "aac" emms-player-base-format-list)
  (put 'emms-browser-delete-files 'disabled nil)
  (setq-default emms-tag-editor-rename-format "%t - %a")
  (emms-all)
  (setq emms-track-description-function 'emms-info-track-simple-description)
  (setq emms-playlist-default-major-mode 'emms-mark-mode)
  (emms-history-load))

(provide 'init-music-player)

;;; init-music-player.el ends here
