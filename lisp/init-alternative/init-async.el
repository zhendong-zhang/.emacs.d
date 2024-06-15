(defvar async-current-window nil)

(defun package-list-packages-async()
  (interactive)
  (use-package async)
  (setq async-current-window (selected-window))
  (async-start
   (lambda ()
     (list-packages))
   (lambda (result)
     (save-excursion
       (when async-current-window
         (select-window async-current-window))
       (package-list-packages-no-fetch)))))

(provide 'init-async)
