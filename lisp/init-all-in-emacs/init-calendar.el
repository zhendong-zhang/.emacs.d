(use-package cal-china-x
  :after (calendar)
  :defer-incrementally t
  :config
  (setq calendar-mark-holidays-flag t)
  (setq cal-china-x-important-holidays cal-china-x-chinese-holidays)
  (setq calendar-holidays
      (append cal-china-x-important-holidays
              cal-china-x-general-holidays)))

(provide 'init-calendar)
