(use-package operate-on-number
  :bind
  (("C-c n +" . apply-operation-to-number-at-point)
   ("C-c n =" . apply-operation-to-number-at-point)
   ("C-c n _" . apply-operation-to-number-at-point)
   ("C-c n -" . apply-operation-to-number-at-point)
   ("C-c n *" . apply-operation-to-number-at-point)
   ("C-c n /" . apply-operation-to-number-at-point)
   ("C-c n \\" . apply-operation-to-number-at-point)
   ("C-c n ^" . apply-operation-to-number-at-point)
   ("C-c n <" . apply-operation-to-number-at-point)
   ("C-c n >" . apply-operation-to-number-at-point)
   ("C-c n #" . apply-operation-to-number-at-point)
   ("C-c n %" . apply-operation-to-number-at-point)
   ("C-c n '" . operate-on-number-at-point))
  :config
  (add-to-list 'operate-on-number-at-point-alist '(?= (1) +))
  (add-to-list 'operate-on-number-at-point-alist '(?_ (1) -)))

(provide 'init-operate-on-number)
