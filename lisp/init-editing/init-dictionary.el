(use-package fanyi
  :bind
  ("M-s d" . fanyi-dwim))

(use-package corfu-english-helper
  :quelpa (corfu-english-helper :fetcher github :repo "manateelazycat/corfu-english-helper")
  :bind
  ("M-s t" . toggle-corfu-english-helper))

(provide 'init-dictionary)
