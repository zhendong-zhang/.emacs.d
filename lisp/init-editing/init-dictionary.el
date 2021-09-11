(use-package youdao-dictionary
  :bind
  (("M-s d" . youdao-dictionary-search-at-point)
   ("M-s v" . youdao-dictionary-play-voice-from-input))
  :config
  (setq url-automatic-caching t))

(provide 'init-dictionary)
