;; 需设置环境变量OPENAI_API_KEY
(use-package mind-wave
  :github "manateelazycat/mind-wave"
  :mode ("\\.chat$" . mind-wave-chat-mode)
  :config
  (setq mind-wave-auto-change-title nil))

(provide 'init-chatgpt)
