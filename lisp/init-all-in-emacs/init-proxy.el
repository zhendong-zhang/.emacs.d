(defvar my-proxy-ip "127.0.0.1")
(defvar my-http-proxy-port "1080")
(defvar my-http-proxy (format "%s:%s" my-proxy-ip my-http-proxy-port))
(defvar my-socks-proxy-port "1081")
(defvar my-socks-proxy (format "%s:%s" my-proxy-ip my-socks-proxy-port))

;; Network Proxy
(defun proxy-http-show ()
  "Show HTTP/HTTPS proxy."
  (interactive)
  (if url-proxy-services
      (message "Current HTTP proxy is `%s'" my-http-proxy)
    (message "No HTTP proxy")))

(defun proxy-http-enable ()
  "Enable HTTP/HTTPS proxy."
  (interactive)
  (proxy-socks-disable)
  (setq url-proxy-services
        `(("http" . ,my-http-proxy)
          ("https" . ,my-http-proxy)
          ("ftp" . ,my-http-proxy)
          ("no_proxy" . "^\\(localhost\\|127.0.0.1\\|::1\\|192.168.*\\|10.*\\)")))
  (proxy-http-show))

(defun proxy-http-disable ()
  "Disable HTTP/HTTPS proxy."
  (interactive)
  (setq url-proxy-services nil)
  (proxy-http-show))

(defun proxy-http-toggle ()
  "Toggle HTTP/HTTPS proxy."
  (interactive)
  (if (bound-and-true-p url-proxy-services)
      (proxy-http-disable)
    (proxy-http-enable)))

(defun proxy-socks-show ()
  "Show SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (message "Current SOCKS%d proxy is %s:%s"
               (cadddr socks-server) (cadr socks-server) (caddr socks-server))
    (message "No SOCKS proxy")))

(defun proxy-socks-enable ()
  "Enable SOCKS proxy."
  (interactive)
  (proxy-http-disable)
  (require 'socks)
  (setq url-gateway-method 'socks
        socks-noproxy '("localhost"))
  (setq socks-server `("Default server" ,my-proxy-ip ,(string-to-number my-socks-proxy-port) 5))
  (setenv "all_proxy" (concat "socks5://" my-socks-proxy))
  (proxy-socks-show))

(defun proxy-socks-disable ()
  "Disable SOCKS proxy."
  (interactive)
  (setq url-gateway-method 'native
        socks-noproxy nil
        socks-server nil)
  (setenv "all_proxy" "")
  (proxy-socks-show))

(defun proxy-socks-toggle ()
  "Toggle SOCKS proxy."
  (interactive)
  (if (bound-and-true-p socks-noproxy)
      (proxy-socks-disable)
    (proxy-socks-enable)))

(defvar proxy-found)

(defun proxy-on-process-result (process status)
  (delete-process process)
  (setq proxy-found (string-match-p "open" status))
  (if proxy-found
      (proxy-socks-enable)
    (proxy-socks-disable)))

(defun proxy-try-enable-proxy ()
  (interactive)
  (make-network-process :name "proxy" :host my-proxy-ip :service my-socks-proxy-port
                        :nowait t :sentinel #'proxy-on-process-result))
(proxy-try-enable-proxy)

;; for debug
;; (display-buffer
;;  (url-retrieve-synchronously
;;   "https://www.github.com"))
;; (display-buffer
;;  (url-retrieve-synchronously
;;   "http://www.github.com"))


(provide 'init-proxy)
