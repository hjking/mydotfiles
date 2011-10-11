;;
;; Filename: ffap-conf.el
;; Description: Setting for ffap.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:39:48

(message ">>>>> Loading [ ffap ] Customizations ....")
(require 'ffap)
(ffap-bindings)
;;  (setq ffap-c-path (append ffap-c-path system-head-file-dir user-head-file-dir))
;; function called to fetch an URL. could be `browse-url-emacs or w3m-browse-url
(setq ffap-url-fetcher 'browse-url)
(setq ffap-require-prefix t)
;; recognize Win path
(setq ffap-string-at-point-mode-alist
      '((file "--:\\\\$+<>@-Z_a-z~*?" "<@" "@>;.,!:")
        (url "--:=&?$+@-Z_a-z~#,%;*" "^A-Za-z0-9" ":;.,!?")
        (nocolon "--9$+<>@-Z_a-z~" "<@" "@>;.,!?")
        (machine "-a-zA-Z0-9." "" ".")
        (math-mode ",-:$+<>@-Z_a-z~`" "<" "@>;.,!?`:")))
;; visit a file
;;  (global-set-key (kbd "<f3>") 'find-file-at-point)))
