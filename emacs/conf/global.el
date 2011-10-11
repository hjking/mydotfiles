
;; don't truncate the message log buffer when it becomes large
(setq message-log-max t)

;; use decimal for `C-q'
(setq read-quoted-char-radix 10)

;;; === startup ===
;; no GNU emacs startup logo
(setq inhibit-startup-message t)
(setq inhibit-startup-screen t)

;; System locale for time
(setq system-time-locale "C")

;; make Gnus fast
(setq gc-cons-threshold 3500000)
;; don't display messages at start and end of garbage collection (as it hides
;; too many interesting messages)
(setq garbage-collection-messages nil)

;; display what I'm typing *immediately*
(setq echo-keystrokes 0.1)

;; visually indicate buffer boundaries and scrolling
(setq indicate-buffer-boundaries t)

;; visually indicate empty lines after the buffer end
(setq-default indicate-empty-lines t)

;; Shut off warning messages when using system shell
(setq w32-allow-system-shell t)

;; no visible bell ring when TAB
(setq visible-bell nil)

;; no ring and screen flash
(setq ring-bell-function 'ignore)

;; yes/no ==> y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; highlight trailing whitespaces in all modes
(setq-default show-trailing-whitespace t)

;; after 1 second typed M-x CMD, display CMD binding key
(setq suggest-key-bindings 1)

;; confirm before quit emacs
(setq confirm-kill-emacs 'yes-or-no-p)

;; Indent before TAB
(setq tab-always-indent 'complete)

