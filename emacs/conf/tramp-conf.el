;;
;; Filename: tramp-conf.el
;; Description: Setting for tramp.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:41:44
;;
(message ">>>>> Loading [ tramp ] Customizations ....")

(when win32p
  (require 'tramp "tramp" t)
  (setq tramp-password-end-of-line "\r\n")
  ;;(setq tramp-sh-program "c:/cygwin/bin/sh.exe")
  ;;(setq tramp-default-method "smx")
  (setq tramp-default-method "plink")
  (setq tramp-verbose 10)
  (setq tramp-debug-buffer t)
  (setq tramp-auto-save-directory "c:/temp")
)
