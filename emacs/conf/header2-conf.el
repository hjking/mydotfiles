;;
;; Filename: header2-conf.el
;; Description: Setting for header2.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:22:38
;;
(message ">>>>> Loading [ header2 ] Customizations ....")

(require 'header2)
(add-hook 'emacs-lisp-mode-hook 'auto-make-header)
(add-hook 'c-mode-common-hook   'auto-make-header)
(add-hook 'write-file-hooks 'auto-update-file-header)
