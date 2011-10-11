;;
;; Filename: column-marker-conf.el
;; Description: Setting for column-marker.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-22 14:34:13

(message ">>>>> Loading [ column-marker ] Customizations ....")

(require 'column-marker)
(dolist (hook '(emacs-lisp-mode-hook
                cperl-mode-hook
                python-mode-hok
                shell-mode-hook
                text-mode-hook
                change-log-mode-hook
                makefile-mode-hook
                message-mode-hook
                verilog-mode-hook
                vlog-mode
                texinfo-mode-hook))
  (add-hook hook (lambda ()
                   (interactive)
                   (column-marker-1 80)
                   (column-marker-2 90)
                   (column-marker-3 100))))

;; use `C-c m' interactively to highlight with `column-marker-1-face'
;;  (global-set-key (kbd "C-c m") 'column-marker-1)

;; EOF
