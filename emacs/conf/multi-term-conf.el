;;
;; Filename: multi-term-conf.el
;; Description: Setting for multi-term.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:40:35

;; multi-term setting
;; available for Emacs 23
;;
(message ">>>>> Loading [ mutil-term ] Customizations ....")
(require 'multi-term)
(setq multi-term-program "/bin/tcsh")
(setq multi-term-switch-after-close nil)
(defun term-mode-settings ()
  "Settings for term-mode"
  (make-local-variable 'scroll-margin)
  (setq-default scroll-margin 0)
)
(add-hook 'term-mode-hook 'term-mode-settings)
(global-set-key "\M-t"              'multi-term)

