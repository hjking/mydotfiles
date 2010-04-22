
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

