
;; Filename: template-conf.el
;; Description: Setting for template.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:16:40
;;

(message ">>>>> Loading [ template ] Customizations ....")
(require 'template nil t)
(when (featurep 'template)
  (template-initialize)
  (setq template-default-directories  "~/.emacs.d/templates/")
  (define-auto-insert 'elisp-mode     "elisp.tpl")
  (define-auto-insert 'verilog-mode   "verilog.tpl")
  (define-auto-insert 'python-mode    "python.tpl")
  (define-auto-insert 'perl-mode      "perl.tpl")
  (define-auto-insert 'sh-mode        '(nil "#!/bin/bash\n\n"))

  (add-hook 'find-file-hooks 'template-expand-template)
)
