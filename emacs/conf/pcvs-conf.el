
;; Filename: pcvs-conf.el
;; Description: Setting for pcvs.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:17:56
;;
(message ">>>>> Loading [ pcvs ] Customizations ....")

;; allow commit on whole directories
(setq cvs-allow-dir-commit t)
;; when to reuse an existing cvs buffer
(setq cvs-reuse-cvs-buffer 'always)  ;; subdir
;; examine
(global-set-key (kbd "C-x v e") 'cvs-examine)
;; examine without asking for a directory
(global-set-key (kbd "<C-f9>")
                '(lambda ()
                   (interactive)
                   (cvs-examine (file-name-directory (buffer-file-name))
                                nil)))
;; messages that should be ignored by the parser
;; TODO Should only ADD the last one to the default value of cvs-parse-...
(setq cvs-parse-ignored-messages
      '("Executing ssh-askpass to query the password.*$"
        ".*Remote host denied X11 forwarding.*$"
        ".*-m wrapper option is not supported remotely.*$"))

;; EOF
