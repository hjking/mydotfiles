
;; Filename: psvn-conf.el
;; Description: Setting for psvn.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:18:35
;;
(message ">>>>> Loading [ psvn ] Customizations ....")

;; `svn-status-property-edit-svn-ignore' (`P TAB') allows user to edit
;; list of files ignored by Subversion
;; hide unmodified files
(setq svn-status-hide-unmodified t)
;; use longer phrases
(setq svn-status-short-mod-flag-p nil)
;; delete temporary files
(setq svn-status-ediff-delete-temporary-files t)
;; show the diff we are about to commit
(define-key svn-log-edit-mode-map (kbd "<f6>") 'svn-log-edit-svn-diff)
;; examine
(global-set-key (kbd "C-x v e") 'svn-status)
;; examine without asking for a directory
(global-set-key (kbd "<C-f9>")
                '(lambda ()
                   (interactive)
                   (svn-status (file-name-directory (buffer-file-name))
                               nil))
)

(defun my-svn-log-edit-mode-setup ()
  (setq ispell-local-dictionary "en_US")
;;    (flyspell-mode)
)

(defun my-svn-load-edit-mode-startup ()
  (interactive)
  (filladapt-mode t)
  (show-paren-mode t)
;;    (flyspell-mode t)
)

(add-hook 'svn-log-edit-mode-hook 'my-svn-log-edit-mode-setup)
(add-hook 'svn-log-edit-mode-hook 'my-svn-load-edit-mode-startup)

;; EOF
