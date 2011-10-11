;;
;; Filename: yasnippet-conf.el
;; Description: Setting for yasnippet.el
;; Author: Hong Jin
;; Created: 2011-8-31 10:00
;; Last Updated: 2011-08-31 15:15:31

(message ">>>>> Loading [ yasnippet ] Customizations ....")
(require 'yasnippet) ;; not yasnippet-bundle
(yas/initialize)
(setq my-yasnippet-dir "~/.emacs.d/plugin/yasnippet/snippets")
(yas/load-directory my-yasnippet-dir)

;; hook for automatic reloading of changed snippets
(defun my-update-yasnippets-on-save ()
  (when (string-match "/yasnippet/snippets" buffer-file-name)
    (yas/load-directory my-yasnippet-dir)))
(add-hook 'after-save-hook 'my-update-yasnippets-on-save)
