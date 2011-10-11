(add-to-list 'load-path ".")

;; If you want Planner to find all of these programs when
;; byte-compiling Planner, make sure to edit these paths to match your
;; configuration.

(add-to-list 'load-path "~/elisp/muse/lisp")
(add-to-list 'load-path "~/elisp/remember")
(add-to-list 'load-path "~/elisp/bbdb/lisp")
(add-to-list 'load-path "~/elisp/erc")
(add-to-list 'load-path "~/elisp/gnats")
(add-to-list 'load-path "~/elisp/gnus/lisp")
(add-to-list 'load-path "~/elisp/psvn")
(add-to-list 'load-path "~/elisp/vm")
(add-to-list 'load-path "~/elisp/w3m")
(add-to-list 'load-path "~/elisp/wl")
(add-to-list 'load-path "~/elisp/xtla")

(defun planner-elint-files ()
  (require 'elint)
  (elint-initialize)

  (defvar nomessage t)
  (load "planner" nil nomessage)
  (load "muse-mode" nil nomessage)
  (dolist (file (directory-files "." nil "\\.el$"))
    (setq file (substring file 0 (string-match "\\.el$" file)))
    (load file nil nomessage))

  (add-to-list 'elint-standard-variables 'current-prefix-arg)
  (add-to-list 'elint-standard-variables 'command-line-args-left)
  (add-to-list 'elint-standard-variables 'buffer-file-coding-system)
  (add-to-list 'elint-standard-variables 'save-some-buffers-action-alist)
  (add-to-list 'elint-standard-variables 'emacs-major-version)
  (add-to-list 'elint-standard-variables 'emacs-minor-version)
  (add-to-list 'elint-standard-variables 'emacs-version)
  (add-to-list 'elint-standard-variables 'window-system)
  (add-to-list 'elint-standard-variables 'debug-on-error)
  (add-to-list 'elint-standard-variables 'planner-mode-abbrev-table)
  (dolist (file command-line-args-left)
    (find-file file)
    (message "Checking %s ..." file)
    (elint-current-buffer)
    (with-current-buffer (elint-get-log-buffer)
      (goto-char (point-min))
      (forward-line 2)
      (while (not (or (eobp)
                      (looking-at "^Linting complete")))
        (message (buffer-substring (planner-line-beginning-position)
                                   (planner-line-end-position)))
        (forward-line 1)))
    (kill-buffer (current-buffer))))

(defun planner-generate-autoloads ()
  (interactive)
  (defvar autoload-package-name)
  (defvar command-line-args-left)
  (defvar generated-autoload-file)
  (require 'autoload)
  (setq generated-autoload-file (expand-file-name "planner-autoloads.el"))
  (setq command-line-args-left (mapcar #'expand-file-name
                                       command-line-args-left))
  (if (featurep 'xemacs)
      (progn
          (setq autoload-package-name "planner")
          (batch-update-autoloads))
    (find-file generated-autoload-file)
    (delete-region (point-min) (point-max))
    (insert ";;; planner-autoloads.el --- autoloads for Planner
;;
;;; Code:
")
    (save-buffer 0)
    (batch-update-autoloads)
    (find-file generated-autoload-file)
    (goto-char (point-max))
    (insert ?\n)
    (insert "(provide 'planner-autoloads)
;;; planner-autoloads.el ends here
;;
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
")
    (insert ?\n)
    (save-buffer 0)
    (kill-buffer (current-buffer))))
