;;; planner-psvn.el --- psvn.el integration for the Emacs Planner

;; Copyright (C) 2005, 2006, 2008 Free Software Foundation, Inc.

;; Author: Stefan Reichör <stefan@xsteve.at>
;; Keywords: planner, psvn

;; This file is part of Planner.  It is not part of GNU Emacs.

;; Planner is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; Planner is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Planner; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;_ + Commentary:

;; This file allows you to refer to your svn changesets easily from within
;; a planner page.
;;
;; Example: the changeset
;; [[psvn://http://my.svn-repos.at/svn/project1/trunk@39][project1#39]]
;;   can be shown easily via psvn.
;;
;; Additionally there is functionality to generate a note for your
;; commits via subversion, just set
;; `planner-psvn-log-edit-notice-commit-function' to t.

;;; Contributors:

;; Seth Falcon provided a patch to write the note to planner-today
;; rather than *temp*.muse.

;; Yann Hodique helped port this to Muse.

;;; Code:

(require 'planner)
(require 'psvn)

(defgroup planner-psvn nil
  "Planner options for the psvn integration."
  :prefix "planner-psvn-"
  :group 'planner)

(defcustom planner-psvn-log-edit-include-files-flag
  t
  "Non-nil means include a list of committed files in the note."
  :type 'boolean
  :group 'planner-psvn)

(defcustom planner-psvn-log-edit-notice-commit-function nil
  "Function that should return non-nil if this commit should be noted.
The function will be run in the log buffer."
  :type '(choice
          (const :tag "Always note commits" t)
          function)
  :group 'planner-psvn)


;; compatibility for older psvn.el revisions
(cond ((fboundp 'svn-run)
       (defalias 'planner-svn-run 'svn-run))
      ((fboundp 'svn-run-svn)
       (defalias 'planner-svn-run 'svn-run-svn))
      (t
       (error "No `svn-run' command found")))

(defun planner-psvn-generate-url (repo-path revision &optional link-prefix no-link-postfix)
  (planner-make-link (concat "psvn://" repo-path "@" revision)
                     (if link-prefix
                         (if no-link-postfix
                             link-prefix
                           (concat link-prefix revision))
                       (concat "svn-rev#" revision))))

;;;###autoload
(defun planner-annotation-from-psvn ()
  "If called from a psvn  *svn-log-view* buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (eq major-mode 'svn-log-view-mode)
    (svn-status-parse-info t)
    (planner-psvn-generate-url (svn-status-base-info->url)
                               (svn-log-revision-at-point))))

;;;###autoload
(defun planner-psvn-browse-url (url)
  "If this is a psvn url, handle it."
  (when (string-match "\\`psvn:/?/?\\(.+\\)@\\([0-9]+\\)" url)
    (let ((repo-path (match-string 1 url))
          (svn-rev (string-to-number (match-string 2 url)))
          (cur-buf (current-buffer)))
      (planner-svn-run nil t 'diff "diff"
                       (concat repo-path "@" (number-to-string (- svn-rev 1)))
                       (concat repo-path "@" (number-to-string svn-rev)))
      (svn-status-activate-diff-mode))
    t))

(defun planner-psvn-log-edit-extract-file-name (file-info)
  (svn-status-line-info->filename file-info))

;;;###autoload
(defun planner-psvn-log-edit-add-note ()
  "Add a note describing the commit via psvn to the current planner page."
  (when (if (functionp planner-psvn-log-edit-notice-commit-function)
            (funcall planner-psvn-log-edit-notice-commit-function)
          planner-psvn-log-edit-notice-commit-function)
    (svn-status-parse-info t)
    (save-window-excursion
      (planner-create-note nil)
      (insert "Commit")
      (insert (concat " " (planner-psvn-generate-url
                           (svn-status-base-info->url)
                           (number-to-string svn-status-commit-rev-number)
                           (when svn-status-module-name
                             (concat svn-status-module-name "#")))))
      (newline)
      (when (and planner-psvn-log-edit-include-files-flag
                 svn-status-files-to-commit)
        (insert "Files: ")
        (insert (mapconcat 'planner-psvn-log-edit-extract-file-name
                           svn-status-files-to-commit " "))
        (newline)
        (newline))
      (insert-buffer-substring "*svn-log-edit*"))))

(add-hook 'svn-log-edit-done-hook 'planner-psvn-log-edit-add-note)

(planner-add-protocol "psvn:/?/?" 'planner-psvn-browse-url nil)
(add-hook 'planner-annotation-functions 'planner-annotation-from-psvn)
(custom-add-option 'planner-annotation-functions 'planner-annotation-from-psvn)

(provide 'planner-psvn)

;;; planner-psvn.el ends here
