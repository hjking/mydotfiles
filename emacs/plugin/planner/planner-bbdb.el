;;; planner-bbdb.el --- BBDB integration for the Emacs Planner

;; Copyright (C) 2001, 2003, 2004, 2005, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2004, 2008 Andrew J. Korty

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: planner, gnus
;; URL: http://www.wjsullivan.net/PlannerMode.html

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

;; This file allows you to refer to your contacts easily from within
;; a planner page.
;;
;; Example:
;; [[bbdb://Sacha.*Chua][Sacha]] will be linked to the blog, web
;; or net fields of the first matching BBDB record.

;;;_ + Contributors

;; Andrew J. Korty (ajk AT iu DOT edu) had the idea to add the mailto:
;; URI and provided the initial patch for it.
;;
;; Yann Hodique helped to port this to Muse.

;;; Code:

(require 'planner)
(require 'bbdb-com)

(defun planner-bbdb-get-name-from-address (address)
  "Return the name for ADDRESS to be used in links."
  (let* ((addr (mail-extract-address-components address))
         (rec (apply 'bbdb-search-simple addr)))
    (if rec
        (bbdb-record-name rec)
      (or (car addr) (cadr addr)))))

(defvar planner-bbdb-plan-field "plan"
  "Field that contains a planner page associated with this record.")

;;;###autoload
(defun planner-bbdb-annotation-from-bbdb ()
  "If called from a bbdb buffer, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (eq major-mode 'bbdb-mode)
      (or (bbdb-record-getprop
           (bbdb-current-record) 'plan)
          ;; From a BBDB entry with a plan page; use that. Yay!
          (planner-make-link
           (concat "bbdb://"
                   (planner-replace-regexp-in-string
                    " " "." (bbdb-record-name (bbdb-current-record))))
           (bbdb-record-name (bbdb-current-record))))))


;;;###autoload
(defun planner-bbdb-browse-url (url)
  "If this is a BBDB URL, jump to it."
  (when (string-match "^bbdb:/?/?\\(.+\\)" url)
    (bbdb (match-string 1 url) nil)
    t))

;;;###autoload
(defun planner-bbdb-resolve-url (id)
  "Replace ID with the blog, web or e-mail address of the BBDB record."
  (save-match-data
    (when (string-match "\\`bbdb:/+" id)
      (setq id (replace-match "" t t id)))
    (let ((record (car (bbdb-search (bbdb-records) id id id))))
      (or (and record
               (or (bbdb-record-getprop record 'blog)
                   (bbdb-record-getprop record 'web)
                   (when (car (bbdb-record-net record))
                     (concat "mailto:" (car (bbdb-record-net record))))))
          nil))))

(planner-add-protocol "bbdb:/+" 'planner-bbdb-browse-url 'planner-bbdb-resolve-url)
(add-hook 'planner-annotation-functions 'planner-bbdb-annotation-from-bbdb)
(custom-add-option 'planner-annotation-functions 'planner-bbdb-annotation-from-bbdb)

(defalias 'planner-get-name-from-address 'planner-bbdb-get-name-from-address)

(provide 'planner-bbdb)

;;; planner-bbdb.el ends here
