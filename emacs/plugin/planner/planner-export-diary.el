;;; planner-export-diary.el --- Diary export for the Emacs Planner

;; Copyright (C) 2001, 2004, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2004, 2008 Xin Wei Hu (huxw AT knight DOT 6test DOT edu DOT cn)

;; Emacs Lisp Archive Entry
;; Filename: planner-export-diary.el
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;; Description: Use Emacs for life planning
;; URL: http://www.wjsullivan.net/PlannerMode.html
;; Compatibility: Emacs20, Emacs21, Emacs22, XEmacs21

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

;;; Commentary:

;; Use this package if you want to export diary entries from your
;; planner to diary files. Customize `planner-export-diary-file'
;; (default: ~/diary.planner). Add the following line to your
;; main `diary-file' (default: ~/diary):
;;
;;     #include ~/diary.planner
;;
;; and then call M-x planner-export-diary-future whenever you want
;; future diary entries published. You can automatically publish
;; entries by adding either of the following to your .emacs:
;;
;; a) Publish future entries on startup
;;
;;    (planner-export-diary-future)
;;
;; b) Publish future entries whenever you save a planner file
;;
;;    (add-hook 'planner-mode-hook 'planner-export-diary-setup)

;;;_ + Contributors

;; Xin Wei Hu (huxw AT knight DOT 6test DOT edu DOT cn) did some work
;; on `planner-export-diary-future' and
;; `planner-export-diary-get-schedule-entries'.

;; Yann Hodique helped port this to Muse.

;;; Code:

(require 'planner)

(defvar planner-export-diary-file "~/diary.planner"
  "*Name of the file into which schedules are exported from planner.
This file will be automatically overwritten every time planner
entries are exported, so be careful.")

(defvar planner-export-diary-number-of-days 3
  "*Number of days to export diary appointments for.")

;;;###autoload
(defun planner-export-diary-future ()
  "Exports only `planner-export-number-of-days' days of entries.
This function can be put into your `after-save-hook'."
  (interactive)
  (planner-export-diary (planner-today)
                        (planner-calculate-date-from-day-offset
                         (planner-today)
                         (1- planner-export-diary-number-of-days))))

;;;###autoload
(defun planner-export-diary-setup ()
  "Add `planner-export-diary-future' to `after-save-hook' in planner buffers.
You can add this function to `planner-mode-hook'."
  (add-hook 'after-save-hook
            'planner-export-diary-future
            nil t))

(defun planner-export-diary-get-schedule-entries (files)
  "Return a list containing the planner schedule entries in FILES.
Entries in the returned list are of the form [DATE START END
DATA].  FILES is an alist of planner pages and their filenames."
  (with-temp-buffer
    (cd (planner-directory))
    (let ((list '())
          start end data)
      (while (car files)
        (insert-file-contents (cdar files))
        (goto-char (point-min))
        (while (re-search-forward
                "^\\s-*\\([0-9]+:[0-9]+\\)\\s-*|\\s-*\\(.+\\)" nil t)
          (setq start (match-string 1))
          (setq data (match-string 2))
          (setq end nil)
          (when (string-match "\\([0-9]+:[0-9]+\\)\\s-*|\\s-*" data)
            (setq end (match-string 1 data))
            (setq data (replace-match "" nil t data)))
          (setq list (cons
                      (vector (caar files)
                              start end data)
                      list)))
        (setq files (cdr files))
        (erase-buffer))
      list)))

(defun planner-export-diary-format-schedule-entries-for-diary (list)
  "Format LIST as diary entries.
LIST should contain entries of the form [DATE START END
DATA]."
  (mapconcat (lambda (item)
               (concat
                (let ((date (planner-filename-to-calendar-date (elt item 0))))
                  (format "%02d/%02d/%04d"
                          (elt date 0)
                          (elt date 1)
                          (elt date 2)))
                " "
                (elt item 1)
                " "
                (elt item 3)))
             list "\n"))

;;;###autoload
(defun planner-export-diary (&optional from to)
  "Exports all the schedules or the ones from FROM to TO (inclusive)."
  (interactive)
  (with-temp-file planner-export-diary-file
    (insert (planner-export-diary-format-schedule-entries-for-diary
             (planner-export-diary-get-schedule-entries
              (planner-get-day-pages from to))))))

(provide 'planner-export-diary)

;;; planner-export-diary.el ends here
