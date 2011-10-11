;;; planner-experimental.el --- Experimental functions for Emacs planner mode

;; Copyright (C) 2004, 2008 Free Software Foundation, Inc.

;; Author: Sacha Chua

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

(require 'planner)

;;; Code:

;;;_ + Searching

(defun planner-search-notes-next-match ()
  "Jump to the next matching entry.  Call after `planner-search-notes.'"
  (interactive)
  (if (buffer-live-p (get-buffer planner-search-notes-buffer))
      (progn
        (set-buffer planner-search-notes-buffer)
        (forward-line -1)
        (goto-char (planner-line-beginning-position))
        (planner-follow-name-at-point))
    (error "No current search.")))

(defun planner-search-notes-previous-match ()
  "Jump to the previous matching entry.  Call after `planner-search-notes.'"
  (interactive)
  (if (buffer-live-p (get-buffer planner-search-notes-buffer))
      (progn
        (set-buffer planner-search-notes-buffer)
        (forward-line 1)
        (goto-char (planner-line-beginning-position))
        (planner-follow-name-at-point))
    (error "No current search.")))

;;;_* Tasks

(defun planner-remove-duplicates ()
  "Remove duplicate tasks."
  (interactive)
  (goto-char (point-min))
  (let ((today (planner-today))
        (on-date (string-match planner-date-regexp (planner-page-name))))
    (while (re-search-forward "^#[A-C][0-9]*\\s-+\\(.+\\)$" nil t)
      (save-excursion
        (let* ((task-info (planner-current-task-info))
               (keep (planner-task-date task-info))
               date
               found
               (start (match-beginning 0)))
          (goto-char (planner-line-beginning-position))
          (save-excursion
            (unless on-date
              (while (planner-find-task task-info (point))
                ;; Determine the most recent one
                (setq date (planner-task-date (planner-current-task-info)))
                (when (or (and (string< keep today)
                               (string< keep date))
                          (string< date keep))
                (setq keep date))
                (forward-line 1))))
          (while (planner-find-task task-info (point))
            (if (string= keep
                         (planner-task-date (planner-current-task-info)))
                (if found
                    (delete-region (planner-line-beginning-position)
                                   (min (1+ (planner-line-end-position))
                                        (point-max)))
                  (setq found t)
                  (forward-line 1))
              (planner-delete-task))))))))

;;;_* Local emacs vars.
;;;Local variables:
;;;allout-layout: (-1 0 : )
;;;End:

(provide 'planner-experimental)

;;; planner-experimental.el ends here
