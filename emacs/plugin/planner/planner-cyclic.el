;;; planner-cyclic.el --- Cyclic task support for the Emacs Planner

;; Copyright (C) 2004, 2005, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2005, 2008 Sergey Vlasov (vsu AT altlinux.ru)

;; Filename: planner-cyclic.el
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Provide cyclic task support
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
;;
;; Place planner-cyclic.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-cyclic)
;;
;; Create a diary file named ~/.diary.cyclic-tasks
;; (or the value of planner-cyclic-diary-file). Example:
;;
;;    Tuesday #B0 _ Study Japanese
;;    Friday #B0 _ Study Japanese (JapaneseStudies)
;;
;; The first will be a plain task, the second will be linked.
;;
;; By default, planner-cyclic creates multiple tasks if you let tasks build up
;; (that is, the next Tuesday rolls around and you _still_ haven't
;; marked the task as done.) To turn off this behavior:
;;
;; (setq planner-cyclic-diary-nag nil)

;;; Code:

(require 'planner)
(require 'diary-lib)

(defcustom planner-cyclic-diary-file "~/.diary.cyclic-tasks"
  "Diary file containing cyclic tasks."
  :type 'string
  :group 'planner)

(defcustom planner-cyclic-diary-nag t
  "If non-nil, create tasks even if there are procrastinated cyclic tasks."
  :type 'boolean
  :group 'planner)

(defcustom planner-cyclic-task-description-format "%s from %s"
  "Format used by `planner-cyclic-generate-task' when creating a task.
This string must be a valid control string for `format'.  First format
argument is the task description read from `planner-cyclic-diary-file',
second argument is the date string.
 
If this format is changed when you already have some cyclic tasks
created with the old format, `planner-cyclic-create-tasks-maybe' will
add the same tasks with the new format, unless you convert existing
tasks to the new format manually."
  :type 'string
  :group 'planner)

;;; functions

(defun planner-cyclic-get-cyclic-tasks (date &optional no-of-days)
  "For DATE, get the cyclic tasks."
  (let ((date (if (stringp date)
		  (planner-filename-to-calendar-date date)
		date)))
    (delq nil
	  (mapcar (lambda (item)
		    (when (string-match "#[A-C].+" (elt item 1))
		      (match-string 0 (elt item 1))))
		  (planner-list-diary-entries planner-cyclic-diary-file
                                              date 1)))))

(defun planner-cyclic-generate-task (date task-string)
  "For DATE, generate a cyclic task based on TASK-STRING."
  (let ((info (planner-task-info-from-string date task-string)))
    (if info
        (setcar (nthcdr 4 info)
		(format planner-cyclic-task-description-format
			(planner-task-description info)
                        date))
      (message "Cannot parse task %s" task-string))
    info))

(defun planner-cyclic-create-task-maybe (date task-string)
  "For DATE, possibly create a task based on TASK-STRING."
  (when (string-match planner-task-regexp task-string)
    (let ((orig-task (planner-task-info-from-string date task-string))
          (new-task (planner-cyclic-generate-task date task-string)))
      (unless (planner-find-task new-task)
        (when (or planner-cyclic-diary-nag (not (planner-find-task orig-task)))
          (planner-create-task-from-info new-task nil nil nil nil nil date))))))

;;;###autoload
(defun planner-cyclic-create-tasks-maybe ()
  "Maybe create cyclic tasks.
This will only create tasks for future dates or today."
  (interactive)
  (when (and (planner-derived-mode-p 'planner-mode)
             (planner-page-name)
             (not muse-publishing-p)
             (string-match planner-date-regexp (planner-page-name))
             (or (string< (planner-today) (planner-page-name))
                 (string= (planner-today) (planner-page-name))))
    (mapcar
     (lambda (task-string)
       (when task-string
         (planner-cyclic-create-task-maybe (planner-page-name)
                                           task-string)))
     (planner-cyclic-get-cyclic-tasks (planner-page-name)))))

(add-hook 'planner-mode-hook 'planner-cyclic-create-tasks-maybe)

(provide 'planner-cyclic)

;;; planner-cyclic.el ends here
