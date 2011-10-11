;;; planner-tasks-overview.el --- Task summary for planner.el
;;
;; Copyright (C) 2004, 2005, 2007, 2008 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: planner-tasks-overview.el
;; Keywords: hypermedia
;; Author: Sandra Jean Chua (Sacha) <sacha@free.net.ph>
;; Description: Task overview for planner.el files
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

;;;_ + Commentary:

;; You can use `planner-tasks-overview' to see a list of tasks between
;; two dates. `planner-tasks-overview-jump' jumps to the linked task.
;; To change the sort order, invoke the following functions:
;; `planner-tasks-overview-sort-by-date'
;; `planner-tasks-overview-sort-by-plan'
;; `planner-tasks-overview-sort-by-category'
;; `planner-tasks-overview-sort-by-status'
;;
;; This file was inspired by Markus Hoenicka's
;; http://www.mhoenicka.de/software/hacks/tasklist.html and Frederick
;; Fouvry's Lisp port of tasklist.pl.

;;;_ + Contributors

;; Andrew J. Korty (ajk AT iu DOT edu) provided a patch that corrected
;; a typo in the keymap.

;; Yann Hodique helped port this to Muse.

;;; Code:

(require 'planner)

(defvar planner-tasks-overview-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "1" 'planner-tasks-overview-sort-by-date)
    (define-key map "2" 'planner-tasks-overview-sort-by-plan)
    (define-key map "3" 'planner-tasks-overview-sort-by-priority)
    (define-key map "4" 'planner-tasks-overview-sort-by-status)
    (define-key map "j" 'planner-tasks-overview-jump)
    (define-key map "o" 'planner-tasks-overview)

    (define-key map [tab] 'planner-next-reference)
    (define-key map [(control ?i)] 'planner-next-reference)
    (if (featurep 'xemacs)
        (define-key map [(shift tab)] 'planner-previous-reference)
      (define-key map [(shift iso-lefttab)] 'planner-previous-reference)
      (define-key map [(shift control ?i)] 'planner-previous-reference))
    map)
  "Keymap for planner task overview buffers.")

(define-derived-mode planner-tasks-overview-mode planner-mode "Overview"
  "Planner tasks overview.
\\{planner-tasks-overview-mode-map}")

(defvar planner-tasks-overview-data nil "Task data.")
(make-variable-buffer-local 'planner-tasks-overview-data)
(defvar planner-tasks-overview-buffer "*planner tasks overview*"
  "Buffer name.")

;;;###autoload
(defun planner-tasks-overview (start end)
  "Display a task overview from START to END."
  (interactive (list (planner-read-date)
                     (planner-read-date)))
  (when (get-buffer planner-tasks-overview-buffer)
    (kill-buffer planner-tasks-overview-buffer))
  (with-current-buffer (get-buffer-create planner-tasks-overview-buffer)
    (planner-tasks-overview-mode)
    (setq planner-tasks-overview-data
          (planner-extract-tasks
           (planner-get-day-pages start end)))
    (setq truncate-lines t)
    (set (make-local-variable 'truncate-partial-width-windows) t)
    (planner-tasks-overview-sort-by-date)
    (switch-to-buffer (current-buffer))))

;;;###autoload
(defun planner-tasks-overview-jump-other-window ()
  "Display the task under point in a different window."
  (interactive)
  (planner-tasks-overview-jump t))

;;;###autoload
(defun planner-tasks-overview-jump (&optional other-window)
  "Display the task under point."
  (interactive "P")
  (when other-window
    (switch-to-buffer-other-window (current-buffer)))
  (let* ((info (get-text-property (point) 'info))
         (page (or (elt info 2) (elt info 3))))
    (planner-find-file page)
    (goto-char (point-min))
    (widen)
    (when (re-search-forward
           (concat
            "#[A-C][0-9]*\\s-+.\\s-+"
            (regexp-quote (elt info 4))) nil t)
      (goto-char (planner-line-beginning-position)))))

(defun planner-tasks-overview-sort-by-field (field)
  "Sort by FIELD."
  (interactive)
  (setq planner-tasks-overview-data
        (sort planner-tasks-overview-data
              (lambda (a b) 
                (cond 
                 ((null (elt a field)) nil)
                 ((null (elt b field)) t)
                 ((string< (elt a field) (elt b field)))))))
  (planner-tasks-overview-insert))

(defun planner-tasks-overview-sort-by-date ()
  "Sort by date."
  (interactive)
  (planner-tasks-overview-sort-by-field 8))

(defun planner-tasks-overview-sort-by-plan ()
  "Sort by plan."
  (interactive)
  (planner-tasks-overview-sort-by-field 7))

(defun planner-tasks-overview-sort-by-priority ()
  "Sort by plan."
  (interactive)
  (planner-tasks-overview-sort-by-field 1))

(defun planner-tasks-overview-sort-by-status ()
  "Sort by status."
  (interactive)
  (setq planner-tasks-overview-data
        (sort planner-tasks-overview-data
              (lambda (a b)
                (if (string= (elt b 3)
                             (elt a 3))
                    (string< (elt a 1)
                             (elt b 1))
                  (member (elt b 3)
                          (member (elt a 3)
                                  '("_" "o" "D" "P" "X" "C")))))))
  (planner-tasks-overview-insert))

(defun planner-tasks-overview-insert ()
  "Insert the textual representation for `planner-tasks-overview-data'."
  (with-current-buffer (get-buffer-create "*planner tasks overview*")
    (setq muse-current-project (muse-project planner-project))
    (setq buffer-read-only nil)
    (erase-buffer)
    (let (last-date last-plan)
      (mapcar
       (lambda (item)
         (let* ((date (planner-task-date item))
                (plan (planner-task-plan item))
                (text
                 (format "%10s | %s | %s %s | %s\n"
                         (if date
                             (planner-make-link 
                              date
                              (format "%-10.10s"
                                      (if (string= last-date date)
                                          "\"\"" 
                                        date)))
                           (format "%-10.10s" ""))
                         (if plan
                             (planner-make-link 
                              plan
                              (format "%-20.20s"
                                      (if (string= last-plan plan)
                                          "\"\""
                                        plan)))
                         (format "%-20.20s" ""))
                         (planner-task-priority item)
                         (planner-task-status item)
                         (planner-task-description item))))
           (add-text-properties 0 (length text) (list 'info item)
                                text)
           (insert text)
           (setq last-date date)
           (setq last-plan plan)))
       planner-tasks-overview-data)
      (goto-char (point-min))
      (setq buffer-read-only t))))

;; Improvements: sort?
;;;###autoload
(defun planner-tasks-overview-show-summary (&optional file-list)
  "Count unscheduled, scheduled, and completed tasks for FILE-LIST.
If called with an interactive prefix, prompt for the page(s) to
display. planner-multi is required for multiple pages."
  (interactive
   (list
    (if current-prefix-arg
        (planner-file-alist
         nil
         (if (featurep 'planner-multi)
             (mapcar 'planner-link-base
                     (planner-multi-split
                      (planner-read-non-date-page
                       (planner-file-alist))))
           (list (planner-read-non-date-page
                  (planner-file-alist))))))))
  (let (data total)
    (with-planner
      (setq data (planner-tasks-overview-get-summary file-list))
      (with-current-buffer (get-buffer-create "*planner tasks overview*")
        (setq buffer-read-only nil)
        (erase-buffer)
        (setq total (+ (elt data 0) (elt data 1) (elt data 2)))
        (insert (format "Total unfinished, unscheduled tasks : %3d (%6.2f%%)\n"
                        (elt data 0) (/ (elt data 0) (* 0.01 total)))
                (format "Total unfinished, scheduled tasks   : %3d (%6.2f%%)\n"
                        (elt data 1) (/ (elt data 1) (* 0.01 total)))
                (format "Total finished tasks                : %3d (%6.2f%%)\n\n"
                        (elt data 2) (/ (elt data 2) (* 0.01 total))))
        (insert (format "%-40s | Unsched  |  Sched   | Complete | Total\n"
                        "Plan page"))
        (mapcar
         (lambda (row)
           (let ((row-total (* 0.01 (+ (elt row 1) (elt row 2) (elt row 3)))))
             (insert
              (format "%s | %3d %3.0f%% | %3d %3.0f%% | %3d %3.0f%% | %3d %3.0f%%\n"
                      (planner-make-link
                       (elt row 0)
                       (format "%-40.40s" (elt row 0)))
                      (elt row 1)
                      (/ (elt row 1) row-total)
                      (elt row 2)
                      (/ (elt row 2) row-total)
                      (elt row 3)
                      (/ (elt row 3) row-total)
                      (+ (elt row 1) (elt row 2) (elt row 3))
                      (/ (+ (elt row 1) (elt row 2) (elt row 3))
                         (* 0.01 total))))))
         (elt data 3))
        (setq muse-current-project (muse-project planner-project))
        (planner-mode)
        (switch-to-buffer (current-buffer))))))

;; Unfinished                Finished      % Complete
;; Unscheduled   Scheduled
(defun planner-tasks-overview-get-summary (&optional file-list)
  "Return a summary of tasks on pages in FILE-LIST.
List is of the form (total-unfinished-unscheduled
total-unfinished-scheduled total-finished data), where data is a
list of the form (plan unfinished-unscheduled
unfinished-scheduled finished)."
  (let ((total-unfinished-unscheduled 0)
        (total-unfinished-scheduled 0)
        (total-finished 0)
        list)
    (unless file-list (setq file-list (planner-file-alist)))
    (with-temp-buffer
      (with-planner
        (while file-list
          (unless (string-match planner-date-regexp (car (car file-list)))
            (let ((unfinished-unscheduled 0)
                  (unfinished-scheduled 0)
                  (finished 0))
              (erase-buffer)
              (insert-file-contents (cdr (car file-list)))
              (goto-char (point-min))
              (while (re-search-forward planner-task-regexp nil t)
                (let ((info (planner-task-info-from-string
                             (car (car file-list))
                             (buffer-substring
                              (planner-line-beginning-position)
                              (planner-line-end-position)))))
                  (cond
                   ((or (string= (planner-task-status info) "X")
                        (string= (planner-task-status info) "C"))
                    (setq finished (1+ finished)))
                   ((planner-task-date info)
                    (setq unfinished-scheduled (1+ unfinished-scheduled)))
                   (t (setq unfinished-unscheduled
                            (1+ unfinished-unscheduled))))))
              (setq list (cons (list (car (car file-list))
                                     unfinished-unscheduled
                                     unfinished-scheduled
                                      finished)
                                list))
               (setq total-unfinished-unscheduled
                     (+ total-unfinished-unscheduled unfinished-unscheduled))
               (setq total-unfinished-scheduled
                     (+ total-unfinished-scheduled unfinished-scheduled))
               (setq total-finished
                     (+ total-finished finished))))
          (setq file-list (cdr file-list)))))
    (list total-unfinished-unscheduled
          total-unfinished-scheduled
          total-finished
          list)))

(provide 'planner-tasks-overview)

;;; planner-tasks-overview.el ends here
