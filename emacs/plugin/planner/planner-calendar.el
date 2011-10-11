;;; planner-calendar.el --- Create a clickable calendar in published html

;; Copyright (C) 2003, 2004, 2008 Gary V. Vaughan (gary AT gnu DOT org)
;; Parts copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: planner-calendar.el
;; Version: 1.1
;; Date: Tue, 1 June 2004
;; Keywords: hypermedia
;; Author: Gary V. Vaughan (gary AT gnu DOT org)
;; Description: Create a clickable calendar in published html
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

;; You will need to install Emacs Muse before this is of any use to
;; you.

;; To publish calendars in your day pages, it is necessary to do two
;; steps.
;;
;; 1. Add (require 'planner-calendar) to your configuration.
;;
;; 2. Add a <calendar> tag to either your header, footer, or
;;    `planner-day-page-template', depending on where you want it to
;;    appear.

;; If you decide to create a today link for published planner pages,
;; add a hook function like this:
;;
;;   (eval-after-load "muse-publish"
;;     '(add-hook 'muse-after-publish-hook
;;                'planner-calendar-create-today-link))

;;; Contributors:

;; drkm <darkman_spam@yahoo.fr> contributed a small patch that fixes a
;; planner-calendar boundary case when last day of the month is
;; Sunday.

;;; Code:

(require 'calendar)
(require 'muse)
(require 'planner)

(eval-when-compile
  (require 'planner-publish))

(defgroup planner-calendar nil
  "Options controlling the behaviour of planner calendar publication."
  :group 'planner)

(defcustom planner-calendar-prev-month-button "&laquo;"
  "*Default html entity to use for previous month buttons."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-next-month-button "&raquo;"
  "*Default html entity to use for next month buttons."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-day-header-chars 3
  "*Default number of characters to use for day column header names."
  :type 'integer
  :group 'planner-calendar)

(defcustom planner-calendar-html-tag-marker "<div id=\"content\">"
  "*Default html block element to add calendar HTML to."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-today-page-name "today"
  "*Default base name for published today page link file."
  :type 'string
  :group 'planner-calendar)

(defcustom planner-calendar-nop-buttons-flag t
  "Non-nil means add <nop> tags before navigation buttons in the calendar."
  :type 'boolean
  :group 'planner-calendar)

(defmacro planner-calendar-render (var begin end tag class &rest body)
  "Generate a row of days for the calendar."
  `(let (string)
     (calendar-for-loop ,var from ,begin to ,end do
      (let ((day (mod (+ calendar-week-start-day i) 7))
	    (wrap-p (and (= 6 (mod ,var 7)) (/= ,var ,end))))
	(setq string (concat string
			     "<" ,tag " class=\"" ,class " "
			     (calendar-day-name day nil t) "\">"
			     ,@body
			     "</" ,tag ">\n"
			     (and wrap-p "</tr>\n<tr>\n")))))
     string))

(put 'planner-calendar-render 'lisp-indent-function 1)

(defun planner-calendar-date-to-filename (date)
  "See `planner-date-to-filename' except don't choke on nil DATE."
  (and date (planner-date-to-filename date)))

;; calendar-week-start-day
(defun planner-calendar (month year &optional arrows)
  "Generate a string of html to render a clickable calendar for MONTH YEAR.
If ARROWS is non-nil, include prev/next month arrows."
  (let*
      ((blank-days			; at start of month
	(mod (- (calendar-day-of-week (list month 1 year))
		calendar-week-start-day)
	 7))
       (last (calendar-last-day-of-month month year))
       (pad-days			; at end of month
	(- 7 (1+ (calendar-day-of-week (list month last year)))))
       ;; don't use leading whitespace in the generated html, or the
       ;; other markup rules will add <blockquote> sections!
       (string
	(concat
	 "<table class=\"month-calendar\">\n"
	 "<tr class=\"month-calendar-head\">\n"
	 (if arrows
	     (concat
	      "<th>"
	      (planner-calendar-prev-month-href
	       month year
	       planner-calendar-prev-month-button
	       planner-calendar-nop-buttons-flag)
	      "</th>\n"
	      "<th colspan=\"5\">\n")
	   "<th colspan=\"7\">\n")
	 (format "%s %d" (calendar-month-name month) year)
	 "</th>\n"
	 (when arrows
	   (concat "<th>"
		   (planner-calendar-next-month-href
		    month year planner-calendar-next-month-button
		    planner-calendar-nop-buttons-flag)
		   "</th>\n"))
	 "</tr>\n"
	 "<tr>\n"

	 ;; add day name headings
	 (planner-calendar-render i 0 6
	  "th" "month-calendar-day-head"
	  (calendar-day-name day planner-calendar-day-header-chars t))

	 "</tr>\n"
	 "<tr>\n"

	 ;; add blank days before the first of the month
	 (planner-calendar-render i 0 (1- blank-days)
	  "td" "month-calendar-day-noday" "&nbsp;")

	 ;; put in the days of the month
	 (planner-calendar-render i blank-days (+ last blank-days -1)
	     "td" (if (planner-page-file
		       (planner-calendar-date-to-filename
			(list month (- i blank-days -1) year)))
		      "month-calendar-day-link"
		    "month-calendar-day-nolink")
	     (planner-calendar-published-file-href
		      (planner-calendar-date-to-filename
		       (list month (- i blank-days -1) year))
		      (int-to-string (- i blank-days -1))
		      planner-calendar-nop-buttons-flag))

	 ;; add padding days at end of month to make rule lines neat
	 (unless (zerop (mod (+ blank-days last) 7))
	   (planner-calendar-render i
	     (+ last blank-days) (+ last blank-days pad-days -1)
	     "td" "month-calendar-day-noday" "&nbsp;"))

	 "</tr>\n"
	 "</table>\n")))
    string))

(defun planner-calendar-coerce-day-page (&optional page)
  "Figure out what day page to use, based on PAGE."
  (save-match-data
    (unless page
      (or (and (setq page (planner-page-name))
	       (stringp page)
	       (string-match planner-date-regexp page))
	  (setq page (planner-today)))))
  page)

(defun planner-calendar-from-page (&optional arrows page)
  "Generate a string of html (possibly with ARROWS) for a calendar for PAGE."
  (setq page (planner-calendar-coerce-day-page page))
  (when (and (stringp page)
	     (save-match-data (string-match planner-date-regexp page)))
    (let ((year (string-to-number (substring page 0 4)))
	  (month (string-to-number (substring page 5 7))))
      (planner-calendar month year arrows))))

(defun planner-calendar-published-file-href (page &optional name nop)
  "Return an href anchor string to the published PAGE if PAGE exists."
  (if (and (stringp page)
	   (planner-page-file page)
	   (not (planner-private-p (planner-page-file page))))
      (planner-link-href page (or name page))
    (or name page)))

(defun planner-calendar-yesterday (date)
  "Return the day before DATE as a (month day year) list."
  (let* ((year (extract-calendar-year date))
	 (month (extract-calendar-month date))
	 (day (extract-calendar-day date))
	 (prev-year (if (and (= 1 month) (= 1 day)) (1- year) year))
	 (prev-month (if (= 1 day) (1+ (mod (+ month 10) 12)) month))
	 (prev-day (if (= 1 day)
		       (calendar-last-day-of-month prev-month prev-year)
		     (1- day))))
    (list prev-month prev-day prev-year)))

(defun planner-calendar-tomorrow (date)
  "Return the day after DATE as a (month day year) list."
  (let* ((year (extract-calendar-year date))
	 (month (extract-calendar-month date))
	 (day (extract-calendar-day date))
	 (last-day (calendar-last-day-of-month month year))
	 (next-year
	  (if (and (= 12 month) (= 31 day))
	      (1+ year)
	    year))
	 (next-month
	  (if (>= day last-day)
	      (1+ (mod month 12))
	    month))
	 (next-day (if (< day last-day) (1+ day) 1)))
    (list next-month next-day next-year)))

(defun planner-calendar-today (&optional max-days)
  "Return today or the first day before today with a day page."
  (planner-calendar-prev-date
   (planner-calendar-tomorrow (calendar-current-date))))

(defun planner-calendar-create-today-link (&optional name)
  "Create a link to the newest published day page.
Add this to `muse-after-publish-hook' to create a \"today\" soft
link to the newest published planner day page, on operating systems that
support POSIX \"ln\"."
  (let* ((today-name planner-calendar-today-page-name)
	 (target-file (planner-published-file (or name today-name)))
	 (source-file (planner-published-file
		       (planner-calendar-date-to-filename
			(planner-calendar-today)))))
    (when (and (stringp target-file)
	       (stringp source-file)
	       (file-exists-p source-file))
      (when (file-exists-p target-file)
	(funcall planner-delete-file-function target-file))
      (make-symbolic-link source-file target-file t))))

(defun planner-calendar-prev-date (date &optional max-days)
  "Return the first day before DATE with a day page."
  (let ((days (or max-days 180))
	(yesterday date)
	(done nil))
    (while (and (not done) (> days 0))
      (setq yesterday (planner-calendar-yesterday yesterday)
	    days (1- days))
      (let ((page (planner-calendar-date-to-filename yesterday)))
	(setq done (and (planner-page-file page)
			(not (planner-private-p (planner-page-file page)))))))
    (if done yesterday nil)))

(defun planner-calendar-next-date (date &optional max-days)
  "Return the first day after DATE with a day page."
  (let ((days (or max-days 180))
	(tomorrow date)
	(done nil))
    (while (and (not done) (> days 0))
      (setq tomorrow (planner-calendar-tomorrow tomorrow)
	    days (1- days))
      (let ((page (planner-calendar-date-to-filename tomorrow)))
	(setq done (and (planner-page-file page)
			(not (planner-private-p (planner-page-file page)))))))
    (if done tomorrow nil)))

(defun planner-calendar-prev-date-href (date name &optional nop max-days)
  "Return an href anchor string for the first day page before DATE."
  (let ((prev-date (planner-calendar-prev-date date max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename prev-date) name nop)))

(defun planner-calendar-next-date-href (date name &optional nop max-days)
  "Return an href anchor string for the first day page after DATE."
  (let ((next-date (planner-calendar-next-date date max-days)))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename next-date) name nop)))

(defun planner-calendar-prev-month-href (month year name &optional nop max-days)
  "Return an href anchor string for the last day page in the previous month."
  (let ((prev-date (planner-calendar-prev-date (list month 1 year) max-days))
	(muse-publish-desc-transforms nil)
	(planner-publish-ignore-url-desc-specials t))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename prev-date) name nop)))

(defun planner-calendar-next-month-href (month year name &optional nop max-days)
  "Return an href anchor string for the first day page in the following month."
  (let ((next-date
	 (planner-calendar-next-date
	  (list month (calendar-last-day-of-month month year) year)
	  max-days))
	(muse-publish-desc-transforms nil)
	(planner-publish-ignore-url-desc-specials t))
    (planner-calendar-published-file-href
     (planner-calendar-date-to-filename next-date) name nop)))

(defun planner-calendar-prev-day-page (&optional page max-days)
  "Return the first planner day page before this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-date-to-filename
     (planner-calendar-prev-date date max-days))))

(defun planner-calendar-next-day-page (&optional page max-days)
  "Return the first planner day page after this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-date-to-filename
     (planner-calendar-next-date date max-days))))

(defun planner-calendar-prev-date-href-from-page (name &optional page max-days)
  "Return an href anchor string for the first day page before this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-prev-date-href date name max-days)))

(defun planner-calendar-next-date-href-from-page (name &optional page max-days)
  "Return an href anchor string for the first day page after this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-next-date-href date name max-days)))

(defun planner-calendar-prev-month-href-from-page (name &optional page max-days)
  "Return a string for the last day page in first month before this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-prev-month-href date name max-days)))

(defun planner-calendar-next-month-href-from-page (name &optional page max-days)
  "Return a string for the first day page in the first month after this one."
  (unless page (setq page (planner-page-name)))
  (let ((date (planner-filename-to-calendar-date page)))
    (planner-calendar-next-month-href date name max-days)))

(defun planner-publish-calendar-tag (beg end attrs)
  (let* ((arrows (cdr (assoc "arrows" attrs)))
	 (page (cdr (assoc "page" attrs)))
	 (calendar (planner-calendar-from-page arrows page)))
    (delete-region beg end)
    (when calendar
      (planner-insert-markup "<div class=\"calendar\">\n")
      (planner-insert-markup calendar)
      (planner-insert-markup "</div>\n"))))

(eval-after-load "planner-publish"
  '(progn
     (add-to-list 'planner-publish-markup-tags
		  '("calendar" nil t nil planner-publish-calendar-tag)
		  t)
     (add-to-list 'planner-publish-finalize-regexps
		  '(200 "<\\(calendar\\)\\(\\s-+[^<>\n]+[^</>\n]\\)?\\(/\\)?>"
			0 muse-publish-markup-tag))))

(provide 'planner-calendar)

;;; planner-calendar.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
