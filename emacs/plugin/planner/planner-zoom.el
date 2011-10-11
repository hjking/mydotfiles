;;; planner-zoom.el --- navigate Planner pages

;; Copyright (C) 2006, 2008 Gregory Novak
;; Parts copyright (C) 2006, 2008 Free Software Foundation, Inc.

;; Author: Gregory Novak <novak@ucolick.org>
;; Date: 10-Mar-2006

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

;; One of the things I like about using Planner is that it gets me
;; into the habit of, at the beginning of the day, deciding what I'm
;; going to do and, at the end of the day, evaluating whether or not I
;; achieved my goals.  I'd like to do this same thing at the week
;; level, the month level, the quarter level, and the year level.
;; This way each time period breaks down into 3-4 smaller time
;; periods, and I can keep an eye on larger, longer-term goals.  (I've
;; posted one or two messages about this before).
;;
;; To this end, I've put together a little code that lets you skip
;; around on pages that correspond to the different time intervals.
;; When I'm looking at how I did over the past month, I want an easy
;; way to look at how I did for the weeks of that month.  Typing out
;; all the page names is tedious and time consuming, so I've created
;; four functions zoom-iup (for interactive-up), zoom-idown,
;; zoom-inext, and zoom-iprev (which I bind to Shift-up, Shift-down,
;; etc).
;;
;; The naming convention for pages is:
;; year - "2006.Year"
;; quarter - "2006.Quarter2"
;; month - "2006.January"
;; week - "2006.January.Week3"
;; day - "2006.01.02"
;; (this can be changed by changing zoom-regexps)
;;
;; So typically I would look at the page named "2006.January" and then
;; hit 'C-u S-down' which shows me 2006.January.Week1 in the other
;; buffer.  Then I can hit S-left and S-right to look at
;; 2006.January.Week2, 2006.January.Week3, etc.
;;
;; I determine the month to which each week belongs by the month which
;; contains the zoom-first-day-of-week'th day of that week.  Zero is
;; Sunday, one is Monday, etc.  Therefore the March 1, 2006, would
;; typically be fall into "2006.February.Week4"

;;; Contributors:

;;; Code:

(require 'planner)

(eval-when-compile (require 'cl))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Config
(defvar planner-zoom-first-day-of-week 1
  "What day should be considered the first of the week.
Zero for Sunday, one for Monday, etc")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guts
(defvar planner-zoom-months
  '(("January" . 1)
    ("February" . 2)
    ("March" . 3)
    ("April" . 4)
    ("May" . 5)
    ("June" . 6)
    ("July" . 7)
    ("August" . 8)
    ("September" . 9)
    ("October" . 10)
    ("November" . 11)
    ("December" . 12)
    ("Month" . 13)) ; Extra invalid value
  "Alist associating month names with numbers.")

(defvar planner-zoom-month-regexp
  (concat "\\(" (mapconcat #'car planner-zoom-months "\\|") "\\)")
  "Regexp matching any month name given in planner-planner-zoom-months.")

(defvar planner-zoom-regexps
  (list '("^\\([0-9]\\{4\\}\\).Year$"
          . year) ; (year)
        '("^\\([0-9]\\{4\\}\\).Quarter\\([0-5]\\)$"
          . quarter) ; (year, quarter)
        (cons (concat "^\\([0-9]\\{4\\}\\)." planner-zoom-month-regexp "$")
              'month) ; (year, month)
        (cons (concat "^\\([0-9]\\{4\\}\\)."
                      planner-zoom-month-regexp
                      ".Week\\([0-6]\\)$")
              'week); year, month, week
        '("^\\([0-9]\\{4\\}\\).\\([0-9]\\{1,2\\}\\).\\([0-9]\\{1,2\\}\\)$"
          . day)) ; year, month, day
  "Alist of regexps that match names of years, quarters, months,
weeks, and days.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Heavy lifting functions
(defun planner-zoom-parse-to-strings (name &optional type)
  "Parse a string NAME, into a period of time given by `planner-zoom-regexps'.

If TYPE is given, it is a symbol specifying the type of
time-period which NAME should be parsed as (one of 'day, 'week,
'month, 'quarter, or 'year.

Return a four element list consisting of the type of time
period and then a list of strings containing the subexpressions
matched by the successful regexp.  Eg,
\(planner-zoom-parse-to-strings \"2006.Year\") returns
\(year \"2006\" nil nil) and
\(planner-zoom-parse-to-strings \"2006.January.Week1\") returns
\(week \"2006\" \"January\" \"1\")"
  (setq type (or type (assoc-default name planner-zoom-regexps 'string-match)))
  ;; Make sure the match data is for the right search
  (unless (string-match (car (rassoc type planner-zoom-regexps)) name)
    (error "planner-zoom: Couldn't parse name"))
  (cons type (list (match-string 1 name)
                   (match-string 2 name)
                   (match-string 3 name))))

(defun planner-zoom-parse (&rest args)
  "Parse a string NAME, into a period of time given by `planner-zoom-regexps'.

If TYPE is given, it is a symbol specifying the type of
time-period which NAME should be parsed as (one of 'day, 'week,
'month, 'quarter, or 'year.

Return a four element list consisting of the type of time period
and then numerical representations of the subexpressions matched
by the successful regexp.

Eg,
\(planner-zoom-parse \"2006.Year\") returns (year 2006 nil nil)
and (planner-zoom-parse \"2006.January.Week1\") returns (week 2006 1 1)."

  (let* ((result (apply 'planner-zoom-parse-to-strings args))
         (type (car result))
         (strings (cdr result))
         numbers)
    (dotimes (i (length strings))
      (setq numbers (cons (when (not (null (nth i strings)))
                            (if (or (and (eq type 'month) (= i 1))
                                    (and (eq type 'week) (= i 1)))
                                (cdr (assoc (nth i strings)
                                            planner-zoom-months))
                              (string-to-number (nth i strings))))
                          numbers)))
    (cons type (nreverse numbers))))

(defun planner-zoom-string (type &rest changes)
  "Convert time-range info into a string name.  You can specify
numerical values or strings.

So,
\(planner-zoom-string 'year 2006) -> \"2006.Year\"
\(planner-zoom-string 'year \"2006\") -> \"2006.Year\"
\(planner-zoom-string 'week 2006 \"February\" 3) -> \"2006.February.Week3\"
\(planner-zoom-string 'week 2006 2 3) -> \"2006.February.Week3\""
  ;; use a template
  (let ((name (cdr (assoc type '((year . "1000.Year")
                                 (quarter . "1000.Quarter5")
                                 (month . "1000.Month")
                                 (week . "1000.Month.Week6")
                                 (day . "1000.99.99"))))))

    ;; Make sure changes are strings
    (let (result)
      (dotimes (i (length changes))
        (setq result (cons (if (not (numberp (nth i changes)))
                               (nth i changes)
                             (if (or (and (eq type 'month) (= i 1))
                                     (and (eq type 'week) (= i 1)))
                                 (car (rassoc (nth 1 changes)
                                              planner-zoom-months))
                               (number-to-string (nth i changes))))
                           result)))
      (setq changes (nreverse result)))

    ;; Special handling for days + months in 'day strings: make sure
    ;; they're two digits
    (when (eq type 'day)
      (setq changes (mapcar (lambda (x) (if (= (length x) 1)
                                            (concat "0" x)
                                          x))
                            changes)))

    (dotimes (i (length changes))
      (planner-zoom-parse name type)    ; make sure match data is
                                        ; correct each time
      (setq name (replace-match (nth i changes) t t name (1+ i))))
    name))

(defun planner-zoom-range (min max)
  "Return a list of numbers from MIN to MAX."
  (let ((lst nil))
    (while (<= min max)
      (setq lst (cons max lst))
      (setq max (1- max)))
    lst))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interactive

(add-hook 'planner-mode-hook
          (lambda ()
            (local-set-key (kbd "<S-up>") 'planner-zoom-iup)
            (local-set-key (kbd "<S-down>") 'planner-zoom-idown)
            (local-set-key (kbd "<S-left>") 'planner-zoom-iprev)
            (local-set-key (kbd "<S-right>") 'planner-zoom-inext)))

(defun planner-zoom-iup (name other-window)
  "Move to the next higher level in the hierarchy."
  (interactive (list (planner-page-name)
                     current-prefix-arg))
  (when other-window (other-window 1))
  (planner-find-file (planner-zoom-up name))
  (when other-window (other-window 1)))

(defun planner-zoom-idown (name other-window)
  "Move to the next lower level in the hierarchy.
If the current date is within the higher-level time range, zoom
to the lower level time range that also contains today.
Otherwise, just go to the first lower-level time range."
  (interactive (list (planner-page-name)
                     current-prefix-arg))
  (when other-window (other-window 1))
  (planner-find-file (planner-zoom-down name))
  (when other-window (other-window 1)))

(defun planner-zoom-inext (name num other-window)
  "Move to the next time range at the same level in the
hierarchy.  With a numeric prefix arg, move by that number of
time ranges.  With a non-numeric prefix arg, show the desired
page in the other window."
  (interactive (list (planner-page-name)
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       1)
                     (consp current-prefix-arg)))
  (when other-window (other-window 1))
  (planner-find-file (planner-zoom-next name num))
  (when other-window (other-window 1)))

(defun planner-zoom-iprev (name num other-window)
  "Move to the previous time range at the same level in the
hierarchy.  With a numeric prefix arg, move by that number of
time ranges.  With a non-numeric prefix arg, show the desired
page in the other window."
  (interactive (list (planner-page-name)
                     (if (numberp current-prefix-arg)
                         current-prefix-arg
                       1)
                     (consp current-prefix-arg)))
  (when other-window (other-window 1))
  (planner-find-file (planner-zoom-next name (- num)))
  (when other-window (other-window 1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Non-period-specific stuff
(defun planner-zoom-contains (name &optional today)
  "Test if TODAY is contained within the time period specified by
string NAME.  If TODAY is not given, use the current date"
  (setq today (or today (gsn/calendar-today-absolute)))
  (and (<= (planner-zoom-beg name) today)
       (>= (planner-zoom-end name) today)))

(defun planner-zoom-beg (name)
  "Return the absolute date of the beginning of the time period
specified by string NAME."
  (funcall
   ;; This is basically do-it-yourself object orientation.  Times are
   ;; lists where the first element is the type and the other elements
   ;; are type-specific information.  This function call dispatches on
   ;; the type, so it's basically a method call on a time range.
   (cdr (assoc (car (planner-zoom-parse name))
               '((year . planner-zoom-year-beg)
                 (quarter . planner-zoom-quarter-beg)
                 (month . planner-zoom-month-beg)
                 (week . planner-zoom-week-beg)
                 (day . planner-zoom-day-beg))))
   name))

(defun planner-zoom-end (name)
  "Return the absolute date of the end of the time period
specified by string NAME."
  (funcall
   ;; See planner-zoom-beg comments
   (cdr (assoc (car (planner-zoom-parse name))
               '((year . planner-zoom-year-end)
                 (quarter . planner-zoom-quarter-end)
                 (month . planner-zoom-month-end)
                 (week . planner-zoom-week-end)
                 (day . planner-zoom-day-end))))
   name))

(defun planner-zoom-up (name)
  "For time range given by string NAME, return a string
representiang the next higher enclosing time range in the
heirarchy."
  (funcall
   ;; See planner-zoom-beg comments
   (cdr (assoc (car (planner-zoom-parse name))
               '((year . planner-zoom-up-year)
                 (quarter . planner-zoom-up-quarter)
                 (month . planner-zoom-up-month)
                 (week . planner-zoom-up-week)
                 (day . planner-zoom-up-day))))
   name))

(defun planner-zoom-down (name)
  "For time range given by string NAME, return a string
representiang the next lower time range in the heirarchy.  If the
current date is within the higher-level time range, choose the
lower-level time range that also includes the current date.
Otherwise, just retturn the first lower-level time range"
  (funcall
   ;; See planner-zoom-beg comments
   (cdr (assoc (car (planner-zoom-parse name))
               '((year . planner-zoom-down-year)
                 (quarter . planner-zoom-down-quarter)
                 (month . planner-zoom-down-month)
                 (week . planner-zoom-down-week)
                 (day . planner-zoom-down-day))))
   name))

(defun planner-zoom-next (name num)
  "For time range given by string NAME, return a string
representiang the next time range at the same level in the
heirarchy."
  (funcall
   ;; See planner-zoom-beg comments
   (cdr (assoc (car (planner-zoom-parse name))
               '((year . planner-zoom-next-year)
                 (quarter . planner-zoom-next-quarter)
                 (month . planner-zoom-next-month)
                 (week . planner-zoom-next-week)
                 (day . planner-zoom-next-day))))
   name num))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Year
(defun planner-zoom-year-beg (name)
  "Return the absolute date of the beginning of the year
specified by string NAME."
  (multiple-value-bind (type year) (planner-zoom-parse name 'year)
    (calendar-absolute-from-gregorian (list 1 1 year))))

(defun planner-zoom-year-end (name)
  "Return the absolute date of the end of the year specified by
string NAME."
  (multiple-value-bind (type year) (planner-zoom-parse name 'year)
    (calendar-absolute-from-gregorian (list 12 31 year))))

(defun planner-zoom-up-year (name)
  "Error: there's nothing above year in the heirarchy."
  nil)

(defun planner-zoom-next-year (name num)
  "Return a string NUM years after the one given by string NAME."
  (multiple-value-bind (type year) (planner-zoom-parse name 'year)
    (planner-zoom-string 'year (+ num year))))

(defun planner-zoom-down-year (name &optional today)
  "If the absolute date TODAY is within the year specified by
NAME, return a string for the quarter that also contains TODAY.
Otherwise, return the a string for the first quarter in the year.
If TODAY is not given, use the current date."
  (multiple-value-bind (junk year) (planner-zoom-parse name 'year)
    (if (not (planner-zoom-contains name today))
        (planner-zoom-string 'quarter year 1)
      (car (planner-remove-if-not
            (lambda (p) (planner-zoom-contains p today))
            (mapcar (lambda (n)
                      (planner-zoom-string 'quarter year n))
                    (planner-zoom-range 1 4)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Quarter
(defun planner-zoom-up-quarter (name)
  "Return a string for the year containing the quarter specified
by string NAME."
  (multiple-value-bind (type year quarter) (planner-zoom-parse name 'quarter)
    (planner-zoom-string 'year year)))

(defun planner-zoom-quarter-beg (name)
  "Return the absolute date of the first day of the quarter given
by string NAME."
  (multiple-value-bind (type year quarter) (planner-zoom-parse name 'quarter)
    (calendar-absolute-from-gregorian (list (1+ (* 3 (1- quarter))) 1 year))))

(defun planner-zoom-quarter-end (name)
  "Return the absolute date of the last day of the quarter given
by string NAME"
  (multiple-value-bind (type year quarter) (planner-zoom-parse name 'quarter)
    (cond ((= 1 quarter) (calendar-absolute-from-gregorian (list 3 31 year)))
          ((= 2 quarter) (calendar-absolute-from-gregorian (list 6 30 year)))
          ((= 3 quarter) (calendar-absolute-from-gregorian (list 9 30 year)))
          ((= 4 quarter)
           (calendar-absolute-from-gregorian (list 12 31 year))))))

(defun planner-zoom-next-quarter (name num)
  "Return a string for the name of the NUMth quarter after the
one given by string NAME."
  (multiple-value-bind (type year quarter) (planner-zoom-parse name 'quarter)
    (let ((new-year (+ year (floor (/ (1- (float (+ quarter num))) 4))))
          (new-quarter (1+ (mod (1- (+ quarter num)) 4))))
      (planner-zoom-string 'quarter new-year new-quarter))))

(defun planner-zoom-down-quarter (name &optional today)
  "If the absolute TODAY is within the quarter given by string
NAME, return a string for the month that also contains TODAY.
Otherwise, return a string for the first month in the quarter.
If TODAY is not given, use the current date."
  (multiple-value-bind (type year quarter) (planner-zoom-parse name 'quarter)
    (if (not (planner-zoom-contains name today))
        (planner-zoom-string 'month year (1+ (* (1- quarter) 3)))
      ;; inefficient, but correct, to just include all months in the
      ;; test since we know that the current quarter contains today,
      ;; therefore some month in another quarter _cannot_ contain
      ;; today
      (car (planner-remove-if-not
            (lambda (p) (planner-zoom-contains p today))
            (mapcar (lambda (n)
                      (planner-zoom-string 'month year n))
                    (planner-zoom-range 1 12)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Month

(defun planner-zoom-month-beg (name)
  "Return the absolute date of the first day of the month given
by the string NAME."
  (multiple-value-bind (type year month) (planner-zoom-parse name 'month)
    (calendar-absolute-from-gregorian (list month 1 year))))

(defun planner-zoom-month-end (name)
  "Return the absolute date of the last day of the month given
by the string NAME."
  (multiple-value-bind (type year month) (planner-zoom-parse name 'month)
    (calendar-absolute-from-gregorian
     (list month (calendar-last-day-of-month month year) year))))

(defun planner-zoom-up-month (name)
  "Return a string for the quarter containing the month given by string NAME."
  (multiple-value-bind (type year month) (planner-zoom-parse name)
    (let ((quarter (1+ (/ (1- month) 3))))
      (planner-zoom-string 'quarter year quarter))))

(defun planner-zoom-next-month (name num)
  "Return a string for the NUMth month after the one given by the string NAME."
  (multiple-value-bind (type year month) (planner-zoom-parse name 'month)
    (let ((new-year (+ year (floor (/ (1- (float (+ month num))) 12))))
          (new-month (1+ (mod (1- (+ month num)) 12))))
      (planner-zoom-string 'month new-year new-month))))

(defun planner-zoom-down-month (name &optional today)
  "If the absolute date TODAY is within the month given by the
string NAME, return a string for the week that also contains
TODAY.  Otherwise, return a string for the first week in the
month.  If TODAY is not given, use the current date."
  (multiple-value-bind (type year month) (planner-zoom-parse name 'month)
    (if (not (planner-zoom-contains name today))
        (planner-zoom-string 'week year month 1)
      (car (planner-remove-if-not
            (lambda (p) (planner-zoom-contains p today))
            (mapcar (lambda (n)
                      (planner-zoom-string 'week year month n))
                    (planner-zoom-range 1 5)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Week

(defun planner-zoom-week-beg (name)
  "Return the absolute date of the first day of the week given by string NAME."
  (multiple-value-bind (type year month week) (planner-zoom-parse name 'week)
    (calendar-absolute-from-gregorian
     (calendar-nth-named-day week planner-zoom-first-day-of-week month year))))

(defun planner-zoom-week-end (name)
  "Return the absolute date of the last day of the week given by string NAME."
  (+ 6 (planner-zoom-week-beg name)))

(defun planner-zoom-up-week (name)
  "Return a string for the month containing the week given by string NAME."
  (multiple-value-bind (type year month week) (planner-zoom-parse name 'week)
    (planner-zoom-string 'month year month)))

(defun planner-zoom-next-week (name num)
  "Return a string for the NUMth week after the one specified by
the string NAME."
  (multiple-value-bind (type year month week) (planner-zoom-parse name 'week)
    ;; New week <= 0 leads to problems with nth-named-day... try to fix them?
    (let* ((new-week (if (> (+ week num) 0)
                         (+ week num)
                       (1- (+ week num))))
           (new-date (calendar-nth-named-day
                      new-week planner-zoom-first-day-of-week month year 1))
           (new-year (extract-calendar-year new-date))
           (new-month (extract-calendar-month new-date))
           (new-day (extract-calendar-day new-date))
           (first-date (calendar-nth-named-day
                        1 planner-zoom-first-day-of-week new-month new-year 1))
           (first-day (extract-calendar-day first-date))
           (new-week (1+ (/ (- new-day first-day) 7))))
      (planner-zoom-string 'week new-year new-month new-week))))

(defun planner-zoom-down-week (name &optional today)
  "If the absolute date TODAY is within the week specified by
string NAME, return a string for TODAY.  Otherwise, return the
first day in the week.  If TODAY is not given, use the current
date."
  (setq today (or today (gsn/calendar-today-absolute)))
  (multiple-value-bind (type year month week) (planner-zoom-parse name 'week)
    (if (not (planner-zoom-contains name today))
        (planner-zoom-string 'day year month
                             (extract-calendar-day
                              (calendar-nth-named-day
                               week planner-zoom-first-day-of-week
                               month year)))
      (let* ((today (calendar-gregorian-from-absolute today))
             (year (extract-calendar-year today))
             (month (extract-calendar-month today))
             (day (extract-calendar-day today)))
        (planner-zoom-string 'day year month day)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Day

(defun planner-zoom-day-beg (name)
  "Return the absolute date of the day given by the string NAME."
  (multiple-value-bind (type year month day) (planner-zoom-parse name 'day)
    (calendar-absolute-from-gregorian (list month day year))))

(defun planner-zoom-day-end (name)
  "Return the absolute date of the day given by the string NAME."
  (planner-zoom-day-beg name))

(defun planner-zoom-up-day (name)
  "Return a string for the week that contains the day given by
the string NAME."
  (multiple-value-bind (type year month day) (planner-zoom-parse name 'day)
    (let* ((first-date (calendar-nth-named-day
                        1 planner-zoom-first-day-of-week month year))
           (first-day (extract-calendar-day first-date))
           (week (1+ (/ (- day first-day) 7))))
      (planner-zoom-string 'week year month week))))

(defun planner-zoom-next-day (name num)
  "Return the NUMth day after the one given by the string NAME."
  (let ((new-date (calendar-gregorian-from-absolute
                   (+ (planner-zoom-day-beg name) num))))
    (planner-zoom-string 'day
                         (extract-calendar-year new-date)
                         (extract-calendar-month new-date)
                         (extract-calendar-day new-date))))

(defun planner-zoom-down-day (name)
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar planner-zoom-tests
  '((planner-zoom-parse-to-strings ("2006.Year")  (year "2006" nil nil))
    (planner-zoom-parse-to-strings ("2006.January")
                                   (month "2006" "January" nil))
    (planner-zoom-parse-to-strings ("2006.Quarter1") (quarter "2006" "1" nil))
    (planner-zoom-parse-to-strings ("2006.January.Week1")
                                   (week "2006" "January" "1"))
    (planner-zoom-parse-to-strings ("2006.01.03") (day "2006" "01" "03"))

    (planner-zoom-parse ("2006.Year") (year 2006 nil nil))
    (planner-zoom-parse ("2006.January") (month 2006 1 nil))
    (planner-zoom-parse ("2006.Quarter1") (quarter 2006 1 nil))
    (planner-zoom-parse ("2006.January.Week1") (week 2006 1 1))
    (planner-zoom-parse ("2006.01.03") (day 2006 1 3))

    (planner-zoom-string (year 2007) "2007.Year")
    (planner-zoom-string (year "2007") "2007.Year")
    (planner-zoom-string (quarter 2007 2) "2007.Quarter2")
    (planner-zoom-string (quarter "2007" "2") "2007.Quarter2")
    (planner-zoom-string (month 2007 2) "2007.February")
    (planner-zoom-string (month "2007" "February") "2007.February")
    (planner-zoom-string (week 2007 2 2) "2007.February.Week2")
    (planner-zoom-string (week "2007" "February" "2") "2007.February.Week2")
    (planner-zoom-string (day 2007 2 2) "2007.02.02")
    (planner-zoom-string (day "2007" "2" "2") "2007.02.02")

    (planner-zoom-contains ("2006.Year" 732311) nil)
    (planner-zoom-contains ("2006.Year" 732312) t)
    (planner-zoom-contains ("2006.Year" 732463) t)
    (planner-zoom-contains ("2006.Year" 732676) t)
    (planner-zoom-contains ("2006.Year" 732677) nil)

    (planner-zoom-year-beg ("2006.Year") 732312)
    (planner-zoom-quarter-beg ("2006.Quarter1") 732312)
    (planner-zoom-quarter-beg ("2006.Quarter2") 732402)
    (planner-zoom-quarter-beg ("2006.Quarter3") 732493)
    (planner-zoom-quarter-beg ("2006.Quarter4") 732585)
    (planner-zoom-month-beg ("2006.January") 732312)
    (planner-zoom-week-beg ("2006.January.Week1") 732313)
    (planner-zoom-week-beg ("2006.January.Week2") 732320)
    (planner-zoom-week-beg ("2006.January.Week3") 732327)
    (planner-zoom-week-beg ("2006.January.Week4") 732334)
    (planner-zoom-week-beg ("2006.January.Week5") 732341)
    (planner-zoom-week-beg ("2006.January.Week6") 732348)
    (planner-zoom-day-beg ("2006.02.03") 732345)

    (planner-zoom-year-end ("2006.Year") 732676)
    (planner-zoom-quarter-end ("2006.Quarter1") 732401)
    (planner-zoom-quarter-end ("2006.Quarter2") 732492)
    (planner-zoom-quarter-end ("2006.Quarter3") 732584)
    (planner-zoom-quarter-end ("2006.Quarter4") 732676)
    (planner-zoom-month-end ("2006.January") 732342)
    (planner-zoom-week-end ("2006.January.Week1") 732319)
    (planner-zoom-week-end ("2006.January.Week2") 732326)
    (planner-zoom-week-end ("2006.January.Week3") 732333)
    (planner-zoom-week-end ("2006.January.Week4") 732340)
    (planner-zoom-week-end ("2006.January.Week5") 732347)
    (planner-zoom-week-end ("2006.January.Week6") 732354)
    (planner-zoom-day-end ("2006.01.01")  732312)

    (planner-zoom-next-year ("2006.Year" 2) "2008.Year")
    (planner-zoom-next-year ("2006.Year" -2) "2004.Year")
    (planner-zoom-next-year ("2006.Year" 0) "2006.Year")
    (planner-zoom-next-quarter ("2006.Quarter2" 5) "2007.Quarter3")
    (planner-zoom-next-quarter ("2006.Quarter2" -5) "2005.Quarter1")
    (planner-zoom-next-quarter ("2006.Quarter2" 0) "2006.Quarter2")
    (planner-zoom-next-month ("2006.June" 13) "2007.July")
    (planner-zoom-next-month ("2006.June" -13) "2005.May")
    (planner-zoom-next-month ("2006.June" 0) "2006.June")
    (planner-zoom-next-week ("2006.April.Week2" 3) "2006.May.Week1")
    (planner-zoom-next-week ("2006.April.Week2" -2) "2006.March.Week4")
    (planner-zoom-next-week ("2006.April.Week2" 0) "2006.April.Week2")
    (planner-zoom-next-day ("2006.04.03" -7) "2006.03.27")
    (planner-zoom-next-day ("2006.04.03" -1) "2006.04.02")
    (planner-zoom-next-day ("2006.04.03" 0) "2006.04.03")
    (planner-zoom-next-day ("2006.04.03" 1) "2006.04.04")
    (planner-zoom-next-day ("2006.04.03" 28) "2006.05.01")

    (planner-zoom-up-quarter ("2006.Quarter1") "2006.Year")
    (planner-zoom-up-month ("2006.April") "2006.Quarter2")
    (planner-zoom-up-week ("2006.April.Week1") "2006.April")
    (planner-zoom-up-day ("2006.04.10") "2006.April.Week2")

    ;(calendar-absolute-from-gregorian (4 30 2006) 732431)
    ;(calendar-absolute-from-gregorian (4 30 2005) 732066)

    ;; April 30th, 2006: Should zoom down to Q2, Month 4, Week 4, day 4.30.2006
    (planner-zoom-down-year ("2006.Year" 732431) "2006.Quarter2")
    (planner-zoom-down-quarter ("2006.Quarter2" 732431) "2006.April")
    (planner-zoom-down-month ("2006.April" 732431) "2006.April.Week4")
    (planner-zoom-down-week ("2006.April.Week4" 732431) "2006.04.30")

    ;; April 30th, 2005: Should zoom down to Q1, January, Week 1, 1.1.2006
    (planner-zoom-down-year ("2006.Year" 732066) "2006.Quarter1")
    (planner-zoom-down-quarter ("2006.Quarter1" 732066) "2006.January")
    (planner-zoom-down-month ("2006.January" 732066) "2006.January.Week1")
    (planner-zoom-down-week ("2006.January.Week1" 732066) "2006.01.02"))
  "A list of lists of the form (function-name function-arguments
desired-result) which is used to test the functions in the zoom
package.")

(defun planner-zoom-test ()
  "Run all the tests in planner-zoom-tests."
  (dolist (test planner-zoom-tests)
    (let* ((fn (first test))
           (fn-args (second test))
           (desired-result (third test))
           (result (apply fn fn-args)))
      (when (not (equal desired-result result))
        (error "Failed test!"))))
  t)

(defun gsn/calendar-today-gregorian ()
  (multiple-value-bind (junk junk junk day month year) (decode-time)
    (list month day year)))

(defun gsn/calendar-today-absolute ()
  (calendar-absolute-from-gregorian (gsn/calendar-today-gregorian)))

(provide 'planner-zoom)

;;; planner-zoom.el ends here
