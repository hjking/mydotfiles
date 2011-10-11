;;; planner-appt.el --- appointment alerts from planner
;;
;;
;; Copyright (C) 2005, 2006, 2007, 2008  Jim Ottaway <j.ottaway@lse.ac.uk>
;; Copyright (C) 2005, 2008  Henrik S. Hansen <hsh@freecode.dk>
;; Parts copyright (C) 2005, 2006, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2005, 2008 Seth Falcon <sethfalcon AT gmail.com>
;;
;; Author: Jim Ottaway
;; Keywords: hypermedia
;;

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

;;
;; Please report any bugs that you come across to the authors at the
;; addresses given above.
;;
;; Usage:
;;
;; Add "(planner-appt-insinuate)" to your configuration to make
;; Planner work with appt.
;;
;; See the Appointments section of the Planner manual for further
;; details.
;;
;; Contributors:
;;
;; * Seth Falcon supplied the idea and the code that is the basis of
;;   the forthcoming appointments display functionality.
;;
;;; TODO:
;;
;; * Correct sorting of task appointments
;;
;; * Consider changing "insinuate" into "install".  I don't like the
;; word "insinuate" very much!  Or a minor mode perhaps:
;; planner-appt-minor-mode
;;
;; * A lot of the code properly belongs elsewhere: schedule sorting,
;; schedule cyclical entries, calendar marking...
;;

;;; Code:


(require 'planner)
(require 'appt)
(require 'calendar)

;;; Customization

(defgroup planner-appt nil
  "Appointment integration for planner.el."
  :prefix "planner-appt-"
  :group 'planner)

(defcustom planner-appt-schedule-section "Schedule"
  "The name of the section where the schedule is to be found."
  :group 'planner-appt
  :type 'string)

(defcustom planner-appt-font-lock-appointments-flag t
  "Non-nil means use font-locking for appointments."
  :group 'planner-appt
  :type '(choice (const :tag "Use font-locking" t)
		 (const :tag "Don't use font-locking" nil)))

(defcustom planner-appt-update-appts-on-save-flag nil
  "Non-nil means update appointment alerts on saving today's plan."
  :group 'planner-appt
  :type '(choice (const :tag "Update on save" t)
		 (const :tag "Don't update on save" nil)))

(defcustom planner-appt-sort-schedule-on-update-flag nil
  "Non-nil means sort the schedule when updating appointments."
  :group 'planner-appt
  :type '(choice (const :tag "Sort on update" t)
		 (const :tag "Don't sort on update" nil)))


(defcustom planner-appt-update-hook '()
  "Hook run after appointments have been updated."
  :group 'planner-appt
  :type 'hook)

(defcustom planner-appt-schedule-cyclic-behaviour 'today
  "Determines the behaviour of cyclical schedule insertion.
Used after `planner-appt-schedule-cyclic-insinuate' has been called.
'today means only add cylical schedule entries for today
'future means add cyclical entries for all future day pages visited."
  :group 'planner-appt
  :type '(choice (const :tag "For today only" today)
		 (const :tag "For all future pages." future)))

(defcustom planner-appt-alert-buffer "*Alerts*"
  "Name of the buffer for displaying active alerts.
Used by `planner-appt-show-alerts'."
  :group 'planner-appt
  :type 'string)


(defcustom planner-appt-task-use-appointments-section-flag nil
  "When non-nil, task appointments will be copied to an appoinments section.
The section name is supplied by
`planner-appt-task-appointments-section'."
  :group 'planner-appt
  :type 'boolean)

(defcustom planner-appt-task-appointments-section "Schedule"
  "Name of the section where task appointments are copied.
The copying is contingent upon
`planner-appt-task-use-appointments-section-flag'."
  :group 'planner-appt
  :type 'string)

(defcustom planner-appt-format-appt-section-line-function
  #'planner-appt-format-appt-section-line
  "The function used when formatting an appointment section line.

This function should take one argument: an appointment description.
The description is in the form used when an appointment alert is
signalled: a string with the time of the appointment and some text
such as \"12:00 Do something\".  Look at the default function
`planner-appt-format-appt-section-line' for inspiration if you want to
make a different format."
  :group 'planner-appt
  :type 'function)

(defcustom planner-appt-limit-highlighting-flag t
  "When non-nil, only highlight appointment times in tasks and the schedule.
When nil, all appointment times are highlighted, wherever they may be
in the buffer."
  :group 'planner-appt
  :type 'boolean)

(defcustom planner-appt-forthcoming-days 7
  "Number of days to look ahead for appointments."
  :group 'planner-appt
  :type 'integer)

(defcustom planner-appt-forthcoming-appt-section "Forthcoming Appointments"
  "Title of the section for forthcoming appointments."
  :group 'planner-appt
  :type 'string)

(defcustom planner-appt-forthcoming-show-day-names-flag t
  "When non nil, show day names in forthcoming appointments."
  :group 'planner-appt
  :type 'boolean)

(defcustom planner-appt-forthcoming-repeat-date-string "          "
  "String to insert for repeated dates.
When there are multiple appointments for a date, the date is inserted
in the first appointment and the others have this string in their date
cell.

If the string consists of anything other than whitespace, then a link
to the day page for the appoinment is created."
  :group 'planner-appt
  :type 'string)

(defcustom planner-appt-forthcoming-look-at-cyclic-flag nil
  "When non nil, add cyclic entries to the forthcoming appointments section."
  :group 'planner-appt
  :type 'boolean)


;; Regular Expressions

;; TODO: Should these really be customizable anyway?

;; TODO: Dynamically changing dependent customizations; i.e., if this
;; is changed, all the other time-based regexps should change too [I
;; don't understand customize well enough to do this].

(defcustom planner-appt-time-regexp
  "[0-9]?[0-9]:[0-5][0-9]\\(?:am\\|pm\\)?"
  "Regular expression matching times."
  :group 'planner-appt
  :type 'regexp)

(defcustom planner-appt-task-regexp
  (concat "[@!][ \t]*\\(" planner-appt-time-regexp "\\)[ \t]*")
  "If a task description matches this regexp, it's an appointment.
Match group 1 is the time of the appointment.
Used with the task-based method.  If you use schedules, look at
`planner-appt-schedule-appt-regexp'."
  :group 'planner-appt
  :type 'regexp)

(defcustom planner-appt-task-nagging-regexp
  (concat "![ \t]*\\(" planner-appt-time-regexp "\\)[ \t]*")
  "If a task description matches this regexp, it's a nagging
appointment.  Used with the task-based method.  If you use schedules,
look at `planner-appt-schedule-appt-regexp'."
  :group 'planner-appt
  :type 'regexp)

(defcustom planner-appt-schedule-basic-regexp
  (concat
   "\\("
   ;; the appointment time (match group 1)
   planner-appt-time-regexp
   "\\)"
   ;; possibly some space, possibly a |, and any amount of space
   "[ \t]*|?[ \t]*"
   ;; perhaps another time [the end time] (match group 2)
   "\\("
   planner-appt-time-regexp
   "\\)?"
   ;; possibly some space or some ?' chars, possibly a |, and any
   ;; amount of space
   "[' \t]*|?[ \t]*"
   ;; the appointment text (match group 3)
   "\\(.+\\)")
  "Basic regular expression to match a schedule.
Match group 1 should yield the start time, match group 2 the stop
time, and match group 3 the schedule text."
  :group 'planner-appt)

;; NB: The groups are shifted in this regexp.
(defcustom planner-appt-schedule-regexp
  (concat
   ;; any amount of whitespace possibly followed by @ and any amount
   ;; of whitespace
   "^[ \t]*\\(@?\\)[ \t]*"
   ;; followed by the basic regexp
   planner-appt-schedule-basic-regexp)
  "Regexp matching schedule entries.
Match group 1 should match at most one leading instance of the
appointment marker, Match group 2 should yield the start time, match
group 3 the stop time, and match group 4 the schedule text."
  :group 'planner-appt
  :type 'regexp)

(defcustom planner-appt-schedule-appt-regexp
  (concat
   ;; any amount of whitespace followed by @ and any amount of
   ;; whitespace
   "^[ \t]*@[ \t]*"
   ;; followed by the basic regexp
   planner-appt-schedule-basic-regexp)
  "Regexp matching appointments in the schedule requiring alerts.
Used with the schedule-based method.  If you use tasks for appointments,
look at `planner-appt-task-regexp.'
Match group 1 should yield the start time, match group 2 the stop
time, and match group 3 the alert text."
  :group 'planner-appt
  :type 'regexp)


;;; Planner Miscellany

;; Could be useful elsewhere in planner?

(defun planner-appt-todays-page-p ()
  "Return t if the current page is today's, otherwise nil."
  (string= (planner-page-name) (planner-today)))

(defun planner-appt-seek-to-end-of-current-section ()
  "Go to the end of the current section."
  (goto-char
   (or (and (re-search-forward "^\\*[^*\n]" nil t)
	    (1- (planner-line-beginning-position)))
       (point-max))))

(defvar planner-appt-write-file-hook
  (if (and (boundp 'write-file-functions)
	   (not (featurep 'xemacs)))
      'write-file-functions
    'write-file-hooks)
  "The write file hook to use.")

;;; Planner-Appt Miscellany

(defvar planner-appt-debug-buffer "*planner-appt debug messages*"
  "The buffer to put debugging messages from planner-appt.")

(defvar planner-appt-debug-flag nil
  "Non-nil means turn on planner-appt debugging.")

(defmacro planner-appt-debug (form &rest body)
  "Evaluate FORM if `planner-appt-debug-flag' is non-nil.
Optional BODY is evaluated otherwise."
`(if planner-appt-debug-flag
     ,form
   ,@body))

(defun planner-appt-debug-message (&rest args)
  "Insert ARGS into `planner-appt-debug-buffer'.
This code runs only if `planner-appt-debug-flag' is non-nil."
  (planner-appt-debug
   (with-current-buffer
       (get-buffer-create planner-appt-debug-buffer)
     (goto-char (point-max))
     (apply #'insert args)
     (insert ?\n))))

(defun planner-appt-earlier-than-now-p (time)
  "Return t if TIME is earlier than the current time.
Time formats are those used by the appt appointment system."
  ;; From appt-check
  (let* ((now (decode-time))
	 (cur-hour (nth 2 now))
	 (cur-min (nth 1 now))
	 (cur-time (+ (* cur-hour 60) cur-min)))
    (> cur-time (appt-convert-time time))))

;; Not used in this file, but added for completeness.
(defun planner-appt-later-than-now-p (time)
  "Return t if TIME is later than the current time.
Time formats are those used by the appt appointment system."
  ;; From appt-check
  (let* ((now (decode-time))
	 (cur-hour (nth 2 now))
	 (cur-min (nth 1 now))
	 (cur-time (+ (* cur-hour 60) cur-min)))
    (< cur-time (appt-convert-time time))))


(defvar --planner-appt-tasks-added-appts '()
  "Internal variable: Tracks added task-based appointment alerts.")

(defvar --planner-appt-tasks-earlier-appts '()
  "Internal variable:
Tracks appointments ignored because they were too early.")

(defun planner-appt-clear-appts (appt-list)
  (while appt-list
    (setq appt-time-msg-list
	  (delete (pop appt-list) appt-time-msg-list))))

(defun planner-appt-format-time-and-description (time description)
"Format TIME [a string] and DESCRIPTION as an appointment."
  (concat time " " description))

(eval-and-compile
  (if (> emacs-major-version 21)
      (defun planner-appt-make-appt-element (time text)
	(list
	 (list (appt-convert-time time))
	 (planner-appt-format-time-and-description time text)
	 t))
    (defun planner-appt-make-appt-element (time text)
	(list
	 (list (appt-convert-time time))
	 (planner-appt-format-time-and-description time text)))))

(defun planner-appt-remember-appt (time text list)
  "Store details of an appointment with TIME and TEXT in LIST.
Return the new list."
  (push (planner-appt-make-appt-element time text) list))

(defun planner-appt-forget-appt (appt appt-list)
  "Remove APPT from APPT-LIST and return the new list.
APPT is in the appt format."
  (delete (car (member appt appt-list)) appt-list))

(defun planner-appt-add-hook (hook function &optional append global)
  "Add to the value of HOOK the function FUNCTION.
This is `add-hook' with local and global switched.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument APPEND is non-nil, in which case
FUNCTION is added at the end.
The optional fourth argument, GLOBAL, if non-nil, says to modify
the hook's global value rather than its local value."
  (add-hook hook function append (not global)))


(defun planner-appt-remove-task-id (description)
  (if (string-match
	 (concat "\\s-*"
		 (if (boundp 'planner-id-regexp)
		     planner-id-regexp))
	 description)
      (replace-match "" t t description)
    description))


(defun planner-appt-format-description (description)
  (planner-appt-remove-task-id
   (planner-remove-links description)))


;;; Advice

;; for speedy enabling and disabling of advice:

(defvar --planner-appt-advice '()
  "Internal variable: List of advices added by `planner-appt-defadvice'.
Each element is a list of args for `ad-enable-advice' and
`ad-disable-advice'.")

(eval-and-compile
  (defvar planner-appt-advice-common-flags
    '(preactivate disable)
    "Advice flags common to all planner-appt advice."))

(defmacro planner-appt-defadvice (function args doc &rest body)
  "Advise FUNCTION with ARGS, DOC and BODY.
Remembers the advice function and args in `--planner-appt-advice'."
  `(prog1
       (defadvice ,function
	 (,@args ,@planner-appt-advice-common-flags) ,doc ,@body)
     (let ((info '(,function ,(car args) ,(cadr args))))
       (unless (member info --planner-appt-advice)
	 (push info --planner-appt-advice)))))

(put 'planner-appt-defadvice
     'edebug-form-spec
     '(&define name
	       (name name &rest sexp)
	       stringp
	       [&optional
		("interactive" interactive)]
	       def-body))

(put 'planner-appt-defadvice 'lisp-indent-function 'defun)

;; See what happened with the preactivation.
(planner-appt-debug
 (progn
   (require 'trace)
   (trace-function-background
    'ad-cache-id-verification-code
    "*planner-appt advice trace*")))

(defun planner-appt-disable-all-advice ()
  "Disable all advice added with `planner-appt-defadvice'."
  (mapcar #'(lambda (args)
	      (apply #'ad-disable-advice args)
	      (ad-activate (car args)))
	  --planner-appt-advice))

(defun planner-appt-enable-all-advice ()
  "Enable all advice added with `planner-appt-defadvice'."
  (mapcar #'(lambda (args)
	      (apply #'ad-enable-advice args)
	      (ad-activate (car args)))
	  --planner-appt-advice))


(defmacro with-planner-appt-task-advice-disabled (&rest body)
  "Evaluate BODY forms with all advice matching \"planner-appt-task\" disabled."
  `(unwind-protect
       (progn
	 (planner-appt-disable-all-advice)
	 (planner-appt-debug-message "all advice disabled")
	 ,@body)
     (planner-appt-enable-all-advice)
     (planner-appt-debug-message "all advice enabled")))

(put 'with-planner-appt-task-advice-disabled 'lisp-indent-function 0)
(put 'with-planner-appt-task-advice-disabled 'edebug-form-spec '(body))

;;; Showing Appointments In Various Ways

(defvar planner-appt-methods '()
  "Methods used for appointment alerts.
Internal variable: to set up appointment methods use one of:
  `planner-appt-use-tasks'
  `planner-appt-use-schedule'
  `planner-appt-use-tasks-and-schedule'.")

;; Copying task appts over to an "Appointments" section.

(defun planner-appt-format-appt-section-line (desc)
  "Format DESC as a line for the appointments section."
  (let* ((td (planner-appt-task-parse-task
	     ;; Trick the function into parsing:
	      (concat "@" desc)))
	 (text (car td))
	 (time (cadr td))
	 (end-time (if (string-match
			(format "\\s-*\\(%s\\)\\s-*"
				planner-appt-time-regexp)
			text)
		       (prog1
			   (match-string 1 text)
			 (setq text (replace-match "" t t text)))
		     "     ")))
    ;; Format in the style of a tabular schedule.
    (format "%6s | %5s | %s"
	    ;; Using an @ means the time gets fontified for free.
	    (concat "@" time)
	    end-time
	    (if (string= planner-appt-task-appointments-section
			 planner-appt-schedule-section)
		;; To avoid confusion, add an indication that this
		;; item came from a task.
		(concat "# " text)
	      text))))

(defvar --planner-appt-lines-added-to-section '()
  "Internal variable:
Remembers lines added by `planner-appt-update-appt-section' the last
time it was called.")

(defun planner-appt-task-schedule-item-p (string)
  "Return t if STRING is a schedule item derived from a task."
  (member string --planner-appt-lines-added-to-section))
;;   ;; Look for any property in the string since STRING will usually be
;;   ;; derived from a buffer substring which may have been edited.
;;   (text-property-any 0 (length string) 'appt-task t string))

(defun planner-appt-update-appt-section ()
    (save-excursion
    (planner-seek-to-first planner-appt-task-appointments-section)
    (let ((bound (make-marker))
	  (lines-to-delete
	   (copy-sequence --planner-appt-lines-added-to-section))
	  line)
      (save-excursion
	(planner-appt-seek-to-end-of-current-section)
	(set-marker bound (point)))
      (dolist (appt (append --planner-appt-tasks-added-appts
			    --planner-appt-tasks-earlier-appts))
	(setq line (funcall planner-appt-format-appt-section-line-function
			    (cadr appt)))
	(setq lines-to-delete (delete line lines-to-delete))
	(save-excursion
	  (unless (search-forward line bound t)
	    (insert line ?\n)))
	;; Remember the line even if it was already there
	(push line --planner-appt-lines-added-to-section))
      ;; Remove lines of deleted tasks
      (dolist (del-line lines-to-delete)
	(setq --planner-appt-lines-added-to-section
	      (delete del-line --planner-appt-lines-added-to-section))
	(save-excursion
	  (when (search-forward del-line bound t)
	    (replace-match "")
	    (when (eq (char-after) ?\n)
	      (delete-char 1)))))
      (set-marker bound nil))
    ;; Use schedule sorting with some changes
    (let ((planner-appt-schedule-section
	   planner-appt-task-appointments-section)
	  (planner-appt-schedule-regexp
	   (concat "\\(.*?\\)"		; to shift the match groups
		   planner-appt-schedule-basic-regexp)))
      (planner-appt-schedule-sort))))

(defun planner-appt-update-appt-section-maybe ()
      (when (and
	        ;; The appointment section is only relevant if the task
	        ;; method is used
	        (memq 'tasks planner-appt-methods)
		planner-appt-task-use-appointments-section-flag)
	(with-planner-update-setup
	 (save-excursion
	   (with-planner-appt-task-advice-disabled
	     (planner-goto-today))
	   (planner-appt-update-appt-section)))))

(defmacro with-planner-appt-update-section-disabled (&rest body)
 `(let ((planner-appt-task-use-appointments-section-flag nil))
    ,@body))

(put 'with-planner-appt-update-section-disabled 'lisp-indent-function 0)
(put 'with-planner-appt-update-section-disabled 'edebug-form-spec '(body))

;; Compatibility fix for Xemacs [and for Emacs <21?]
(if (fboundp 'fit-window-to-buffer)
    (defalias 'planner-fit-window-to-buffer 'fit-window-to-buffer)
  (defalias 'planner-fit-window-to-buffer 'shrink-window-if-larger-than-buffer))

;; Display Current Appointments
(defun planner-appt-show-alerts ()
  "Display a list of currently active alerts in another window."
  (interactive)
  (let ((buf (get-buffer-create planner-appt-alert-buffer)))
    (with-current-buffer buf
      (erase-buffer)
      (insert "Current alerts\n==============")
      (if appt-time-msg-list
	  (dolist (appt appt-time-msg-list)
	    (insert "\n" (cadr appt)))
	(insert "\nNone"))
      (goto-char (point-min)))
    (planner-fit-window-to-buffer (display-buffer buf))))


;; Display/Insert Forthcoming Appointments

(defvar planner-appt-forthcoming-regexp
  (concat "\\("
	  planner-appt-schedule-appt-regexp
	  "\\)\\|\\("
	  planner-live-task-regexp
	  planner-appt-task-regexp
	  "\\)"))

(defvar planner-appt-forthcoming-task-regexp
  (concat planner-live-task-regexp
	  planner-appt-task-regexp))

(defun planner-appt-forthcoming-format-appt-description (time description)
  (funcall
   planner-appt-format-appt-section-line-function
   (planner-appt-format-time-and-description
    time
    (planner-appt-format-description description))))

(defun planner-appt-forthcoming-task-data (info)
  (let ((task-appt
	 (planner-appt-task-parse-task
	  (planner-task-description info))))
    (when task-appt
      (cons (appt-convert-time (nth 1 task-appt))
	    (planner-appt-forthcoming-format-appt-description
	     (nth 1 task-appt)
	     (nth 0 task-appt))))))

(defun planner-appt-forthcoming-get-appts (n &optional include-today)
  (planner-save-buffers)
  (let ((appts '())
	(pages (planner-get-day-pages
		(if include-today
		    (planner-today)
		  (planner-calculate-date-from-day-offset
		   (planner-today) 1))
		(planner-calculate-date-from-day-offset
		 (planner-today) (if include-today n (1+ n)))))
	cyclic-data cyclic-task-descriptions
	line task-info task-data
	date-absolute date time text)
    ;; After scanning pages and [conditionally] cyclic entries, each
    ;; element of appts has:
    ;;
    ;; (<absolute date>
    ;;  <time in appt format [minutes from midnight]>
    ;;  <date in planner format>
    ;;  description text)
    ;;
    ;; The first two elements are used for sorting/merging; they are
    ;; removed from the returned list.
    (when (and (featurep 'planner-cyclic)
	       planner-appt-forthcoming-look-at-cyclic-flag)
      ;; Returns (<appts> . <list of planner-cyclic-ly formatted tasks>)
      (setq cyclic-data (planner-appt-forthcoming-get-cyclic n))
      (setq appts (car cyclic-data)
	    cyclic-task-descriptions (cdr cyclic-data)))
    (with-temp-buffer
      (with-planner
	(dolist (page pages)
	  (when (file-exists-p (cdr page))
	    (setq date (car page))
	    (setq date-absolute (calendar-absolute-from-gregorian
				 (planner-filename-to-calendar-date
				  date)))
	    (insert-file-contents (cdr page))
	    (goto-char (point-min))
	    (while (re-search-forward planner-appt-forthcoming-regexp nil t)
	      (setq line (match-string 0))
	      (if (string-match planner-appt-schedule-appt-regexp line)
		  (unless (planner-appt-task-schedule-item-p line)
		    (setq time (save-match-data
				 (appt-convert-time (match-string 1 line)))
			  text (match-string 0 line)))
		(setq task-info (planner-current-task-info))
		(setq task-data (planner-appt-forthcoming-task-data task-info))
		(when (and task-data
			   ;; Check for a cyclic task already added.
			   ;; This is a bit messy, since a task id
			   ;; won't have been added [and there might
			   ;; be other special case that I haven't
			   ;; anticipated].
			   (not (member
				 (if (string-match
				      "\\s-+{{Tasks:[0-9]+}}\\s-*"
				      (planner-task-description task-info))
				     (replace-match
				      "" nil t
				      (planner-task-description task-info))
				   (planner-task-description task-info))
				 cyclic-task-descriptions)))
		  (setq time (car task-data)
			text (cdr task-data))))
	      (when (and time text)
		;; Add if it is not there already [there may be a
		;; duplicate if this is a schedule item derived from a
		;; task item]
		(add-to-list 'appts (list date-absolute time date text))
		(setq time nil text nil)))
	    (erase-buffer)))))
    (when appts
      (mapcar #'cddr
	      (sort appts
		    #'(lambda (a b)
			(or (< (car a) (car b))
			    (and (= (car a) (car b))
				 (< (cadr a) (cadr b))))))))))


(defun planner-appt-forthcoming-get-cyclic (n)
  (let ((appts '())
	(cyclic-task-descriptions '())
	date line time text task-info task-data)
  (dolist (entry (planner-list-diary-entries
		  planner-cyclic-diary-file
		  (planner-filename-to-calendar-date
		   (planner-calculate-date-from-day-offset
		    (planner-today) 1))
		  (1- n)))
    (setq date (planner-date-to-filename (car entry))
	  line (cadr entry))
    (if (string-match planner-appt-schedule-appt-regexp line)
	(setq time (save-match-data
		     (appt-convert-time (match-string 1 line)))
	      text (match-string 0 line))
      (when (string-match planner-appt-forthcoming-task-regexp line)
	(setq task-info (planner-task-info-from-string date line))
	(setq task-data (planner-appt-forthcoming-task-data task-info))
	(when task-data
	  ;; For duplicate checking: remember the description as
	  ;; it would be transformed by planner-cyclic.
	  (push (format planner-cyclic-task-description-format
			(planner-task-description task-info) date)
		cyclic-task-descriptions)
	  (setq time (car task-data)
		text (cdr task-data)))))
    (when (and time text)
      (add-to-list
       'appts
       (list (calendar-absolute-from-gregorian (car entry))
	     time date text))
      (setq time nil text nil)))
  (cons appts cyclic-task-descriptions)))

;; Format the data into a big string to make it easy either to put
;; into a display buffer or into the day page.
(defun planner-appt-forthcoming-format (appt-data)
  (let* ((last-date "")
         (empty-cell-p (string-match
		       (format "\\`[%s]+\\'" muse-regexp-blank)
		       planner-appt-forthcoming-repeat-date-string))
         (calendar-abbrev-length
          ;; compatibility fix for Gnu Emacs < 22 and XEmacs
          (if (boundp 'calendar-abbrev-length)
              calendar-abbrev-length
            3))
         (day-padding (make-string (1+ calendar-abbrev-length) ?\ )))
    (mapconcat #'(lambda (a)
		   (prog1
		       (concat
			(if (string= (car a) last-date)
                            (concat
                             (when planner-appt-forthcoming-show-day-names-flag
                               day-padding)
                             (if empty-cell-p
                                 planner-appt-forthcoming-repeat-date-string
                               (planner-make-link
                                (car a)
                                planner-appt-forthcoming-repeat-date-string)))
                          (concat
                           (when planner-appt-forthcoming-show-day-names-flag
                             (concat
                              (calendar-day-name
                               (planner-filename-to-calendar-date (car a))
                               ;; compatibility:
                               calendar-abbrev-length)
                              " "))
                           (planner-make-link (car a))))
			" | "
			;; Remove @s from times to avoid spurious
			;; highlighting.
			(muse-replace-regexp-in-string
			 "@\\([ \t]*[0-9]\\)"
			 " \\1"
			 (cadr a)))
		     (setq last-date (car a))))
	       appt-data "\n")))


(defvar planner-appt-forthcoming-display-buffer
  "*Forthcoming Appointments*"
  "Buffer to display forthcoming appointments.")

(defun planner-appt-forthcoming-display (&optional days)
  (interactive
   ;; TODO: I wanted to use (interactive "p"), but that defaults to
   ;; 1.  Is this really the best way of getting nil as the default
   ;; for a command that takes an optional integer prefix?:
   (list (cond ((consp current-prefix-arg)
		(car current-prefix-arg))
	       ((integerp current-prefix-arg)
		current-prefix-arg)
	       (t nil))))
  (unless days (setq days planner-appt-forthcoming-days))
  (with-current-buffer
      (get-buffer-create planner-appt-forthcoming-display-buffer)
    (unless (planner-derived-mode-p 'planner-mode)
      (setq muse-current-project (muse-project planner-project))
      (planner-mode)
      (cd (planner-directory)))
    (delete-region (point-min) (point-max))
    (insert "* Appointments in the next "
	    (number-to-string days)
	    (if (= days 1) " day" " days")
	    "\n\n"
	    (planner-appt-forthcoming-format
	     (planner-appt-forthcoming-get-appts
	      (or days planner-appt-forthcoming-days) t)))
    (goto-char (point-min)))
  (display-buffer planner-appt-forthcoming-display-buffer)
  (planner-fit-window-to-buffer
   (get-buffer-window planner-appt-forthcoming-display-buffer)))


(defun planner-appt-forthcoming-update-section (&optional days)
  (interactive
   (list (cond ((consp current-prefix-arg)
		(car current-prefix-arg))
	       ((integerp current-prefix-arg)
		current-prefix-arg)
	       (t nil))))
  (with-planner-update-setup
   (save-excursion
     (planner-goto-today)
     (planner-seek-to-first planner-appt-forthcoming-appt-section)
     (delete-region (point)
		    (planner-appt-seek-to-end-of-current-section))
     (insert (planner-appt-forthcoming-format
	      (planner-appt-forthcoming-get-appts
	       (or days planner-appt-forthcoming-days)))
	     ?\n))))


;; A function suitable for planner-mode-hook
(defun planner-appt-forthcoming-update-section-maybe ()
  (when (planner-appt-todays-page-p)
    (planner-appt-forthcoming-update-section)))


;;; Alerts From Task Descriptions

;; Add a bit of [premature] optimization, mainly for updating
;; functions where the same task gets examined often.
(defvar --planner-appt-task-cache (make-hash-table :test 'equal))

(defun planner-appt-task-parse-task (description)
  "Extract appointment time and text from the DESCRIPTION.
Return a list (text time).  If the task is not an
appointment, time defaults to nil."
  (or (gethash description --planner-appt-task-cache)
      (puthash
       description
       (if (string-match planner-appt-task-regexp description)
	   (list (substring description (match-end 0))
		 (substring description (match-beginning 1) (match-end 1)))
	 (list description nil))
       --planner-appt-task-cache)))

(defun planner-appt-task-nagging-p (description)
  "Return non-nil if task DESCRIPTION is a nagging appointment."
  (string-match planner-appt-task-nagging-regexp description))

(defun planner-appt-task-member (description time appt-list)
  "Return non-nil if DESCRIPTION at TIME in APPT-LIST has been scheduled."
  (member (planner-appt-make-appt-element time description) appt-list))


;; Integration with planner-schedule.el

(defvar planner-appt-schedule-task-estimate-regexp
  (concat "[!@][ \t]*\\(?:"   ; "shy" form of planner-appt-task-regexp
	  planner-appt-time-regexp
	  "\\)[ \t]*"
	  "\\s-*\\([0-9]+[smhdw]\\)")
  "Regular expression matching a task time estimate.")

;; NB: The following advice could be avoided if the regexp were not
;; hard-coded into the original function.

;; NNB: This is not well tested!

(defadvice planner-schedule-task-estimate (around planner-appt-task disable)
  "Modify the regexp matched to take appointments into account."
  (when (string-match planner-appt-schedule-task-estimate-regexp
		      (planner-task-description info))
    (schedule-duration-to-seconds
     (match-string 2 (planner-task-description info)))))


(defun planner-appt-task-add (&optional info)
  "Create an appointment from the current task if this is today's plan.
Return t if an appointment was added.
If the task is an appointment, it is not cancelled, it is scheduled for
later today, and is not already added.
Optional argument: use INFO instead of the current task info."
  (interactive)
  (let* ((info (or info
		   ;; Unfortunately, in emacs-lisp there are no
		   ;; defaults for optional arguments, so one can't
		   ;; distinguish between null info in an arg and null
		   ;; info from planner-current-task-info; so the
		   ;; error message might be uninformative here.
		   (planner-current-task-info)
		   (error "There is no task on the current line")))
	 (appt (planner-appt-task-parse-task
		(planner-task-description info)))
	 (time (nth 1 appt))
	 (desc (and time (planner-appt-format-description (nth 0 appt)))))
    (when (and time
	       (not (string= (planner-task-status info) "C"))
	       (string= (planner-task-date info)
			(planner-date-to-filename
			 (decode-time (current-time))))
	       (not (planner-appt-task-member desc time
					      appt-time-msg-list)))
      (if (planner-appt-earlier-than-now-p time)
	  (progn
	    ;; Remember earlier appts separately [principally for
	    ;; their addition in an appointment section].
	    (unless (planner-appt-task-member
		     desc time --planner-appt-tasks-earlier-appts)
	      (setq --planner-appt-tasks-earlier-appts
		    (planner-appt-remember-appt
		     time desc
		     --planner-appt-tasks-earlier-appts)))
	    (planner-appt-update-appt-section-maybe)
	    ;; Make sure nil is returned.
	    nil)
	(appt-add time desc)
	;; Keep track of tasks added by this function.
	(setq --planner-appt-tasks-added-appts
	      (planner-appt-remember-appt
	       time desc
	       --planner-appt-tasks-added-appts))
	(planner-appt-update-appt-section-maybe)
	t))))

(defun planner-appt-task-delete (&optional info)
  "Delete the appointment from the current task if this is today's plan.
Do not remove the time string.
Return any deleted appointments.
Optional argument: use INFO instead of the current task info."
  (interactive)
  (let* ((info (or info
		   ;; See planner-appt-task-add for comments about
		   ;; the possibly uninformative error message.
		   (planner-current-task-info)
		   (error "There is no task on the current line")))
	 (appt (planner-appt-task-parse-task
		(planner-task-description info)))
	 (time (nth 1 appt))
	 (desc (and time (planner-appt-format-description (nth 0 appt))))
	 (tmp-msg-list appt-time-msg-list))
    (when time
      ;; Method from appt-delete
      (let ((deleted-appts '())
	    (earlier-appt
	     (car (planner-appt-task-member
		   desc time --planner-appt-tasks-earlier-appts)))
	    element)
	;; NB: Mustn't concat time onto description until earlier-appt
	;; has been determined [since planner-appt-task-member does
	;; the concat itself [this could be improved somehow]]
	(setq desc (concat time " " desc))
	(while tmp-msg-list
	  (setq element (car tmp-msg-list))
	  (when (string= (car (cdr element)) desc)
	    (push element deleted-appts)
	    (setq appt-time-msg-list (delq element appt-time-msg-list))
	    (setq --planner-appt-tasks-added-appts
		  (planner-appt-forget-appt
		   element --planner-appt-tasks-added-appts)))
	  (setq tmp-msg-list (cdr tmp-msg-list)))
	(when (or deleted-appts earlier-appt)
	  ;; Forget a deleted appt that was earlier than now.
	  (when earlier-appt
	    (setq --planner-appt-tasks-earlier-appts
		  (planner-appt-forget-appt
		   earlier-appt --planner-appt-tasks-earlier-appts)))
	  (planner-appt-update-appt-section-maybe))
	deleted-appts))))

(defun planner-appt-add-appts-from-tasks ()
  "Parse all of today's tasks and add appointments automatically."
  (interactive)
  (when (planner-appt-todays-page-p)
    ;; Clear old appts added by this function.
    (planner-appt-clear-appts --planner-appt-tasks-added-appts)
    (setq --planner-appt-tasks-added-appts '()
	  --planner-appt-tasks-earlier-appts '())
    (let ((case-fold-search nil))
      (save-excursion
	(goto-char (point-min))
	(with-planner-appt-update-section-disabled
	 (while (re-search-forward
		 (concat planner-live-task-regexp
			 planner-appt-task-regexp)
		 nil t)
	   (planner-appt-task-add))))
      (when (or --planner-appt-tasks-added-appts
		--planner-appt-tasks-earlier-appts)
	(planner-appt-update-appt-section-maybe)))))

(planner-appt-defadvice planner-task-cancelled
  (before planner-appt-task)
  "Delete the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-task-cancelled")
  (planner-appt-task-delete))

(planner-appt-defadvice planner-task-done
  (before planner-appt-task)
  "Delete the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-task-done")
  (planner-appt-task-delete))

(planner-appt-defadvice planner-delete-task
  (before planner-appt-task)
  "Delete the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-delete-task")
  (planner-appt-task-delete))

;; The advice for planner-update-task is quite tricky.  A task will
;; need updating [for appointments] if the task is dated today and the
;; description differs from other task lines linked to by the current
;; task.  If this is true, we have to examine all the other links,
;; delete any appointments, and then add the task after planner-update
;; has been called.  Note that it is only possible for this to happen
;; if planner-id is loaded since otherwise the "same" task line can't
;; have different descriptions.

(defun planner-appt-get-diff-links (info)
  "Given INFO, return a list of tasks linked to it whose info differs."
  (let ((diffs '())
	(linked-info))
    ;; Todo: with-planner-update-setup really ought to return the
    ;; value of the body.
    (with-planner-update-setup
      ;; Preserve point as well.
      (save-excursion
	(dolist (link
		 (if (featurep 'planner-multi)
		     (planner-multi-link-delete
		      (planner-task-page info)
		      (planner-multi-task-link-as-list info))
		   (list (planner-task-link info))))
	  (when (and (planner-find-file (planner-link-base link))
		     (planner-find-task info)
		     (setq linked-info (planner-current-task-info))
		     (not (planner-tasks-equal-p info linked-info)))
	    (push linked-info diffs)))))
    diffs))

(planner-appt-defadvice planner-update-task
  (around planner-appt-task)
  "Update the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-update-task")
  (let* ((info (planner-current-task-info))
	 (diff-linked
	  (and (featurep 'planner-id)
	       (string= (planner-task-date info) (planner-today))
	       (planner-appt-get-diff-links info))))
    (with-planner-appt-task-advice-disabled ad-do-it)
    (when diff-linked
      (dolist (i diff-linked)
	(planner-appt-task-delete i))
      (planner-appt-task-add))))

;; For planner-id-update-tasks-on-page, it is actually much faster to
;; update today's page after it has done its work rather than using
;; the update advice above.

(planner-appt-defadvice planner-id-update-tasks-on-page
  (around planner-appt-task)
  "Update today's appointments as well."
  (planner-appt-debug-message
   "*** called advice on planner-id-update-tasks-on-page")
  (with-planner-appt-task-advice-disabled ad-do-it)
  (with-planner-update-setup
    (save-excursion
      (planner-goto-today)
      ;; Update the appointments section afterwards for efficiency.
      (with-planner-appt-update-section-disabled
	(planner-appt-update))
      (planner-appt-update-appt-section-maybe))))

(defvar --planner-appt-planning nil
"Internal flag:
Lets planner-appt advice know that it has been called within a call to
`plan'.")

(planner-appt-defadvice plan (around planner-appt-task)
  "Note that plan is in progress."
  (planner-appt-debug-message
   "*** called advice on plan")
  (let ((--planner-appt-planning t))
	  ad-do-it))

(planner-appt-defadvice planner-copy-or-move-task
  (around planner-appt-task)
  "Update the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-copy-or-move-task; "
   (if --planner-appt-planning
       "in plan"
     "not in plan"))
  (cond ((not --planner-appt-planning)
	 (let
	     ;; Save the appt information for error cleanup and
	     ;; appointment adding.
	     ((deleted-appts (planner-appt-task-delete))
	      (old-info (planner-current-task-info)))
	   (condition-case err
	       (progn
		 (with-planner-appt-task-advice-disabled ad-do-it)
		 ;; If the task was moved to today, add it.
		 (when (and date ; Bound in the advised function.
			    (string= date (planner-today)))
		   ;; Fiddle the old info so it looks like
		   ;; today's.  NB: would need changing should
		   ;; the task-info format ever change.
		   ;;
		   ;; planner-multi uses the date in the link for
		   ;; planner-task-date [why?], so that has to be
		   ;; modified too. This has to be done before the
		   ;; date element is changed, of course.
		   (when (and (featurep 'planner-multi)
			      (consp (nth 5 old-info))) ; the link element
		     (setcar (or
			      (member (nth 8 old-info)
				     (nth 5 old-info))
			      ;; Silly way of avoiding an error if the
			      ;; date is not in the list: can the date
			      ;; not be in the list?
			      '(nil))
			     date))
		   ;; Set the date element.
		   (setcar (nthcdr 8 old-info) date)
		   (planner-appt-task-add old-info)))
	     ('error
	      ;; Catch errors in planner-copy-or-move-task: restore
	      ;; deleted tasks.
	      (dolist (d deleted-appts)
		(push d appt-time-msg-list))
	      (error (error-message-string err))))))
	(t
	 ;; `plan' in progress: only move the task if it is not a
	 ;; regular (non-nagging) appointment.  If it's a nagging
	 ;; appointment, remove the appointment and then move the
	 ;; task.
	 (let* ((info (planner-current-task-info))
		(appt (planner-appt-task-parse-task
		       (planner-task-description info)))
		(time (nth 1 appt)))
	   (with-planner-appt-task-advice-disabled
	     (if (planner-appt-task-nagging-p (planner-task-description info))
		 (progn (planner-edit-task-description (nth 0 appt))
			ad-do-it)
	       (if (and info time)
		   ;; Fri Nov 25 13:59:02 GMT 2005: Fix for new [?]
		   ;; behaviour of planner-copy-or-move-region.
		   (progn
		     (forward-line -1)
		     ;; Return nil, otherwise
		     ;; planner-copy-or-move-region's count will be
		     ;; wrong.
		     (setq ad-return-value nil))
		 ad-do-it)))))))

;; NB: this advice leads to two updates of the task appointment
;; section [when updating it is enabled, of course]: it is hard to see
;; how to avoid this unless there is yet another global variable
;; tracking deleted appts.
(planner-appt-defadvice planner-edit-task-description
  (around planner-appt-task)
  "Update the appointment as well."
  (planner-appt-debug-message
   "*** called advice on planner-edit-task-description")
  (planner-appt-task-delete)
  (with-planner-appt-task-advice-disabled ad-do-it)
  (planner-appt-task-add))

;; planner-create-task-from-info: The appointment adding needs doing
;; after all hooks to planner-create-task-from-info have been run so
;; that planner-appt-task-add has the correct task line; planner-id,
;; for example, adds a task id so if the planner-appt hook is run
;; first it won't have the right task description.
;;
;; Hence these shenanigans:

(defvar --planner-appt-created-task-marker (make-marker))
(defvar --planner-appt-close-the-buffer-flag nil)

(eval-when-compile
  ;; Hide warning about live-buffers [q.v.].
  (defvar live-buffers))

(defun planner-appt-create-task-hook-func ()
  "One half of planner-appt create task handling.
Remembers the position of the added task. The other half of the
handling is in advice to `planner-create-task-from-info'."
  (set-marker --planner-appt-created-task-marker (point))
  ;; If planner-tasks-file-behavior is 'close, it won't be possible to
  ;; recover the position from a marker, so temporarily defeat closing
  ;; this file, and close it if necessary in the
  ;; planner-create-task-from-info advice.
  (setq --planner-appt-close-the-buffer-flag
	(if (and (eq planner-tasks-file-behavior 'close)
		 ;; `live-buffers' is defined in
		 ;; planner-create-task-from-info, but it is not
		 ;; defined in the planner-multi advice [hmm...].
		 (boundp 'live-buffers)
		 (not (memq (current-buffer) live-buffers)))
	    ;; Adding the buffer to `live-buffers' means it won't be
	    ;; closed automatically.
	    (progn (push (current-buffer) live-buffers) t)
	  nil)))


(planner-appt-defadvice planner-create-task-from-info
  (around planner-appt-task)
  "Add an appointment alert for the new task if necessary."
  (planner-appt-debug-message
   "*** called advice on planner-create-task-from-info")
  (with-planner-appt-task-advice-disabled ad-do-it)
  (let ((buf (marker-buffer --planner-appt-created-task-marker))
	(pos (marker-position --planner-appt-created-task-marker)))
    (when buf
      (with-current-buffer buf
	(save-excursion
	  (goto-char pos)
	  (planner-appt-task-add)))
      (set-marker --planner-appt-created-task-marker nil)
      (when --planner-appt-close-the-buffer-flag
	;; Use planner-save-buffers for consistency; remove the buffer
	;; from buffer-list so that it gets closed.
	(planner-save-buffers (delq buf (buffer-list)))))))

(defun planner-appt-task-insinuate ()
  "Do task-specific insinuation into `planner-mode'."
  ;; Nothing at the moment!
  )

;; In case of breakage while developing
;; NB: remember to remove hooks locally where relevant
(defun planner-appt-task-de-insinuate ()
  "Remove task-specific hooks."
  ;; Nothing at the moment!
  )


;;; Alerts From The Schedule

(defvar --planner-appt-schedule-added-appts '()
  "Internal variable: Tracks added schedule-based appointment alerts.")

(defun planner-appt-add-appts-from-schedule ()
  "Add appointment reminders from the schedule if this is today's plan."
  (interactive)
  (when (planner-appt-todays-page-p)
    ;; Delete old appts created by this function.
    (planner-appt-clear-appts --planner-appt-schedule-added-appts)
    (setq --planner-appt-schedule-added-appts '())
    (save-excursion
      (planner-seek-to-first planner-appt-schedule-section)
      (let ((bound (save-excursion
		     (planner-appt-seek-to-end-of-current-section)))
	    line time text task-item-p)
	;; There are no entries in the schedule unless bound is
	;; greater than point.
	(when (> bound (point))
	  (while (re-search-forward
		  planner-appt-schedule-appt-regexp bound t)
	    (setq line (planner-match-string-no-properties 0)
		  time (planner-match-string-no-properties 1)
		  text (save-match-data
			 (planner-appt-format-description
			  (planner-match-string-no-properties 3)))
		  task-item-p
		  (planner-appt-task-schedule-item-p (match-string 0)))
	    (unless (or task-item-p
			(planner-appt-earlier-than-now-p time))
	      (appt-add time text)
	      ;; Remember tasks added here.
	      (setq --planner-appt-schedule-added-appts
		    (planner-appt-remember-appt
		     time text --planner-appt-schedule-added-appts)))))))))

(defun planner-appt-schedule-insinuate ()
  "Do schedule specific insinuation into `planner-mode'."
  ;; Nothing at the moment!
  )

(defun planner-appt-schedule-de-insinuate ()
  "Remove schedule-based hooks."
  ;; Nothing at the moment!
  )

;; Make appt-make-list behave.

(defadvice appt-make-list (after planner-appt activate)
  "Restore appointments added by planner-appt."
  (dolist (appt (append --planner-appt-tasks-added-appts
			--planner-appt-schedule-added-appts))
    (unless (member appt appt-time-msg-list)
      (push appt appt-time-msg-list)))
  (setq appt-time-msg-list (appt-sort-list appt-time-msg-list)))

;;; Sorting the Schedule

(defun planner-appt-schedule-sort ()
  "Sort the schedule in the current page."
  (interactive)
  (save-excursion
    (save-restriction
      (planner-seek-to-first planner-appt-schedule-section)
      (narrow-to-region (point)
			(save-excursion
			  (planner-appt-seek-to-end-of-current-section)
			  (point)))
      (sort-subr nil 'forward-line 'end-of-line
		 #'(lambda ()
		     (goto-char (planner-line-beginning-position))
		     (if (looking-at planner-appt-schedule-regexp)
			 (appt-convert-time
			  (match-string 2))
		       ;; 1+ max number of minutes from midnight
		       1441))
		 nil))))

;;; Cyclical Schedule Entries

(require 'diary-lib)

(eval-when-compile
  (defvar planner-cyclic-diary-file))

;; Purloined from planner-cyclic.el.
(defun planner-appt-schedule-get-cyclic-tasks (date &optional no-of-days)
  "For DATE, get the cyclic tasks.
Optional argument get tasks for NO-OF-DAYS from DATE, the default is 1
day [i.e., only for DATE]."
  (let ((date (if (stringp date)
		  (planner-filename-to-calendar-date date)
		date)))
    (delq nil
	  (mapcar #'(lambda (item)
		      (when (string-match planner-appt-schedule-regexp
					  (elt item 1))
			(match-string 0 (elt item 1))))
		  (planner-list-diary-entries planner-cyclic-diary-file
					      date no-of-days)))))

(defun planner-appt-schedule-add-cyclic ()
  "Add cylical tasks to the schedule if the current buffer is a day page."
  (when (string-match planner-date-regexp (planner-page-name))
    (let ((entries
	   (planner-appt-schedule-get-cyclic-tasks (planner-page-name))))
      (when entries
	(planner-seek-to-first planner-appt-schedule-section)
	(let ((start (point)))
	  (dolist (entry entries)
	    ;; Only insert if the entry is not already there.
	    (unless (save-excursion
		      (goto-char start)
		      (search-forward entry nil t))
	      (insert entry ?\n))))
	;; Lazy way of putting them in the right place.
	(when planner-appt-sort-schedule-on-update-flag
	  (planner-appt-schedule-sort))))))

(defun planner-appt-schedule-add-cyclic-maybe ()
  "Add cylical tasks to the schedule.
Behaviour depends upon `planner-appt-schedule-cyclic-behaviour'."
  (when (and (not (string< (planner-page-name) (planner-today)))
	     (or (eq planner-appt-schedule-cyclic-behaviour 'future)
		 (and
		  (eq planner-appt-schedule-cyclic-behaviour 'today)
		  (planner-appt-todays-page-p))))
    (planner-appt-schedule-add-cyclic)))

(defun planner-appt-schedule-cyclic-insinuate ()
  "Insinuate the adding of cyclical schedule entries."
  ;; TODO: Add locally?
  (planner-appt-add-hook 'planner-mode-hook
			 'planner-appt-schedule-add-cyclic-maybe nil t))

(defun planner-appt-schedule-cyclic-de-insinuate ()
  "Remove cyclic schedule adding functionality."
  (remove-hook 'planner-mode-hook
	       'planner-appt-schedule-add-cyclic-maybe))


;;; Common Functions


;; So that one doesn't have to use two separate commands when using
;; both methods:

(defvar --planner-appt-updated nil)

;;;###autoload
(defun planner-appt-update ()
  "Update the appointments on the current page."
  (interactive)
  ;; NB: Task adding has to be done before the schedule to avoid
  ;; duplicates if task appointments are copied to the schedule.
  (when (memq 'tasks planner-appt-methods)
    (planner-appt-add-appts-from-tasks))
  (when (memq 'schedule planner-appt-methods)
    (planner-appt-add-appts-from-schedule))
  (when (and planner-appt-sort-schedule-on-update-flag
	     (planner-appt-todays-page-p))
    (planner-appt-schedule-sort))
  (run-hooks 'planner-appt-update-hook)
  ;; TODO: Use a belt and some braces: see comments in
  ;; `planner-appt-insinuate-if-today'.
  (setq --planner-appt-updated t))

(defun planner-appt-update-for-write ()
  (when planner-appt-update-appts-on-save-flag
    (planner-appt-update)
    ;; Return nil for local-write-file-hooks.
    nil))

;; NB: Something like the above could be done for other hooks as
;; well...


;;; General Insinuation

;; This indirect method is used rather than some variable so that the
;; function advice can be avoided if it is not necessary [when using
;; schedule appointments exclusively].


(defun planner-appt-use (source)
  "Use SOURCE to derive appointments from plan pages.
Possible values for SOURCE are:
  'tasks            [use today's task descriptions]
  'schedule         [use today's schedule]
  '(tasks schedule) [use both tasks and schedule]."
  (dolist (s (if (listp source)
		 source
	       (list source)))
    (cond ((eq s 'tasks)
 	   ;; Add task-specific non-insinuating code here.
;; 	   ;; Only activate advice when necessary.
;; 	   (ad-enable-regexp "planner-appt-task")
;; 	   (ad-activate-regexp "planner-appt-task")
	   ;; An outstanding global hook [planner-appt-task-add tests
	   ;; for today-ness anyway].
	   (planner-appt-add-hook 'planner-create-task-hook
				  'planner-appt-create-task-hook-func nil t)
	   (add-to-list 'planner-appt-methods s))
	  ((eq s 'schedule)
	   ;; Add schedule task-specific non-insinuating code here.
	   (add-to-list 'planner-appt-methods s))
	  (t
	   (error "Invalid appointment source %s" s))))
  ;; Add here any non method-specific code that should be executed
  ;; even if planner-appt-insinuate is not called.
  (if (memq 'tasks planner-appt-methods)
      (planner-appt-enable-all-advice)
    (planner-appt-disable-all-advice))
  (planner-appt-add-hook 'planner-mode-hook
			 'planner-appt-font-setup nil t)
  ;; Might as well return something interesting.
  planner-appt-methods)

;;;###autoload
(defun planner-appt-insinuate-if-today ()
  (when (planner-appt-todays-page-p)
    (planner-appt-add-hook 'planner-mode-hook
			   'planner-appt-update t)
    ;; Add method specific things
    (when (memq 'tasks planner-appt-methods)
      (planner-appt-task-insinuate))
    (when (memq 'schedule planner-appt-methods)
      (planner-appt-schedule-insinuate))
    (planner-appt-add-hook planner-appt-write-file-hook
			   'planner-appt-update-for-write t)
    ;; TODO: Under some conditions, as yet undetermined, an update is
    ;; not done when `plan' is called for the first time.  So do an
    ;; update, but only if there hasn't been one already.
    ;;
    ;; Fri 15 Apr 2005 16:26:04 BST: this has probably been sorted out
    ;; now, but it doesn't do any harm to leave this in for now, just
    ;; in case.
    (unless --planner-appt-updated
      (planner-appt-update))))

;;;###autoload
(defun planner-appt-insinuate ()
  "Insinuate appointment alerting into planner mode.
Appointment methods should be set up first using one of:
  `planner-appt-use-tasks'
  `planner-appt-use-schedule'
  `planner-appt-use-tasks-and-schedule'."
  (unless planner-appt-methods
    (error
     "No appointment source methods.  Use one of:
  `planner-appt-use-tasks'
  `planner-appt-use-schedule'
  `planner-appt-use-tasks-and-schedule'
before you call this function"))
  ;; Initialize the appt system according to emacs type and version.
  (cond ((fboundp 'appt-activate)	; Gnu Emacs >= 22
	 (appt-activate 1))
	((fboundp 'appt-initialize)	; Xemacs
	 ;; appt-initialize needs a diary file, so create one if it
	 ;; doesn't already exist
	 (unless (file-exists-p diary-file)
	   (with-temp-buffer
	     (insert ?\n)
	     (write-region (point-min) (point-max) diary-file)))
	 (display-time)
	 (appt-initialize))
	(t (display-time)		; Gnu Emacs < 22
	   (add-hook 'diary-hook 'appt-make-list)))
  (planner-appt-add-hook 'planner-mode-hook
			 'planner-appt-insinuate-if-today t t))

;; Remove hooks if there is something not working during testing.
(defun planner-appt-de-insinuate ()
  "Remove all hooks associated with planner-appt.
Use in an emergency if breakage in planner-appt interferes with your planning."
  (interactive)
  (remove-hook 'planner-mode-hook 'planner-appt-insinuate-if-today)
  (remove-hook 'mark-diary-entries-hook 'planner-appt-mark-calendar-maybe)
  (when (get-buffer (planner-today))
    (with-current-buffer (get-buffer (planner-today))
      ;; NB: Remember to remove locally where appropriate.
      (remove-hook 'planner-goto-hook
		   'planner-appt-update t)
      (remove-hook 'planner-mode-hook
		   'planner-appt-update t)
      (remove-hook planner-appt-write-file-hook
		   'planner-appt-update-for-write t)
      (remove-hook 'planner-create-task-hook
		   'planner-appt-create-task-hook-func)
      (planner-appt-task-de-insinuate)
      (planner-appt-schedule-de-insinuate)
      (planner-appt-schedule-cyclic-de-insinuate)))
  (planner-appt-disable-all-advice)
  (message "Planner-appt de-insinuated."))


;;; Convenient Functions For Users' Configuration.

;;;###autoload
(defun planner-appt-use-tasks ()
  "Use tasks to derive appointment alerts."
  (planner-appt-use 'tasks))

;;;###autoload
(defun planner-appt-use-schedule ()
  "Use the schedule to derive appointment alerts."
  (planner-appt-use 'schedule))

;;;###autoload
(defun planner-appt-use-tasks-and-schedule ()
  "Use both tasks and the schedule to derive appointment alerts."
  (planner-appt-use '(tasks schedule)))

;;; Font Highlighting

(defface planner-appt-face '((t (:foreground "green")))
  "Face for scheduled time."
  :group 'planner-appt)

(defface planner-appt-overdue-face '((t (:foreground "red")))
  "Face for scheduled, but overdue time."
  :group 'planner-appt)

(defun planner-appt-task-highlight-face (time)
  "Return appropriate face, depending on TIME.
If TIME is earlier than now, return `planner-appt-face', else
return `planner-appt-overdue-face'."
  (if (planner-appt-earlier-than-now-p time)
      'planner-appt-overdue-face
    'planner-appt-face))

(defun planner-appt-task-highlight (beg end &optional verbose)
  "Highlight appointment times in tasks from BEG to END.
VERBOSE is ignored."
  (when planner-appt-font-lock-appointments-flag
    (goto-char beg)
    (while (re-search-forward planner-appt-task-regexp end t)
      (when (or (not planner-appt-limit-highlighting-flag)
		(save-excursion
		  (goto-char (planner-line-beginning-position))
		  (save-match-data
		    (or (and (looking-at planner-live-task-regexp)
			     (string= (planner-task-date
				       (planner-current-task-info))
				      (planner-today)))
			(and
			 (planner-appt-todays-page-p)
			 (looking-at planner-appt-schedule-appt-regexp))))))
	(planner-highlight-region
	 (match-beginning 1)
	 (match-end 1)
	 'planner-appt 60
	 (list 'face
	       (planner-appt-task-highlight-face
		(match-string 1))))))))

(defun planner-appt-font-setup ()
  "Hook into `planner-mode'."
    (planner-appt-add-hook 'muse-colors-buffer-hook
			   'planner-appt-task-highlight t))

;;; Calendar Marking

;;  This is not strictly part of appointment handling, but if the
;;  diary is to be by-passed for appointments, it makes sense to mark
;;  the calendar using day pages.


;; Use a different face from the diary-entry-marker so we can
;; see where the different marks come from.
(defface planner-appt-entry-marker
  '((t (:foreground "indianred")))
  "Face for planner day page mark in the calendar."
  :group 'planner-appt)


(defun planner-appt-mark-calendar (&optional from to)
  "Mark dates in the calendar that have day pages.
Optional args: mark dates from FROM to TO.
FROM and to are lists: (month day year)."
  (with-current-buffer calendar-buffer	; Needed to get displayed date
					; information.
    (let* ((displayed-month-last
	    (1+ (cdr (assq 'displayed-month
			   (buffer-local-variables)))))
	   (displayed-year
	    (cdr (assq 'displayed-year
		       (buffer-local-variables))))
	   (today-filename (planner-today))
	   (today (planner-filename-to-calendar-date today-filename)))
      ;; Do nothing if the calendar is currently showing all months
      ;; earlier than today.
      (when (and (>= (elt today 2)	; year
		     displayed-year)
		 (>= displayed-month-last
		     (elt today 0)))	; month
	(let ((diary-entry-marker 'planner-appt-entry-marker)
	      (day-pages
	       (planner-get-day-pages
		(if from
		    (planner-date-to-filename from)
		  today-filename)
		(if to
		    (planner-date-to-filename to)
		  ;; The default is last day visible
		  ;; in the calendar.
		  (planner-date-to-filename
		   (list displayed-month-last 31 displayed-year))))))
	  (dolist (day-page day-pages)
	    (apply #'mark-calendar-date-pattern
		   (planner-filename-to-calendar-date (car day-page)))))))))

(defun planner-appt-calendar-insinuate ()
  (add-hook 'mark-diary-entries-hook 'planner-appt-mark-calendar))

(provide 'planner-appt)
;;; planner-appt.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
