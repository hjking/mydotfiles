;; @(#) sams-lib.el -- Library for book: Sams Teach Yourself Emacs in 24 Hours
;; @(#) $Id: sams-lib.el,v 1.1 2001/02/01 20:15:58 lasse Exp $

;; This file is not part of Emacs

;;{{{ Id

;; Copyright (C) Jesper Pedersen, Jari Aalto, Charles Curley
;; Author:       Jesper Pedersen <blackie@ifad.dk>
;; Maintainer:   Jesper Pedersen <blackie@ifad.dk>
;; Created:      1999-01-31
;; Keywords:     extensions
;;
;; Look at the code with folding.el

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation; either version 2 of the License, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
;; for more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with GNU Emacs.  If you did not, write to the Free Software Foundation,
;; Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;}}}
;;{{{ Install

;; Put this file on your Emacs-Lisp load path, add following into your
;; ~/.emacs startup file.
;;
;;     (require 'sams-lib)

;;}}}

;;{{{ Documentation

;;; Commentary:

;;  This file contains functions that are used in the
;;  Sams Teach Yourself Emacs in 24 Hours, ISBN 0-672-31594-7

;;}}}

;;; Change Log:
;;; Code:

(message "Loading sams-lib...")

(require 'cl)

(eval-when-compile			;use dynamic byte compilation
  (make-local-variable 'byte-compile-dynamic)
  (defconst byte-compile-dynamic t)
  (require 'advice)
  (autoload 'reporter-submit-bug-report "reporter")
  (autoload 'bbdb-complete-name         "bbdb" "" t)
  )

(defvar sams-lib-load-hook nil
  "Hook that is run when package is loaded.")

(defvar sams-lib-version (substring "$Revision: 1.1 $" 11 15)
  "Version number.")

;;; ######################################################### &tinylib ###
;;; some functions from lisp library kit by Jari Aalto
;;; these functions are available at ftp://cs.uta.fi/pub/ssjaaa/ema-tiny.html

(eval-and-compile
  (when (not (featurep 'tinylibm))

  (defsubst xemacs-p ()
    "Check if running XEmacs. Return main revision as integer (eg. 19)."
    (if (or (boundp 'xemacs-logo)
	    (featurep 'xemacs))		;Appeared in 20.2+
	emacs-major-version))


  (defsubst xemacs-pp  ()
    "If XEmacs 20.x+, return `emacs-minor-version'"
    (and (or (featurep 'xemacs)		;Introduced in 20.x
	     (and (boundp 'xemacs-logo)
		  (> emacs-major-version 19)))
	 emacs-minor-version))


  (defsubst emacs-p ()
    "Check if running Emacs."
    (if (not (boundp 'xemacs-logo))
	emacs-major-version))


  (defsubst emacs-pp ()
    "If Emacs 20.x+, return `emacs-minor-version'."
     (and (not (boundp 'xemacs-logo))
	  (> emacs-major-version 19)
	  emacs-minor-version))


  (defsubst emacs-version-number-as-string ()
    "Emacs and XEmacs compatibility. Return plain version number string."
    (if (emacs-p)
	emacs-version			; "19.34"
      ;; XEmacs return "19.14 XEmacs Lucid", get only version
      (and (string-match "^\\([0-9]+\\.[0-9.]+\\)" emacs-version)
	   (substring emacs-version 0 (match-end 1)) )))


  (defsubst win32-p ()
    (cond
     ((fboundp 'console-type)
      (memq (let ((f 'console-type)) (funcall f)) ;Quiet Emacs byte compiler
	  '(win32 w32)))
     ((boundp 'window-system)
      (memq (symbol-value 'window-system) '(win32 w32)))
     ((error "Internal alert, contact maintainer of TinyLib."))))


  (defsubst win32-nt-p ()
    (and (win32-p)
	 (string-match "windows.*NT"  (or (getenv "OS") "" ))))


  (defun win32-cygwin-p (&optional use-cache)
    "Return cygwin version string, like 'B19' from `exec-path'.
If USE-CACHE is non-nil, retrieve cached value."
    (let (ret)
      (cond
       ((and use-cache
	     (get 'win32-cygwin-p 'cache-set))
	(get 'win32-cygwin-p 'cache-value))
       (t
	(put 'win32-cygwin-p 'cache-set t)
	(dolist (path exec-path)
	  ;; "E:/USR/LOCAL/CYGNUS/B19/H-I386-CYGWIN32/BIN"
	  (when (string-match "CYGNUS[/\\]\\([^/\\]+\\)[/\\].*cygwin32" path)
	    (setq ret (match-string 1 path))
	    (put 'win32-cygwin-p 'cache-value ret)
	    (return)))))
      ret))

  ))

(eval-and-compile
(unless (fboundp 'region-active-p)	;[jari]
  (defun region-active-p  ()
    "Return t if mark (region) is active."
    (cond
     ((and (not (emacs-p))
	   (boundp 'zmacs-regions))        ;XEmacs
      (let* ((zmacs-regions t))
	(mark)))
     (t
      (symbol-value 'mark-active)          ;Emacs
      )))))


(defmacro sams-definteractive (&rest body) ;[jari]
  "Define simple anonymous interactive function.
Function can take one optional argument 'arg'.
Very usefull place where you can use this function is when you
want to define simple key functions

 (global-set-key
   \"\\C-cc\"
   (definteractive (message \"You gave arg: %s\" (prefix-arg-to-text arg))))"
  (` (function (lambda (&optional arg) (interactive "P") (,@ body)))))


;;; #################################################### &emacs-checks ###

(defconst sams-Emacs-20-p (= emacs-major-version 20)
  "This predicate is true if your current emacs major version is 20")

(defconst sams-Emacs-19-p (= emacs-major-version 19)
  "This predicate is true if your current emacs major version is 19")

(defconst sams-UNIX-p
  (eval-and-compile
    (let* ((xemacs-func 'console-type)
	   (type (if (fboundp xemacs-func)
		     (funcall xemacs-func)
		   (symbol-value 'window-system))))
      (if (memq type '(nil x))
	  t)))
  "This predicate is true if emacs is running in UNIX")

(defconst sams-Windows-p (win32-p)
  "This predicate is true if running win32 environment.")

(defconst sams-Gnu-Emacs-p (emacs-p)
  "This preficate is true if your current emacs version is Gnu Emacs")

(defconst sams-XEmacs-p (xemacs-p)
  "This preficate is true if your current emacs version is XEmacs")

;;; ########################################################## &window ###

;; [Jesper]
;; These function are just like other-window and other-frame, except
;; that they jump in the opposite direction.

(defun sams-other-window-backwards (arg) ;[Jesper]
  "Like `other-window', but moves in the opposite direction."
  (interactive "p")
  (other-window (- 0 arg)))

(defun sams-other-frame-backwards (arg)	;[Jesper]
  "Like `other-frame', but moves in the opposite direction."
  (interactive "p")
  (other-frame (- 0 arg)))

(defun sams-toggle-truncate ()		;[Jesper]
  "Toggle whether lines should be truncated or not.
Easy way to shift between truncate mode for a single buffer."
  (interactive "")
  (make-local-variable 'truncate-lines)
  (make-local-variable 'truncate-partial-width-windows)
  (if (< (window-width) (frame-width))
      (progn
        (setq truncate-lines nil)
        (setq truncate-partial-width-windows
	      (not truncate-partial-width-windows)))
    (setq truncate-lines (not truncate-lines))))


;;; ########################################################### &frame ###

(defun sams-find-file-dedicated-frame (name) ;[Jesper]
  "Load a file into a dedicated frame.
Create dedicated frame from file name."
  (interactive "ffind file: ")
  (special-display-popup-frame (find-file-noselect name)))

(defun sams-switch-buffer-dedicated-frame (name) ;[Jesper]
  "This functions loads an existing buffer into a dedicated frame"
  (interactive "bbuffer name: ")
  (special-display-popup-frame name))


;;; ################################################## &xemacs-menubar ###
;;; by [Jesper]

(defvar sams-xemacs-menu-found nil
  "This variable contains the menu items found")

(defun sams-where-is (definition)
  "Alternative to the `where-is'.
`where-is' doesn't look for definitions in the XEmacs menu bar.
This function is designed to work both with Gnu Emacs and XEmacs."
  (interactive
   (let ((question
          (if (xemacs-p)
              "Where is command or variable"
            "Where is command"))
         (fn (function-called-at-point))
         (enable-recursive-minibuffers t)
         val)
     (setq val (completing-read (if fn
				    (format "%s (default %s): " question fn)
				  (concat question ": "))
				obarray
                                (if (xemacs-p)
                                    (lambda (x) t)
                                  'fboundp)
                                t))
     (list (if (equal val "")
                   fn (intern val)))))

  ;; ............................................................ body ...

  (if (xemacs-p)
      ;;; XEmacs
      (let* ((function 'sorted-key-descriptions) ;just Bytecomp silencer
	     (keyboard-location (funcall function (where-is-internal definition)))
	     (menu-location (sams-xemacs-where-is-menubar definition))
	     (all (if (string= keyboard-location "")
		      menu-location
		    (if (string= menu-location "")
			keyboard-location
		      (concat keyboard-location ", " menu-location))))
	     )
        (if (equal all "")
            (message "%s is not on any key" definition)
          (message "%s is on %s" definition all)))

    ;;; Gnu Emacs works fine
    (where-is definition)))

(defun sams-xemacs-where-is-menubar (name)
  "This function searches for a function in the XEmacs menu bar"
  (setq sams-xemacs-menu-found nil)
  (sams-xs-top-menu name)
  (mapconcat #'(lambda (x) x) sams-xemacs-menu-found ", "))

(defun sams-xs-top-menu (name)
  ""
  (let ((menu-list (symbol-value 'current-menubar))
        (menu-item nil))
    (while (not (null menu-list))
      (setq menu-item (car menu-list))
      (setq menu-list (cdr menu-list))
      (if (not (null menu-item))
          (sams-xs-menu-item (car menu-item) (cdr menu-item) name)))))

(defun sams-xs-menu-item (path menu name)
  ""
  (let ((menu-list menu)
        (menu-item nil))

    (while (not (null menu-list))
      (setq menu-item (car menu-list))
      (setq menu-list (cdr menu-list))

      (cond ( (stringp menu-item)
              nil)

            ( (vectorp menu-item)
              (sams-xs-entry-found path menu-item name))

            ( (eq menu-item ':filter)
              (progn
                (setq menu-list (cdr menu-list))))

            ( (and (listp menu-item) (stringp (car menu-item)))
              (sams-xs-menu-item
	       (concat path " " (car menu-item)) (cdr menu-item) name)))
      )))

(defun sams-xs-entry-found (path menu-item name)
  "Not documented."
  (let* (( text (aref menu-item 0))
         (full-name (concat "menu-bar " path " " text))
         ( func (aref menu-item 1)))

    (if (listp func)
        (if (or (eq (car func) 'setq)
                (eq (car func) 'setq-default))
            (setq func (intern (symbol-name (cadr func))))))

    (if (equal func name)
        (setq sams-xemacs-menu-found
	      (cons full-name sams-xemacs-menu-found)))))


;;; ########################################################## &menus ###

;; Function for asking emacs where a given keybinding is located in
;; the menus.
;; This function might be bound to C-h M-c with the follwoing command:
;; (global-set-key [(control h) (meta c)] 'sams-list-all-bindings)

(defun sams-list-all-bindings (key)
  "Show where key resides in Emacs. This is pure interactive function."
  (interactive "kWhich key: ")
  (message "%s" (key-binding key))
  (sams-where-is (key-binding key)))

;;; ########################################################## &abbrev ###
;;; Make emacs save abbreviations as soon as they have been defined.
;;; by [Jesper]

(defadvice define-abbrev (after sams-save-abbrevs dis)
  (write-abbrev-file abbrev-file-name)
  (setq abbrevs-changed nil))

(defadvice define-abbrevs (after sams-save-abbrevs dis)
  (write-abbrev-file abbrev-file-name)
  (setq abbrevs-changed nil))

(defun sams-write-abbrev-at-once (&optional off)
  "Write abbrevs to  definition file as soon as they are defined or modified.
With \\[universal-argument] OFF, turn this feature off."
  (interactive "P")
  (cond
   (off
    (ad-disable-advice 'define-abbrev  'after 'sams-save-abbrevs)
    (ad-activate 'define-abbrev)
    (ad-disable-advice 'define-abbrevs 'after 'sams-save-abbrevs)
    (ad-activate 'define-abbrevs))
   (t
    (ad-enabel-advice 'define-abbrev  'after 'sams-save-abbrevs)
    (ad-activate 'define-abbrev)
    (ad-enable-advice 'define-abbrevs 'after 'sams-save-abbrevs)
    (ad-activate 'define-abbrevs))))

;;; ############################################################ &font ###
;;; Insert the information in the .Xdefaults file for getting the
;;; font and size of the current frame

(defun sams-insert-font-setup ()	;[Jesper]
  "Insert the current frame font and size at point, in .Xdefault format."
  (interactive)
  (let* ((func-frame 'frame-parameters)
	 (func-font  'face-font-name)
	 (font (if (fboundp func-font)   ; Xemacs
		   (funcall func-font 'default)
		 (cdr (assq 'font (funcall func-frame)))))
	 (geometry (format "%sx%s+0+0" (frame-width) (frame-height))))
    (insert (format "Emacs.font:%s\nEmacs.geometry:%s\n"
                    font geometry))))


;;; ############################################################ &text ###

(defun sams-transpose-prev-chars ()	;[Jesper]
  "Transposes the previous typed character and the one before it.
Usefull if bound to C-S-t ot even as a replacement for C-t."
  (interactive)
  (backward-char 1)
  (transpose-chars 1))

(defun sams-fill ()			;[Jesper]
  "If area is selected call `fill-region' otherwise call `fill-paragraph'."
  (interactive)
  (if (region-active-p)
      (fill-region (region-beginning) (region-end))
    (fill-paragraph nil)))

(defun sams-count-matching-lines (regexp) ;[Jesper]
  "Count lines matching REGEXP."
  (let ((lines 0))
    (progn
      (save-excursion
        (while (re-search-forward regexp (point-max) t)
          (setq lines (+ 1 lines))
          (forward-line)))
      lines)))


;; This function deletes all lines which don't match a given regular
;; expression. The user is asked for the regular expression and is told
;; how many lines will be deleted, and is asked to confirm that he wants
;; to delete these lines.  If the buffer is readonly, it makes the file
;; writable (assuming the reader has permission to do so), deletes the
;; lines, and makes it readonly again. Otherwise if the buffers isn't
;; changed, the user is asked whether he wants to make the buffer readonly,
;; to avoid accidentally saving it.

(defun sams-keep-lines (regexp)		;[Jesper]
  "Count lines not matching REGEXP and ask to delete those lines.
The user is told how many lines this is, and is asked to confirm that he
wants to delete these lines. If the buffer isn't modified before the
action, the function offers to make the buffer readonly, to avoid accidentally
saving it and deleting all the lines for ever."
  (interactive "sRegexp to match: ")
  (let* ((lines (sams-count-matching-lines regexp))
         (rest (count-lines (point) (point-max)))
         (total (- rest lines))
         (modified (buffer-modified-p))
         (read-only buffer-read-only))
    (progn
      ;; if the buffer is readonly, toggle this, so we can delete the lines
      (if read-only
          (vc-toggle-read-only))
      ;; are there any lines to delete?
      (if (> total 0)
          (if (y-or-n-p (concat "Delete "
                                total
                                " lines, which don't match the regexp "))
              (progn
                (keep-lines regexp)
                (if (and (not modified) (not read-only))
                    (if (y-or-n-p "Make buffer read-only ")
                        (progn
                          (set-buffer-modified-p nil)
                          (vc-toggle-read-only)))))
            (message "No lines deleted"))
        (message "No match"))
      ;; if the buffer was readonly, set the buffer to "not modified"
      ;; and toggle the readonly flag again.
      (if read-only
          (progn
            (set-buffer-modified-p nil)
            (vc-toggle-read-only))))))


;; This function is equivalent to the function above, with the only
;; difference that it delete lines which match.

(defun sams-kill-lines (regexp)		;[Jesper]
  "Count lines matching REGEXP and ask for deleting of those lines.
The user is told how many lines this is, and are asked to confirm that he
wants to delete these lines. If the buffer isn't modified before the
action, the function offers to make the buffer readonly, to avoid accidentally
saving it and deleting all the lines for ever."
  (interactive "sRegexp to match: ")
  (let* ((total (sams-count-matching-lines regexp))
         (modified (buffer-modified-p))
         (read-only buffer-read-only))
    (progn
      ; if the buffer is readonly, toggle this, so we can delete the lines
      (if read-only
          (vc-toggle-read-only))
      ; are there any lines to delete?
      (if (> total 0)
          (if (y-or-n-p (concat "Delete "
                                total
                                " lines, which match the regexp "))
              (progn
                (flush-lines regexp)
                (if (and (not modified) (not read-only))
                    (if (y-or-n-p "Make buffer read-only ")
                        (progn
                          (set-buffer-modified-p nil)
                          (vc-toggle-read-only)))))
            (message "No lines deleted"))
        (message "No match"))
      ; if the buffer were readonly, set the buffer to "not modified"
      ; and toggle the readonly flag again.
      (if read-only
          (progn
            (set-buffer-modified-p nil)
            (vc-toggle-read-only))))))


(defun sams-make-buffer-copy ()		;[Jesper]
"Make an indirect copy to dedicated frame with same `major-mode'."
  (interactive)
  (let* ( (b-name (buffer-name))
         (new-name (generate-new-buffer-name
                    (concat b-name "-indirect")))
         (m-mode major-mode))
    (make-indirect-buffer (current-buffer) new-name)

    ;;; Use this to get the buffer in a dedicated frame
    (special-display-popup-frame new-name)

    ;;; use this instead to get the buffer in another frame
    ; (switch-to-buffer-other-frame new-name)

    ;;; Start the major mode
    (set-buffer new-name)
    (funcall m-mode)
    ))

(defun sams-apply-macro-on-region (start end command) ;[Jesper]
  "Evaluate a given function (or the last defined macro) on region.
I.e. it will continue until the point is position
outside the region.

This function is much like the function apply-macro-to-region-lines,
which is shipped with Emacs. It has one difference though. It
executes the macros until point is below the end of the region."
  (interactive "r\naCommand name (default:last keyboard macro).")
  (goto-char end)
  (let ((mark (point-marker)))
    (goto-char start)
    (while (< (point) (marker-position mark))
    (if (not (fboundp command))
        (call-last-kbd-macro)
      (command-execute command)))))

(defun sams-unbound-key ()		;[Jesper]
  "Ring the bell and show message that key is not bound to any function."
  (interactive)
  (message "This key is not bound to any function")
  (ding))


;;; ########################################################## &gnus ###

;; This function makes auto-fill-mode start in gnus. adaptive-fill-mode
;; is, however a bad idea when replying to letters, as it may insert
;; the quote character in front of your own messages. Unfortunately
;; adaptive-fill-mode is required, when typing the TO: field (if you
;; use bbdb). In conclusion: adaptive-fill-mode is disabled when
;; replying to letters.

(defadvice message-reply (after sams-disable-adaptive dis)
  (make-local-variable 'adaptive-fill-mode)
  (setq adaptive-fill-mode nil))

(or (fboundp 'turn-on-auto-fill)
    (defun turn-on-auto-fill () (auto-fill-mode t)))

(defun sams-gnus-filling (&optional off) ;[Jesper]
  "Start auto filling in the message buffer of gnus.
Disable adaptive filling, when sending replies.

With optional argument OFF, restore Gnus to original settings."
  (interactive "P")
  (cond
   (off
    (ad-disable-advice 'message-reply  'after 'sams-disable-adaptive)
    (ad-activate 'message-reply)
    (remove-hook 'message-setup-hook 'turn-on-auto-fill))
   (t
    (ad-enable-advice 'message-reply  'after 'sams-disable-adaptive)
    (ad-activate 'message-reply)
    (add-hook 'message-setup-hook 'turn-on-auto-fill))))

;; These two functions is used to make gnus complete on entries in the
;; bbdb database and from the mail alias list

(defun sams-lookup-address-in-gnus ()	;[Jesper]
  "Complete on entries from the bbdb database or mail aliases list"
  (interactive)
  (let* ((function 'bbdb-complete-name))
    (when (null (expand-abbrev))
      (if (fboundp function)
	  (funcall function)
	(message "Sams lib: function bbdb-complete-name not loaded.")))))


(defun sams-bind-alias-tabs-in-gnus ()
  "Setup gnus to complete both entries from the bbdb database and on
mail aliases (eventually defined by gnus), using M-tab"
  (add-hook 'message-setup-hook
            (lambda ()
	      (local-set-key "\M-\t" 'sams-lookup-address-in-gnus ))))

(defun sams-gnus-increase-global-score (&optional score)
  "Increase the SCORE in the global score file."
  (interactive "P")
  (gnus-summary-increase-score score 'a))

(defun sams-gnus-lower-global-score (&optional score)
  "Decrease the SCORE in the global score file."
  (interactive "P")
  (gnus-summary-lower-score score 'a))

(defun sams-define-score-bindings ()
  "Bind C-S-l and C-S-i to functions which lowers
respectivly increases the score in the global score file for gnus."
  (interactive)
  (add-hook 'gnus-summary-mode-hook
            (lambda ()
              (local-set-key [(control I)] 'sams-gnus-increase-global-score)
              (local-set-key [(control L)] 'sams-gnus-lower-global-score))))


;;; ################################################## &trace-hooks ###

(defadvice run-hooks (before sams-trace-hook dis)
  (let ((nm (ad-get-arg 0)))
    (unless (memq nm '(auto-save-hook
		       activate-menubar-hook
		       menu-bar-update-hook
		       post-command-idle-hook
		       minibuffer-exit-hook
		       post-command-hook
		       activate-mark-hook
		       minibuffer-setup-hook
		       change-major-mode-hook
		       ))
      (message "Running hook '%s'" nm))))

(defun sams-trace-hooks ()
  (interactive)
  (ad-enable-advice 'run-hooks 'before 'sams-trace-hook)
  (ad-activate 'run-hooks))

(defun sams-stop-trace ()
  (interactive)
  (ad-disable-advice 'run-hooks 'before 'sams-trace-hook)
  (ad-activate 'run-hooks))

;;; ##################################################### &cycle-marks ###

(defvar sams-cm-ring nil
  "List of markers that points to buffer-positions.")

(defun sams-cm-same-pos ()
  (and sams-cm-ring
       (equal (point) (marker-position (car sams-cm-ring)))
       (equal (current-buffer) (marker-buffer (car sams-cm-ring)))))

(defun sams-cm-save-point (arg)
  (interactive "P")
  (if (or (and arg (< (prefix-numeric-value arg) 0))
          (sams-cm-same-pos))
      (progn
        (setq sams-cm-ring (cdr sams-cm-ring))
        (message "Point deleted from stack (%d left)" (length sams-cm-ring)))
    (setq sams-cm-ring (cons (point-marker) sams-cm-ring))
    (message "Point saved (%d saved)" (length sams-cm-ring))))


(defun sams-cm-rotate (num)
  "If point differ from first position in ring then goto that.
Otherwise rotate the ring of points and go to the now newest point in the ring"
  (interactive "P")
  (if (not sams-cm-ring)
      (error "No points saved!"))
  (setq num
        (if (null num) (if (sams-cm-same-pos) 1 0)
          (prefix-numeric-value num)))
  (setq num (mod num (length sams-cm-ring)))
  (let ((top nil))
    (while (> num 0)
      (setq top (cons (car sams-cm-ring) top))
      (setq sams-cm-ring (cdr sams-cm-ring))
      (setq num (1- num)))
    (setq sams-cm-ring (append sams-cm-ring (nreverse top)))
    (if (marker-position (car sams-cm-ring))
        (progn
          (switch-to-buffer (marker-buffer (car sams-cm-ring)))
          (goto-char (car sams-cm-ring)))
      (setq sams-cm-ring (cdr sams-cm-ring))
      (sams-cm-rotate 1))))


;;; ###################################################### &bug-report ###

(defun sams-submit-bug-report ()
  "Contact maintainer of package sams-lib.el and send feedback"
  (interactive)
  (reporter-submit-bug-report
   "Jesper Pedersen <blackie@ifad.dk>"
   "sams-lib.el"
   '(sams-lib-version
     gnus-version
     )
   nil nil
   "Hi,\n"
   )
  (goto-char (point-min))
  (insert "X-BugReport: elisp sams-lib.el " sams-lib-version "\n")
  (goto-char (point-min))
  (if (re-search-forward "Subject: *" nil t)
      (insert "feedback: Teach yourself Emacs in 24 hours "))
  (re-search-forward "Hi,\n\n")

  (message "Write any feedback you may have and Hit C-c C-c to send mail.")
  (sleep-for 2)
  )


(provide   'sams-lib)
(run-hooks 'sams-lib-load-hook)

(message "Loading sams-lib... Done")

;;; sams-lib.el ends here
