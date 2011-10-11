;;; planner-registry.el --- registry for Planner

;; Copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;; Time-stamp: <2006-01-23 18:21:18 guerry>
;;
;; Author: Bastien Guerry <bzg@altern.org>
;; Version: $Id: planner-registry.el,v 0.1 2006/01/23 17:21:21 guerry Exp $
;; Keywords: planner muse registry

;; This file is part of Planner.  It is not part of GNU Emacs.

;; Planner is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 3, or (at your
;; option) any later version.

;; Planner is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Planner; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This module provides a way to keep track of all the URLs in your
;; projects, and to list them depending on the current buffer.  The
;; URLs are defined in `muse-url-protocols' - it does NOT include
;; wikiwords (for now).
;;
;; If a URL has been created by `planner-create-task-from-buffer',
;; going to that buffer and calling `planner-registry-show' will show you
;; where planner put the URL.
;;
;; Say for example that you created a task from an e-mail.  Go to that
;; e-mail and call `planner-registry-show': it will open a new buffer
;; displaying the files (in a muse links format) where a link to this
;; e-mail has been added.

;;; Getting Started:

;; Put this in your init file:
;;
;; (require 'planner-registry)
;; (planner-registry-initialize)
;;
;; You MUST put it after the place where where Planner has been loaded
;; in your configuration file.
;;
;; If you want the registry to be updated each time you save a Planner
;; file, add this:
;;
;; (planner-registry-insinuate)
;;
;; If you don't want to update the registry each time a file is
;; written, you can do it manually with `planner-registry-update': it
;; will update the registry for saved muse/planner buffers only.
;;
;; There's no default `define-key' for `planner-registry-show' because
;; it's not bounded to one particular mode.  You can bound it to
;; whatever you want.

;;; Todo:

;; 1) Better windows manipulations
;; 2) Wiki links support

;;; Problems:

;; If you're using this with Planner, the default value of
;; `planner-bibtex-separator' must be changed from ":" to something
;; else.
;;
;; (setq planner-bibtex-separator "#")
;;
;; "#" as a separator enables you to perform fuzzy-matching on bibtex
;; URLs as well.

;;; History:

;; 2005.11.22 - new release.
;; 2005.11.18 - first release.

;;; Contributors:

;;; Code:

;;;_* Prerequisites

(require 'planner)

;;;_* Options

(defgroup planner-registry nil
  "A registry for Planner."
  :prefix "planner-registry-"
  :group 'muse)

;; You can setq this var to what do you like.
(defcustom planner-registry-file
  (concat (getenv "HOME") "/.planner-registry.el")
  "The registry file."
  :type 'string
  :group 'planner-registry)

(defcustom planner-registry-min-keyword-size 3
  "Minimum size for keywords."
  :type 'integer
  :group 'planner-registry)

(defcustom planner-registry-max-keyword-size 10
  "Maximum size for keywords."
  :type 'integer
  :group 'planner-registry)

(defcustom planner-registry-max-number-of-keywords 3
  "Maximum number of keywords."
  :type 'integer
  :group 'planner-registry)

(defcustom planner-registry-ignore-keywords
  '("E-Mail" "from" "www")
  "A list of ignored keywords."
  :type 'list
  :group 'planner-registry)

(defcustom planner-registry-show-level 0
  "Level for `planner-registry-show'.
0 means that this function shows only exact matches.
1 means that this function also shows descriptive matches.
2 (or more) means that this function also shows fuzzy matches."
  :type 'boolean
  :group 'planner-registry)

;;;_* Other variables and constants

(defvar planner-registry-alist nil
  "An alist containing the Planner registry.")

(defconst planner-registry-url-regexp
  (concat "\\(" (mapconcat 'car muse-url-protocols "\\|") "\\)"
	  "[^][" muse-regexp-blank "\"'()^`{}\n]*[^][" muse-regexp-blank
	  "\"'()^`{}.,;\n]+")
  "A regexp that matches Muse URL links.")

(defconst planner-registry-link-regexp
  (concat "\\[\\[\\(" planner-registry-url-regexp
	  "\\)\\]\\[\\([^][\n]+\\)\\]\\]")
  "A regexp that matches Muse explicit links.")

(defconst planner-registry-url-or-link-regexp
  (concat "\\(" planner-registry-url-regexp "\\)\\|"
	  planner-registry-link-regexp)
  "A regexp that matches both Muse URL and explicit links.
The link is returned by `match-string' 3 or 1.
The protocol is returned bu `match-string' 4 or 2.
The description is returned by `match-string' 5")

;;;_* Core code

;;;###autoload
(defun planner-registry-initialize (&optional from-scratch)
  "Set `planner-registry-alist' from `planner-registry-file'.
If `planner-registry-file' doesn't exist, create it.
If FROM-SCRATCH is non-nil, make the registry from scratch."
  (interactive "P")
  (if (or (not (file-exists-p planner-registry-file))
	  from-scratch)
      (planner-registry-make-new-registry)
    (planner-registry-read-registry))
  (message "Planner registry initialized"))

(defun planner-registry-update nil
  "Update the registry from the current buffer."
  (interactive)
  (let* ((from-file (buffer-file-name))
	 (new-entries
	  (planner-registry-new-entries from-file)))
    (planner-registry-update-registry from-file new-entries))
  (with-temp-buffer
    (insert-file-contents planner-registry-file)
    (eval-buffer)))

(defun planner-registry-insinuate nil
  "Call `planner-registry-update' after saving in Planner mode.
Use with caution.  This could slow down things a bit."
  (interactive)
  (add-hook 'planner-mode-hook
	    (lambda nil
	      (add-hook 'after-save-hook 'planner-registry-update t t))))

(defun planner-registry-show (&optional level)
  "Show entries at LEVEL.
See `planner-registry-show-level' for details."
  (interactive "p")
  (let ((annot (run-hook-with-args-until-success
		'planner-annotation-functions))
	(level (or level planner-registry-show-level)))
    (if (not annot)
	(message "Annotation is not supported for this buffer")
      (let ((entries (planner-registry-get-entries annot level)))
	(if (not entries)
	    (message
	     (format "No match (level %d) for \"%s\"" level
		     (progn (string-match
			     planner-registry-url-or-link-regexp annot)
			    (match-string 5 annot))))
	  (delete-other-windows)
	  (switch-to-buffer-other-window
	   (set-buffer (get-buffer-create "*Planner registry*")))
	  (erase-buffer)
	  (dolist (elem entries)
	    (dolist (entry elem)
	      (insert entry))
	    (when elem (insert "\n")))
	  (muse-mode))))))

(defun planner-registry-create nil
  "Create `planner-registry-file'."
  (let ((items planner-registry-alist)
	item)
    (with-temp-buffer
      (find-file planner-registry-file)
      (erase-buffer)
      (insert
       (with-output-to-string
	 (princ ";; -*- emacs-lisp -*-\n")
	 (princ ";; Planner registry\n;; What are you doing here?\n\n")
	 (princ "(setq planner-registry-alist\n'(\n")
	 (while items
	   (when (setq item (pop items))
	     (prin1 item)
	     (princ "\n")))
	 (princ "))\n")))
      (save-buffer)
      (kill-buffer (current-buffer))))
  (message "Planner registry created"))

(defun planner-registry-entry-output (entry)
  "Make an output string for ENTRY."
  (concat " - [[pos://" (car entry)
	  "#" (nth 1 entry) "]["
	  (planner-registry-get-project-name (car entry))
	  ": " (file-name-nondirectory (car entry))
	  "]] - [[" (nth 2 entry) "][" (nth 3 entry) "]]\n"))

(defun planner-registry-get-project-name (file)
  "Get project name for FILE."
  (let ((file1 (directory-file-name
		(file-name-directory file))))
    (planner-replace-regexp-in-string "/?[^/]+/" "" file1 t t)))

(defun planner-registry-read-registry nil
  "Set `planner-registry-alist' from `planner-registry-file'."
  (with-temp-buffer
    (find-file planner-registry-file)
    (eval-buffer)
    (kill-buffer (current-buffer))))

(defun planner-registry-update-registry (from-file new-entries)
  "Update the registry FROM-FILE with NEW-ENTRIES."
  (with-temp-buffer
    (find-file planner-registry-file)
    (goto-char (point-min))
    (while (re-search-forward
	    (concat "^(\"" from-file) nil t)
      (delete-region (planner-line-beginning-position)
		     (planner-line-end-position)))
    (goto-char (point-min))
    (re-search-forward "^(\"" nil t)
    (goto-char (match-beginning 0))
    (dolist (elem new-entries)
      (insert (with-output-to-string (prin1 elem)) "\n"))
    (save-buffer)
    (kill-buffer (current-buffer)))
    (message (format "Planner registry updated for URLs in %s"
		     (file-name-nondirectory
		      (buffer-file-name)))))

(defun planner-registry-make-new-registry nil
  "Make a new `planner-registry-alist' from scratch."
  (setq planner-registry-alist nil)
  (let ((muse-directories (mapcar 'caadr muse-project-alist))
	muse-directory)
    (while muse-directories
      (when (setq muse-directory (pop muse-directories))
	(mapcar (lambda (file)
		  (unless (or (file-directory-p file)
			      (let ((case-fold-search nil))
				(string-match muse-project-ignore-regexp
					      file)))
		    (dolist (elem (planner-registry-new-entries file))
		      (add-to-list 'planner-registry-alist elem))))
		(directory-files muse-directory t)))))
  (planner-registry-create))

(defun planner-registry-new-entries (file)
  "List links in FILE that will be put in the registry."
  (let (result)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (re-search-forward planner-registry-url-or-link-regexp nil t)
	(let* ((point (number-to-string (match-beginning 0)))
	       (link (or (match-string-no-properties 3)
			 (match-string-no-properties 1)))
	       (desc (or (match-string-no-properties 5)
			 (progn (string-match
				 planner-registry-url-regexp link)
				(substring
				 link (length (match-string 1 link))))))
	       (keywords (planner-registry-get-keywords desc))
	       (ln-keyword (planner-registry-get-link-keywords link)))
	  (add-to-list 'result
		       (list file point link desc keywords ln-keyword)))))
    result))

(defun planner-registry-get-entries (annot level)
  "Show the relevant entries in the registry.
ANNOT is the annotation for the current buffer.
LEVEL is set interactively or  set to `planner-registry-show-level'."
  (when (string-match planner-registry-url-or-link-regexp annot)
    (let* ((link (or (match-string 3 annot)
		     (match-string 1 annot)))
	   (desc (or (match-string 5 annot) ""))
	   exact-match descriptive fuzzy)
      (dolist (entry planner-registry-alist)
	(let* ((output (planner-registry-entry-output entry))
	       (keyword (nth 4 entry))
	       (ln-keyword (nth 5 entry)))
	  ;; exact matching
	  (when (equal (nth 2 entry) link)
	    (add-to-list 'exact-match output))
	  ;; descriptive matching
	  (when (and (> level 0) (equal (nth 3 entry) desc))
	    (unless (member output exact-match)
	      (add-to-list 'descriptive output)))
	  ;; fuzzy matching
	  (when (and (> level 1)
		     (or (string-match ln-keyword link)
			 (string-match keyword desc)))
	    ;; use (planner-registry-get-keywords)?
	    (unless (or (member output exact-match)
			(member output descriptive))
	      (add-to-list 'fuzzy output)))))
      (when exact-match
	(add-to-list 'exact-match
		     (concat "* Exact match(es):\n\n")))
      (when descriptive
	(add-to-list 'descriptive
		     (concat "* Description match(es):\n\n")))
      (when fuzzy
	(add-to-list 'fuzzy
		     (concat "* Fuzzy match(es):\n\n")))
      (cond (fuzzy (list exact-match descriptive fuzzy))
	    (descriptive (list exact-match descriptive))
	    (exact-match (list exact-match))
	    (t nil)))))

(defun planner-registry-get-link-keywords (link)
  "Make a list of keywords for LINK."
  (setq link (car (planner-split-string link "#" t))))

(defun planner-registry-get-keywords (desc)
  "Make a list of keywords for DESC."
  (let ((kw (planner-split-string desc "[ ./]+" t)))
    (mapcar (lambda (wd) (setq kw (delete wd kw)))
	    planner-registry-ignore-keywords)
    (setq kw
	  (mapcar (lambda (a)
		    (when (>= (length a) planner-registry-min-keyword-size)
		      (substring
		       a 0 (if (> (length a) planner-registry-max-keyword-size)
			       planner-registry-max-keyword-size (length a)))))
		  kw))
    (setq kw (delq nil kw))
    (setq kw (nthcdr (- (length kw)
			planner-registry-max-number-of-keywords) kw))
    (mapconcat (lambda (e) e) kw ".*")))

(provide 'planner-registry)

;;; planner-registry.el ends here
;;
;; Local Variables:
;; indent-tabs-mode: t
;; tab-width: 8
;; End:
