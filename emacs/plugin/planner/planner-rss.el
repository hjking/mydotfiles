;;; planner-rss.el --- RSS export for the Emacs Planner (planner.el)

;; Copyright (C) 2003, 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2005, 2008 David D. Smith

;; Emacs Lisp Archive Entry
;; Filename: planner-rss.el
;; Keywords: hypermedia
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Export planner entries as an RSS feed
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

;; If you use `remember-to-planner' to keep daily notes, you can
;; automatically publish remembered notes as an RSS feed by adding the
;; following code to your .emacs:
;;
;;   (add-to-list 'remember-planner-append-hook 'planner-rss-add-note t)
;;
;; You can also invoke `planner-rss-add-note' on any note you would
;; like added.

;;;_ + Contributors

;; David Smith (davidsmith AT acm DOT org) ported this to work with
;; Muse.

;;; Code:

(require 'planner)
(require 'planner-publish)

(defgroup planner-rss nil
  "Planner options for RSS feeds."
  :prefix "planner-rss-"
  :group 'planner)

(defcustom planner-rss-base-url
  ""
  "Base URL for blog entries. Should include trailing /.
Example: http://sacha.free.net.ph/notebook/wiki/"
  :type 'string
  :group 'planner-rss)

;; On my system, this is set to
;;'(("."
;;   "/home/sacha/public_html/notebook/wiki/blog.rdf"
;;   "<?xml version=\"1.0\"?><rss version=\"2.0\"><channel>
;;<title>sachachua's blog</title>
;;<link>http://sacha.free.net.ph/notebook/wiki/today.php</link>
;;<description>Random notes</description>
;;</channel></rss>\n"))
(defcustom planner-rss-category-feeds
  nil
  "List of (CONDITION FILENAME INITIAL-CONTENTS).

If CONDITION is a regexp, all entries that match the regexp in
either title or body will be included in FILENAME. If CONDITION
is a function with one argument, it will be called with the
marked-up text, and a non-nil return value means include this
entry in FILENAME.

If INITIAL-CONTENTS is non-nil, it is used to initialize the file if
the file is not found or is corrupted.

Example:
'((\".\"
   \"/home/sacha/public_html/notebook/wiki/blog.rdf\"
   \"<?xml version=\\\"1.0\\\"?><rss version=\\\"2.0\\\"><channel>
<title>sachachua's blog</title>
<link>http://sacha.free.net.ph/notebook/wiki/today.php</link>
<description>Random notes</description>
</channel></rss>\n\"))"
  :type '(repeat (group (choice regexp function) file string))
  :group 'planner-rss)

(defcustom planner-rss-feed-limits nil
  "A list of (REGEX SIZE-LIMIT ITEM-LIMIT).

REGEX is a regular expression that matches the filename.
SIZE-LIMIT, if non-nil, is the upper limit in characters.
ITEM-LIMIT, if non-nil, is the upper limit in items. If the feed
exceeds the stated limits, older items are deleted."
  :type '(alist :key-type regexp
                :value-type (group (choice
                                    :tag "Characters: "
                                    (const :tag "No limit" nil)
                                    (integer))
                              (choice
                               :tag "Size: "
                               (const :tag "No limit" nil)
                               (integer))))
  :group 'planner-rss)

;; Determined from planner-rss-category-feeds.
;; You don't need to set this.
(defvar planner-rss-file-name nil "Filename of current RSS feed.")
(defvar planner-rss-initial-contents nil "Initial contents.")

(defun planner-rss-add-item (item)
  "Add an item to the top of the items list in `planner-rss-file-name'.
It will have TITLE, LINK, DESCRIPTION, PUBDATE and CATEGORIES.
`planner-rss-initialize' is called if necessary."
  (save-excursion
    (save-window-excursion
      (find-file planner-rss-file-name)
      (goto-char (point-min))
      (unless (re-search-forward "<item>\\|</channel>" nil t)
        (progn
          (erase-buffer)
          (insert planner-rss-initial-contents)
          (goto-char (point-max))
          (re-search-backward "</channel>")))
      (goto-char (match-beginning 0))
      (insert item)
      (planner-rss-limit)
      (save-buffer))))

(defun planner-rss-strip-tags (string)
  "Remove all tags from STRING."
  (planner-replace-regexp-in-string "<[^>]+>" "" string))

;;;###autoload
(defun planner-rss-add-note (&optional feed)
  "Export the current note using `planner-rss-add-item'.
If FEED is non-nil, add the note to the specified feed only.
Call with the interactive prefix in order to be prompted for FEED."
  (interactive (list (when current-prefix-arg
                       (read-file-name "Feed: "))))
  (save-window-excursion
    (save-excursion
      (save-restriction
        (when (planner-narrow-to-note)
          (let* ((seen)
                 (text (buffer-substring-no-properties (point-min) (point-max)))
                 (muse-publishing-current-file (buffer-file-name))
                 (entry (with-temp-buffer
                          (insert text)
                          (muse-publish-markup-buffer "*rss*" "planner-rss")
                          (buffer-string))))
            (dolist (feed planner-rss-category-feeds nil)
              (let ((condition (elt feed 0))
                    (planner-rss-file-name (elt feed 1))
                    (planner-rss-initial-contents (elt feed 2)))
                (when (cond ((functionp condition)
                             (funcall condition text))
                            ((stringp condition)
                             (string-match condition text))
                            (t condition))
                  (unless (member planner-rss-file-name seen)
                    (add-to-list 'seen planner-rss-file-name)
                    (planner-rss-add-item entry)))))))))))

(defun planner-rss-limit ()
  "Apply limits specified in `planner-rss-feed-limits'."
  (let ((filename (expand-file-name (planner-current-file))))
    (mapcar
     (lambda (item)
       (when (string-match (elt item 0) filename)
         (planner-rss-limit-size (elt item 1))
         (planner-rss-limit-items (elt item 2))))
     planner-rss-feed-limits)))

(defun planner-rss-limit-size (limit)
  "Delete RSS items that cause this file to go over LIMIT characters."
  (when limit
    (widen)
    (goto-char limit)
    (unless (eobp)
      (re-search-backward "<item>" nil t)
      (let ((start (match-beginning 0)))
        (re-search-forward "</channel>" nil t)
        (delete-region start (match-beginning 0))))))

(defun planner-rss-limit-items (limit)
  "Delete RSS items past the LIMIT-th item."
  (when limit
    (widen)
    (goto-char (point-min))
    (while (and (> limit -1) (re-search-forward "<item>" nil t))
      (setq limit (1- limit)))
    (when (= limit -1)
      (let ((start (match-beginning 0)))
        (re-search-forward "</channel>" nil t)
        (delete-region start (match-beginning 0))))))

(defun planner-publish-markup-note-rss ()
  "Replace note with RSS 2.0 representation of note data.  Borrowed
  heavily from Sacha's personal configs."
  (save-restriction
    (narrow-to-region
     (save-excursion (beginning-of-line) (point))
     (or (save-excursion (and (re-search-forward "<item>\\|</channel>" nil t) 
                              (match-beginning 0)))
         (point-max)))
    (let ((info (planner-current-note-info t)))
      (delete-region (point-min) (point-max))
      (muse-insert-markup
       "<item>\n"
       "<title>"
       (muse-publish-escape-specials-in-string (planner-note-title info))
       "</title>\n"
       "<link>"
       (concat planner-rss-base-url (muse-page-name) ".html#"
	       (planner-note-anchor info))
       "</link>\n"
       "<guid>"
       (concat planner-rss-base-url (muse-page-name) ".html#"
	       (planner-note-anchor info))
       "</guid>\n")
      (when (planner-note-body info)
        (muse-insert-markup "<description><![CDATA["
                (with-temp-buffer
                  (insert (planner-note-body info))
                  (muse-publish-markup-buffer "*title*" "planner-rss-info")
                  (buffer-string))
                "]]></description>\n"))
      (when (planner-note-date info)
        (muse-insert-markup "<pubDate>"
                (let ((system-time-locale "C")
                      (timestamp (planner-note-timestamp info))
                      (date (planner-filename-to-calendar-date
                             (planner-note-date info)))
                      (minutes) (hour) (day) (month) (year))
                  (format-time-string
                   "%a, %d %b %Y %T %Z"
                   (when (string-match "\\([0-9]+\\):\\([0-9]+\\)" timestamp)
                     (let ((hour (string-to-number (match-string 1 timestamp)))
                           (minutes (string-to-number
                                     (match-string 2 timestamp)))
                           (month (nth 0 date))
                           (day (nth 1 date))
                           (year (nth 2 date)))
                       (encode-time 0 minutes hour day month year)))))
                 "</pubDate>\n"))
      (muse-insert-markup "</item>\n"))))

(defcustom planner-publish-markup-rss-functions
  '((note . planner-publish-markup-note-rss))
  "An alist of style types to custom functions for that kind of text for RSS.
For more on the structure of this list, see
`muse-publish-markup-functions'."
  :type '(alist :key-type symbol :value-type function)
  :group 'planner-publish)

(unless (assoc "planner-rss" muse-publishing-styles)
  (muse-derive-style "planner-rss" "planner-xml"
                     :functions 'planner-publish-markup-rss-functions
                     :header    ""
                     :footer    ""
                     :prefix    planner-rss-base-url)
  (muse-derive-style "planner-rss-info" "planner-html"
                     :header    ""
                     :footer    ""
                     :prefix    planner-rss-base-url))

(provide 'planner-rss)

;;; planner-rss.el ends here
