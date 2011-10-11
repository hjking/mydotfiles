;;; planner-authz.el --- restrict portions of published pages

;; Copyright (C) 2004, 2005, 2006, 2008 Andrew J. Korty <ajk@iu.edu>
;; Parts copyright (C) 2004, 2005, 2006, 2007, 2008 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: planner-authz.el
;; Keywords: hypermedia
;; Author: Andrew J. Korty <ajk@iu.edu>
;; Maintainer: Andrew J. Korty <ajk@iu.edu>
;; Description: Control access to portions of published planner pages
;; URL:
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

;; This library lets you publish your planner pages while controlling
;; access to certain portions of them to users you specify.  When you
;; load this library, you gain access to two additional markup
;; directives to use in your planner pages.  The <authz> tag lets you
;; restrict access to arbitrary content as follows:

;;   Here is a sentence everyone should see.  This sentence also
;;   contains no sensitive data whatsoever.  <authz users="ajk">This
;;   sentence, however, talks about my predilection for that French
;;   vanilla instant coffee that comes in the little tin, and I'm
;;   embarrassed for anyone else to know about that.</authz> And
;;   here's some more perfectly innocuous content.

;; You can use <authz> tags to mark up entire paragraphs, tasks,
;; notes, and anything else.  The tags are replaced with Mason code by
;; default, but you could add support for some other templating system
;; by configuring planner-authz-mason-markup-strings and
;; planner-authz-after-publish-hook.

;; The #authz directive restricts access to an entire page.  It will
;; generate a 403 error when someone not listed tries to access it.
;; Any notes or tasks on a #authz-protected page are also wrapped in
;; authorization controls on linked pages.

;; * Diary Markup

;; If your pages have a section with diary entries maintained by
;; planner-appt.el (or by any other means), you can control access to
;; these entries.  First, customize `planner-section-tagnames' to map
;; your diary section ("* Schedule", in this example) to a tag called
;; "diary-section", for example:

;;   (add-to-list 'planner-section-tagnames '("Schedule" . "diary-section"))

;; If the name of your diary section is "* Diary", you will not need
;; to customize `planner-section-tagnames' by default.

;; Then make sure the diary entries you want restricted contain a
;; corresponding plan page name in parentheses, for example:

;;   10:00 10:30 Meeting with boss (WorkStuff)

;; * Startup

;; Add the following to your .emacs file to cause
;; M-x muse-project-publish to automatically use planner-authz
;; features.

;;   (require 'planner-authz)

;; * Customization

;; All user-serviceable options can be customized with
;; M-x customize-group RET planner-authz RET.

;; * Defaults

;; The following customization options let you set default access
;; lists for pages that don't have explicit settings:

;; planner-authz-project-default

;;   Default access list for project pages (not day pages).  If a
;;   given project page doesn't contain a #authz tag, it will receive
;;   the access list defined here.  If this variable is nil, all users
;;   will be allowed to view the page.  No corresponding variable is
;;   provided for day pages because it doesn't seem like you'd ever
;;   want to control access based on what day it was.  (But I will
;;   accept patches. :) Notes and tasks referencing pages without
;;   #authz tags will also be restricted to the users listed here.

;; planner-authz-day-note-default

;;   Default access list for notes on day pages not associated with
;;   any project.  There is way to set a default for notes on project
;;   pages for the reason above; they would only be associated with
;;   date pages anyway.

;; planner-authz-day-task-default

;;   Same as above but for tasks.

;;; Todo

;; - Make more specific tags override less specific ones, rather than
;;   more restrictive overriding less restrictive

;;; Code

(require 'planner-publish)

;; Customization options

(defgroup planner-authz nil
  "A planner.el extension for restricting portions of your
published pages to specified users."
  :group 'planner
  :prefix "planner-authz")

(defcustom planner-authz-after-publish-hook
  '(planner-authz-generate-mason-component)
  "Functions called after all pages have been published."
  :group 'planner-authz
  :type 'hook)

(defcustom planner-authz-appt-alt nil
  "If non-nil, show `planner-appt' appointments to users not
authorized to see them, but replace the text of the appointment with
the contents of this variable.  If nil, don't show any part of an
appointment to an unauthorized user.

For example, if this variable is set to \"Private appointment\" and
some hypothetical user is not authorized for the SecretStuff page, an
appointment that was entered as

 #A1  _ @10:00 12:00 Secret meeting (SecretStuff)

would appear to our unauthorized user as

 #A1  _ @10:00 12:00 Private appointment"
  :group 'planner-authz
  :type '(choice (string :tag "Replacement text")
                 (const :tag "Disable" nil)))

(defcustom planner-authz-appt-regexp
  (if (require 'planner-appt nil t)
      (concat "\\(?:[@!][ \t]*\\)?\\(?:" planner-appt-time-regexp
              "\\|&nbsp;\\)\\(?:[ \t|]+\\(?:" planner-appt-time-regexp
              "\\|&nbsp;\\)\\)?[ \t|]+"))
  "Regexp that matches a `planner-appt' start and end time specification."
  :group 'planner-authz
  :type 'string)

(defcustom planner-authz-day-note-default nil
  "Default list of users for restricting non-project notes on day pages."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-day-task-default nil
  "Default list of users for restricting non-project tasks on day pages."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-link-regexp
  (concat "(\\(" muse-explicit-link-regexp
          (if (boundp 'muse-wiki-wikiword-regexp)
              (concat "\\|" muse-wiki-wikiword-regexp))
          "\\|" muse-implicit-link-regexp "\\))$")
  "Regexp that matches the plan page link at the end of a line in a
task or diary entry."
  :group 'planner-authz
  :type '(string))

(defcustom planner-authz-mason-component-contents
  "<%once>
sub authz {
        my $r_user = $r ? $r->connection->user
                        : $ENV{REMOTE_USER} or return 0;
        foreach (@_) { return 1 if $r_user eq $_ }
        return 0;
}
</%once>
<%method content>
<%args>
$alt    => undef
@users
</%args>
% if (authz @users) {
<% $m->content %>\\
% } elsif ($alt) {
<% $alt %>\\
% }
</%method>
<%method page>
<%args>@users</%args>
<%perl>
unless (authz @users) {
        $m->clear_buffer;
        $m->abort(404);
}
</%perl>
</%method>
"
  "Mason code to be stored in a component.
The component's name is determined from
`planner-authz-mason-component-name'."
  :group 'planner-authz
  :type 'string)

(defcustom planner-authz-mason-component-name "authz.mas"
  "Name of Mason component that restricts content."
  :group 'planner-authz
  :type 'string)

(defcustom planner-authz-multi-func 'planner-authz-multi-union
  "*Function used to combine access lists for multiple planner pages.

When `planner-multi' is in effect and a task or note is linked to
multiple plan pages, `planner-authz' uses this function to decide how
to build the access list for the task or note from the access lists of
the linked pages.  

It is passed a list of sublists, each sublist being the access list (a
list of usernames) for one of the linked pages.  It should return a
combined single list of usernames.

Two such functions are provided: `planner-authz-multi-intersection'
returns only those user names that are common to all the access lists
for all the linked pages, and `planner-authz-multi-union' returns a
list of all the unique user names in any of those access lists."
  :group 'planner-authz
  :type '(radio (function-item :tag "Intersection"
                               planner-authz-multi-intersection)
                (function-item :tag "Union" planner-authz-multi-union)
                (function :tag "Other")))


(defcustom planner-authz-project-default nil
  "Default list of users for restricting project pages if #authz is nil."
  :group 'planner-authz
  :type '(repeat string))

(defcustom planner-authz-sections-regexp "^\\([*]\\)+\\s-+\\(.+\\)"
  "Regexp that matches headings for sections authorization markup."
  :group 'planner-authz
  :type '(string))

(defcustom planner-authz-sections-rule-list nil
  "List of sections and their access rule.

Each rule is a sublist of the form:

    (SECTION-NAME PREDICTION USER-LIST)

For sections matching SECTION-NAME, if the PREDICTION is t or a
function return t, that section will be accessable for users in
USER-LIST only.

The following example will make the \"Timeclock\" section and
\"Accomplishments\" section on day pages only accessable by user1 and
user2, while on plan pages obey the \"parent\" rule.

    ((\"Timeclock\" planner-authz-day-p
				       (\"user1\" \"user2\"))
    (\"Accomplishments\" planner-authz-day-p
				     (\"user1\" \"user2\")))"
  :group 'planner-authz
  :type '(repeat (regexp (choice boolean function))
		 (repeat string)))

(defcustom planner-authz-markup-regexps
  '((2300 "\\(<li>\\)\\(<&| [^<]*>\\)\\(.*\\)\\(</&>\\)\\(</li>\\)" 3
          planner-authz-fix-list-item))
  "List of markup rules for publishing PLANNER with `planner-authz' restrictions.
For more on the structure of this list, see `muse-publish-markup-regexps'."
  :group 'planner-authz
  :type '(repeat (choice
                  (list :tag "Markup rule"
                        integer
                        (choice regexp symbol)
                        integer
                        (choice string function symbol))
                  function)))

(defcustom planner-authz-markup-functions
  '((table . planner-authz-mason-markup-table))
  "An alist of style types to custom functions for that kind of text."
  :group 'planner-authz
  :type '(alist :key-type symbol :value-type function))

(defcustom planner-authz-markup-tags
  '(("authz"   t t nil planner-authz-tag)
    ("diary-section" t t nil planner-authz-diary-section-tag)
    ("note"    t t nil planner-authz-note-tag)
    ("task"    t t nil planner-authz-task-tag))
  "A list of tag specifications for authorization markup."
  :group 'planner-authz
  :type '(repeat (list (string :tag "Markup tag")
                       (boolean :tag "Expect closing tag" :value t)
                       (boolean :tag "Parse attributes" :value nil)
                       (boolean :tag "Nestable" :value nil)
                       function)))

(defcustom planner-authz-mason-markup-strings
  '((planner-authz-begin . "<&| authz.mas:content, 'users', [qw(%s)] &>")
    (planner-authz-begin-alt
     . "<&| authz.mas:content, 'users', [qw(%s)], 'alt', '%s' &>")
    (planner-authz-end   . "</&>")
    (planner-authz-page  . "<& authz.mas:page, 'users', [qw(%s)] &>"))
  "Strings used for additing authorization controls.

If a markup rule is not found here, `planner-html-markup-strings' is
searched."
  :type '(alist :key-type symbol :value-type string)
  :group 'planner-authz)

;; Non-customizable variables

(defvar planner-authz-pages nil
  "Alist of planner pages and users authorized to view them.
The list of users is separated by spaces.  This variable is
internal to planner-authz; do not set it manually.")
(defvar planner-authz-publishing-alist nil
  "Alist used by `planner-authz' to track published pages and their dependencies.
This alist stores pages that have been published during the current
publishing process, as (PAGENAME . t), and pages whose tasks and notes
depend on those pages for access control, as (PAGENAME . nil).  At the
end of publishing, `planner-authz' uses this alist to determine which
dependencies need to be republished, even if they themselves haven't
changed.")
(defvar planner-authz-disable-dependency-publishing nil
  "If non-nil, `planner-authz' will not republish unchanged pages whose tasks or notes depend on the page currently being published.
Normally, linked pages are republished in case the access list for the
current page has changed.  This variable is set to t while
`planner-authz' is republishing dependent pages to avoid indefinite
recursion.")

;;; Functions

(defun planner-authz-after-markup ()
  "Remove the page currently being marked up from the queue of pages
to republish and enforce default access controls for project pages."
  (let ((page (planner-page-name)))
    (when page

      (let ((cell (assoc page planner-authz-publishing-alist)))
        (if cell

            ;; if already t, the list is stale; whack it
            (if (cdr cell)
                (setq planner-authz-publishing-alist '(page . t)))

          (push '(page . t) planner-authz-publishing-alist)))

      (let ((users (planner-authz-users)))
        (when users
          (goto-char (point-min))
          (planner-insert-markup (muse-markup-text 'planner-authz-page users))
          (insert "\n"))))))

(defun planner-authz-after-project-publish (project)
  "Republish pages that reference restricted pages and call the
generate Mason code."
  (when (string= planner-project (car project))
    (while planner-authz-publishing-alist
      (if (not (cdar planner-authz-publishing-alist))
          (let ((planner-authz-disable-dependency-publishing t))
            (muse-project-publish-file (caar planner-authz-publishing-alist)
                                       planner-project t)))
      (setq planner-authz-publishing-alist
            (cdr planner-authz-publishing-alist)))
    (run-hook-with-args 'planner-authz-after-publish-hook project)))

(defun planner-authz-before-markup ()
  "Process #authz directives when publishing only a single page.  Mark
planner page sections according to
`planner-authz-sections-rule-list'."
  (planner-authz-markup-all-sections))

(defun planner-authz-day-p (&optional page)
  "Return non-nil if the current page or PAGE is a day page."
  (save-match-data
    (string-match planner-date-regexp (or page (planner-page-name)))))

(defun planner-authz-default (page)
  "Return the default space-separated string of users that would apply
to PAGE.  Nil is always returned for day pages."
  (and planner-authz-project-default
       (not (planner-authz-day-p page)) ; not on day pages
       (mapconcat 'identity planner-authz-project-default " ")))

(defun planner-authz-file-alist (users)
  "Generate a list of planner files that USERS have access to."
  (let ((pages (planner-file-alist))
        result)
    (while pages
      (let (not-found-p)
        (with-temp-buffer
          (insert-file-contents-literally (cdar pages))
          (when (re-search-forward "^#authz\\s-+\\(.+\\)\n+" nil t)
            (let ((users-iter users)
                  (authz (split-string (match-string 1))))
              (while (and users-iter (not not-found-p))
                (unless (member (car users-iter) authz)
                  (setq not-found-p t))
                (setq users-iter (cdr users-iter)))))
          (unless not-found-p
            (setq result (append (list (car pages)) result))))
        (setq pages (cdr pages))))
    result))

(defun planner-authz-fix-list-item ()
  "Rearrange list items restricted by `planner-authz' to avoid empty list items on the published page."
  (replace-match "\\2\\1\\3\\5\\4")
  (muse-publish-mark-read-only (match-beginning 0) (match-end 2))
  (muse-publish-mark-read-only (match-beginning 4) (match-end 0)))

(defun planner-authz-generate-mason-component (project)
  "Generate the Mason component restricting content.
The component's name is taken from
`planner-authz-mason-component-name' and initialized with the
contents of `planner-authz-mason-component-contents'.  The
component restricts access to users specified by <authz> and
#authz tags."
  (with-temp-buffer
    (insert planner-authz-mason-component-contents)
    (let ((backup-inhibited t)
          (styles (cddr project)))
      (while styles
        (let ((path (muse-style-element :path (car styles))))
          (and path
               (string-match "mason" (muse-style-element :base (car styles)))
               (write-file
                (concat (file-name-directory path)
                        planner-authz-mason-component-name))))
        (setq styles (cdr styles))))))

(defun planner-authz-markup-section-predict (rule)
  "Check if the prediction is satisfied."
  (let ((predict (elt rule 1)))
    (if (functionp predict)
	(funcall predict)
      predict)))

(defun planner-authz-markup-section ()
  "Restrict section according to `planner-authz-sections-rule-list'."
  (let ((begin (planner-line-beginning-position))
	(rule-list planner-authz-sections-rule-list)
	section-name
	section-level
	next-section-regexp)
    (goto-char begin)
    (save-match-data
      (re-search-forward planner-authz-sections-regexp nil t)
      (setq section-level (length (match-string 1)))
      (setq section-name (match-string 2)))
    (let ((rule (catch 'done
                  (while rule-list
                    (if (string-match (caar rule-list) section-name)
                        (throw 'done (car rule-list))
                      (setq rule-list (cdr rule-list))))
                  nil)))
      (if (and rule
	       (planner-authz-markup-section-predict rule))
	  (progn
	    (goto-char begin)
            (muse-publish-surround-text
             (format "<authz users=\"%s\">\n"
                     (mapconcat 'identity (elt rule 2) " "))
             "\n</authz>\n"
	     (lambda ()
	       (save-match-data
		 (let ((found nil))
		   (re-search-forward planner-authz-sections-regexp nil t)
		   (while (and (not found)
			       (re-search-forward planner-authz-sections-regexp
                                                  nil t))
		     (if (<= (length (match-string 1))
			     section-level)
			 (setq found t)))
		   (if found
		     (goto-char (planner-line-beginning-position))
		   (goto-char (point-max))))))))))))

(defun planner-authz-markup-all-sections ()
  "Run `planner-authz-markup-section' on the entire buffer."
  (goto-char (point-min))
  (while (re-search-forward planner-authz-sections-regexp nil t)
    (planner-authz-markup-section)))

(defun planner-authz-mason-markup-table ()
  "Protect \"<&|\" Mason constructs from Muse table markup."
  (let* ((beg (planner-line-beginning-position))
         (style (muse-style-element :base (muse-style)))
         (base (if style
                   (muse-style-element :base style)))
         (func (if base
                   (muse-find-markup-element
                    :functions 'table (muse-style-element :base base)))))
    (when (functionp func)
      (save-excursion
        (save-match-data
          (goto-char beg)
          (while (search-forward "<&|" (line-end-position) t)
            (replace-match "<&:" t t))))
      (funcall func)
      (let ((end (point)))
        (goto-char beg)
        (while (search-forward "<&:" end t)
          (replace-match "<&|" t t))))))


(defun planner-authz-index-as-string (&optional as-list exclude-private)
  "Generate an index of all Muse pages with authorization controls.
In the published index, only those links to pages which the remote
user is authorized to access will be shown.
If AS-LIST is non-nil, insert a dash and spaces before each item.
If EXCLUDE-PRIVATE is non-nil, exclude files that have private permissions.
If EXCLUDE-CURRENT is non-nil, exclude the current file from the output."
  (with-temp-buffer
    (insert (planner-index-as-string as-list exclude-private))
    (when muse-publishing-p
      (goto-char (point-min))
      (while (and (re-search-forward
                   (if as-list
                       (concat "^[" muse-regexp-blank "]+-["
                               muse-regexp-blank "]*")
                     (concat "^[" muse-regexp-blank "]*"))
                   nil t)
                  (looking-at muse-explicit-link-regexp))
        (let* ((link (buffer-substring (point) (line-end-position)))
               (page (planner-link-base link))
               (users (if page (planner-authz-users page))))
          (if users
              (progn
                (insert (format "<authz users=\"%s\">" users))
                (end-of-line)
                (insert "</authz>"))
            (end-of-line)))))
    (buffer-substring (point-min) (point-max))))

(defun planner-authz-republish-dependencies-maybe (linked-pages)
  "Remember LINKED-PAGES to be republished later.
The pages will be republished if and only if the current page is
restricted."
  (and (not planner-authz-disable-dependency-publishing)
       (planner-authz-users)
       (while linked-pages
         (unless (assoc (car linked-pages) planner-authz-publishing-alist)
           (push '(car linked-pages) planner-authz-publishing-alist))
         (setq linked-pages (cdr linked-pages)))))

(defun planner-authz-tag (beg end attrs)
  "Publish <authz> tags.  The region from BEG to END is protected.
ATTRS should be an alist of tag attributes including \"users\" and
optionally \"alt\" for alternative text to be displayed to
unauthorized users."
  (save-excursion
    (let ((alt   (or (cdr (assoc "alt"   attrs)) ""))
          (users (or (cdr (assoc "users" attrs)) "")))
      (goto-char beg)
      (planner-insert-markup
        (if (zerop (length alt))
            (muse-markup-text 'planner-authz-begin users)
          (muse-markup-text 'planner-authz-begin-alt users alt)))
      (goto-char end)
      (planner-insert-markup (muse-markup-text 'planner-authz-end)))))

(defun planner-authz-diary-section-tag (beg end attrs)
  "Restrict entries in a diary section."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (planner-publish-nested-section-tag beg end)
      (goto-char beg)
      (while (and (zerop (forward-line))
                  (= (point) (planner-line-beginning-position)))
        (unless (looking-at "^\\(?:[ \t]*\\|No entries\\|</div>\\)$")
          (let ((line-begin (point))
                (line-end (line-end-position)))
            (re-search-forward planner-authz-link-regexp line-end t)
            (let* ((link (match-string 1))
                   (linked-pages (if link
                                     (mapcar 'planner-link-base
                                             (if (featurep 'planner-multi)
                                                 (planner-multi-split link)
                                               link))))
                   (linked-users
                    (if linked-pages
                        (planner-authz-multi-users linked-pages)
                      (and planner-authz-day-task-default
                           (mapconcat 'identity planner-authz-day-task-default
                                      " ")))))
              (when linked-users
                (if (and planner-authz-appt-alt planner-authz-appt-regexp
                           (progn
                             (goto-char line-begin)
                             (re-search-forward
                              planner-authz-appt-regexp line-end t)))
                    (progn
                      (search-forward " - " (+ 2 (point)) t)
                      (planner-insert-markup
                       (muse-markup-text 'planner-authz-begin-alt linked-users
                                         planner-authz-appt-alt)))
                  (planner-insert-markup
                   (muse-markup-text 'planner-authz-begin linked-users)))
                (end-of-line)
                (planner-insert-markup
                 (muse-markup-text 'planner-authz-end))))))))))

(defun planner-authz-note-tag (beg end attrs)
  "Restrict notes linked to a restricted page.  If this page is
restricted and the note is linked to another page, remember to
republish that page later and restrict the note as it appears there.
Call `planner-publish-note-tag' as a side effect."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (planner-publish-note-tag beg end attrs)
      (let* ((categories (cdr (assoc "categories" attrs)))
             (links (if (or (not categories) (zerop (length categories)))
                        (cdr (assoc "link" attrs))
                      categories))
             (linked-pages (if (and links (not (zerop (length links))))
                               (mapcar 'planner-link-base
                                       (if (featurep 'planner-multi)
                                           (planner-multi-split links)
                                         links))))
             (linked-users
              (if linked-pages
                  (planner-authz-multi-users linked-pages)
                (and planner-authz-day-note-default
                     (planner-authz-day-p)
                     (mapconcat 'identity
                                planner-authz-day-note-default " ")))))

        ;; If this note is linked to another page, republish that page
        ;; later to restrict the note as it appears there, providing that
        ;; page has an authz restriction

        (if linked-pages
            (planner-authz-republish-dependencies-maybe linked-pages))

        ;; If the linked page has an authz restriction, restrict this note
      
        (when linked-users
          (goto-char (point-min))
          (planner-insert-markup
           (muse-markup-text 'planner-authz-begin linked-users))
          (insert "\n")
          (goto-char (point-max))
          (planner-insert-markup (muse-markup-text 'planner-authz-end))
          (insert "\n"))))))

(defun planner-authz-task-tag (beg end attrs)
  "Restrict tasks linked to restricted pages.  If this page is
restricted and the task is linked to another page, remember to
republish that page later and restrict the task as it appears there.
Call `planner-publish-task-tag' as a side effect."
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (planner-publish-task-tag beg end attrs)
      (let* ((link (cdr (assoc "link" attrs)))
             (linked-pages (if link
                               (mapcar 'planner-link-base
                                       (if (featurep 'planner-multi)
                                           (planner-multi-split link)
                                         link))))
             (linked-users
              (if linked-pages
                  (planner-authz-multi-users linked-pages)
                (and planner-authz-day-task-default
                     (planner-authz-day-p)
                     (mapconcat 'identity
                                planner-authz-day-task-default " ")))))

        ;; If this task is linked to another page, republish that page
        ;; later to restrict the task as it appears there, providing that
        ;; page has an authz restriction

        (if linked-pages
            (planner-authz-republish-dependencies-maybe linked-pages))

        ;; If the linked page has an authz restriction, restrict this task

        (when linked-users
          (goto-char (point-min))
          (planner-insert-markup
           (muse-markup-text 'planner-authz-begin linked-users))
          (goto-char (point-max))
          (planner-insert-markup (muse-markup-text 'planner-authz-end)))))))

(defun planner-authz-users (&optional page)
  "Return a list of acceptable users for PAGE.
The list of users is returned as space-separated string, based on
a #authz directive appearing in the page.  If PAGE contains no
#authz directive and is a project page (it doesn't match
`planner-date-regexp'), return `planner-authz-project-default' as
a space-separated string.

If PAGE is nil, return a list of users associated with the
current page."
  (unless page (setq page (planner-page-name)))
  (let ((match (cdr (assoc page planner-authz-pages))))
    (unless match
      (let ((file (cdr (assoc page (planner-file-alist)))))
        (setq match
              (or (and file
                       (with-temp-buffer
                         (insert-file-contents-literally file)
                         (if (re-search-forward "^#authz\\s-+\\(.+\\)\n+"
                                                nil t)
                             (match-string 1))))
                  (planner-authz-default page))))
      (push `(,page . ,match) planner-authz-pages))
    match))

(defun planner-authz-multi-intersection (list)
  "Merge a list of `planner-authz' access lists, returning a list of only those user names that are common to all the passed access lists."
  (let ((count (length list))
        alist intersection sublist)

    ;; in alist, associate each name with its frequency of appearance
    (while list
      (setq sublist (car list))
      (while sublist
        (let ((entry (assoc (car sublist) alist)))
          (if entry
              (setcdr entry (1+ (cdr entry)))
            (setq alist (cons `(,(car sublist) . 1) alist))))
        (setq sublist (cdr sublist)))
      (setq list (cdr list)))

    ;; those names with `count' frequencies were in every sublist
    (while alist
      (if (= (cdar alist) count)
          (setq intersection (cons (caar alist) intersection)))
      (setq alist (cdr alist)))
    intersection))

(defun planner-authz-multi-union (list)
  "Merge a list of `planner-authz' access lists, returning a list of all the unique user names in any of those access lists."
  (let (sublist union)
    (while list
      (setq sublist (car list))
      (while sublist
        (add-to-list 'union (car sublist))
        (setq sublist (cdr sublist)))
      (setq list (cdr list)))
    union))

(defun planner-authz-multi-users (pages)
  "Return a merged access list for PAGES.
The list of users is returned as space-separated string, based on a
#authz directive appearing in the PAGES.  If one of PAGES contains no
#authz directive and is a project page (it doesn't match
`planner-date-regexp'), it will contribute
`planner-authz-project-default' to the merge."
  (let ((users
         (funcall planner-authz-multi-func
                  (mapcar (lambda (page)
                            (if (not (planner-authz-day-p page))
                                (let ((users (planner-authz-users page)))
                                  (if users
                                      (split-string users)))))
                          pages))))
    (if users
          (mapconcat 'identity users " "))))

(add-hook 'muse-after-project-publish-hook
          'planner-authz-after-project-publish)

(let ((styles (list "html" "xhtml")))
  (while styles
    (let ((style (concat "planner-authz-mason-" (car styles))))
      (unless (assoc style muse-publishing-styles)
        (muse-derive-style
         style (concat "planner-" (car styles))
         :before     'planner-authz-before-markup
         :after      'planner-authz-after-markup
         :functions  'planner-authz-markup-functions
         :regexps    'planner-authz-markup-regexps
         :strings    'planner-authz-mason-markup-strings
         :tags       (append planner-authz-markup-tags
                             planner-publish-markup-tags))))
    (setq styles (cdr styles))))

(provide 'planner-authz)

;;; planner-authz.el ends here
