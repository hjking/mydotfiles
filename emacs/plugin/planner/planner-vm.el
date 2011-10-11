;;; planner-vm.el --- VM support for Planner, an organizer for Emacs

;; Copyright (C) 2004, 2005, 2008 Free Software Foundation, Inc.

;;; Commentary:
;;
;;;_* Commentary

;;;_ + Package description

;; Emacs Lisp Archive Entry
;; Filename: planner.el
;; Keywords: hypermedia
;; Author: John Wiegley <johnw@gnu.org>
;; Description: Use Emacs for life planning
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

;;;_ + Usage
;;
;; Place planner-vm.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-vm)
;;
;; VM URLs are of the form
;;
;; vm://path/to/inbox/message-id
;;
;; Annotations will be of the form
;; [[vm://home/test/INBOX/<E1AyTpt-0000JR-LU@sacha.ateneo.edu>][E-mail from Sacha Chua]]

;;;_ + Contributors

;; Jürgen Doser (email address unknown) contributed a patch to
;; properly open a vm message.

;; Greg Novak provided a message-id bugfix.

;; Fran Burstall provided a patch that turned on annotations in
;; vm-presentation-mode.

(require 'planner)
(require 'vm)

;;; Code:

;;;###autoload
(defun planner-vm-annotation-from-mail ()
  "Return an annotation for the current message.
This function can be added to `planner-annotation-functions'."
  (save-excursion
    (save-restriction
      (when (eq major-mode 'vm-summary-mode)
        (vm-follow-summary-cursor)
        (set-buffer vm-mail-buffer))
      (when (memq major-mode '(vm-mode vm-presentation-mode))
        (let ((message (car vm-message-pointer)))
          (planner-make-link
           (concat "vm://"
                   (buffer-file-name
                    (marker-buffer
                     (elt (vm-location-data-of
                           (vm-real-message-of message)) 0)))
                   "/"
                   (vm-get-header-contents message "Message-ID"))
           (if (and planner-ignored-from-addresses
                    (string-match planner-ignored-from-addresses
                                  (vm-get-header-contents message "From")))
               (concat "E-mail to " (planner-get-name-from-address
                                     (vm-get-header-contents message "To")))
             (concat "E-mail from "
                     (planner-get-name-from-address
                      (vm-get-header-contents message "From"))))
	   t))))))

;;;###autoload
(defun planner-vm-browse-url (url)
  "If this is an VM URL, jump to it."
  (when (string-match "\\`vm://\\(.+\\)/\\([^/\n]+\\)$" url)
    (let ((message-id (match-string 2 url))
          (inbox (match-string 1 url)))
      ; open the inbox read-only
      (vm inbox t)
      (let ((mp vm-message-list)
            (done nil))
        ; search for matching message-id
        (while (and mp (not done))
          (if (string= message-id (vm-get-header-contents (car mp) "Message-ID"))
              ; jump to the message and done
              (progn
                (vm-record-and-change-message-pointer
                 vm-message-pointer mp)
                (vm-preview-current-message)
                (setq done t))
            (setq mp (cdr mp))))
        t))))

(planner-add-protocol "vm://" 'planner-vm-browse-url nil)
(add-hook 'planner-annotation-functions 'planner-vm-annotation-from-mail)
(custom-add-option 'planner-annotation-functions
                   'planner-vm-annotation-from-mail)
(planner-update-wiki-project)
(provide 'planner-vm)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; planner-vm.el ends here
