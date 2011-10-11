;;; planner-rmail.el --- RMAIL support for Planner, an organizer for Emacs

;; Copyright (C) 2004, 2006, 2008 Free Software Foundation, Inc.
;; Parts copyright (C) 2004, 2008 Frederik Fouvry

;;; Commentary:
;;
;;;_* Commentary

;;;_ + Package description

;; Emacs Lisp Archive Entry
;; Filename: planner-rmail.el
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

;; Place planner-rmail.el in your load path and add this to your .emacs:
;;
;;    (require 'planner-rmail)
;;
;; RMAIL URLs are of the form
;;
;; rmail://PATH/TO/INBOX/message-id
;;
;; Annotations will be of the form
;; [[rmail://PATH/TO/INBOX/<E1AyTpt-0000JR-LU@sacha.ateneo.edu>][E-mail from Sacha Chua]]

;;;_ + Contributors

;; Frederik Fouvry (fouvry AT coli DOT uni-sb DOT de) made an
;; improvement to gracefully deal with missing messages.

(require 'planner)
(require 'rmail)

;;; Code:

;; This isn't provided in XEmacs
(defun planner-rmail-narrow-to-non-pruned-header ()
  "Narrow to the whole (original) header of the current message."
  (if (fboundp 'rmail-narrow-to-non-pruned-header)
      (rmail-narrow-to-non-pruned-header)
    (let (start end)
      (narrow-to-region (rmail-msgbeg rmail-current-message) (point-max))
      (goto-char (point-min))
      (forward-line 1)
      (if (= (following-char) ?1)
          (progn
            (forward-line 1)
            (setq start (point))
            (search-forward "*** EOOH ***\n")
            (setq end (match-beginning 0)))
        (forward-line 2)
        (setq start (point))
        (search-forward "\n\n")
        (setq end (1- (point))))
      (narrow-to-region start end)
      (goto-char start))))


;;;###autoload
(defun planner-rmail-annotation-from-mail ()
  "Return an annotation for the current message.
This function can be added to `planner-annotation-functions'."
  (when (eq major-mode 'rmail-mode)
    (save-excursion
      (save-restriction
        (planner-rmail-narrow-to-non-pruned-header)
        (planner-make-link
         (concat "rmail://" (buffer-file-name) "/" (mail-fetch-field "message-id"))
         (if (and planner-ignored-from-addresses
                  (string-match planner-ignored-from-addresses
                                (mail-fetch-field "from")))
             (concat "E-mail to " (planner-get-name-from-address
                                   (mail-fetch-field "to")))
           (concat "E-mail from " (planner-get-name-from-address
                                   (mail-fetch-field "from"))))
	 t)))))

;;;###autoload
(defun planner-rmail-browse-url (url)
  "If this is an RMAIL URL, jump to it."
  (when (string-match "\\`rmail://\\(.+\\)/\\(.+?\\)$" url)
    (let ((message-id (match-string 2 url))
          (file (match-string 1 url))
          message-number)
      ;; Backward compatibility
      (save-excursion
        (save-window-excursion
          (rmail (if (string= file "RMAIL") rmail-file-name file))
          (setq message-number
                (save-restriction
                  (widen)
                  (goto-char (point-max))
                  (if (re-search-backward
                       (concat "^Message-ID:\\s-+" (regexp-quote message-id))
                       nil t)
                      (rmail-what-message))))))
      (if message-number
          (progn
            (rmail (if (string= file "RMAIL") rmail-file-name file))
            (rmail-show-message message-number)
            message-number)
        (error "Message not found")))))

(planner-add-protocol "rmail://" 'planner-rmail-browse-url nil)
(add-hook 'planner-annotation-functions 'planner-rmail-annotation-from-mail)
(custom-add-option 'planner-annotation-functions
                   'planner-rmail-annotation-from-rmail)
(planner-update-wiki-project)
(provide 'planner-rmail)

;;;_* Local emacs vars.

;; Local variables:
;; allout-layout: (* 0 : )
;; End:

;;; planner-rmail.el ends here
