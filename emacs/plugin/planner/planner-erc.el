;;; planner-erc.el --- ERC support for Planner, an organizer for Emacs

;; Copyright (C) 2004, 2006, 2008 Free Software Foundation, Inc.

;; Emacs Lisp Archive Entry
;; Filename: planner-erc.el
;; Keywords: hypermedia erc chat
;; Author: Sacha Chua <sacha@free.net.ph>
;; Description: Create tasks and notes based on IRC
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

;; Place planner-erc.el in your load path and add this to your .emacs:
;;
;; (require 'planner-erc)

;; IRC URLs may be of the following forms.
;;
;; irc://server/nick,isnick or
;; irc://server/#channel or
;; irc://server

;; Annotations will be in the following forms.
;;
;; [[irc://server/nick,isnick][Chat with nick on server#channel]]
;; [[irc://server/nick,isnick][Chat with nick on server]]
;; [[irc://server/#channel][Chat on server#channel]]
;; [[irc://server][Chat on server]]

;;; Contributors:

;;; Code:

(require 'planner)
(require 'erc)

(unless (boundp 'erc-server-announced-name)
  (message "ERC 5.1 or higher is required; please upgrade ERC"))

(defvar planner-irc-regexp
  "\\`[ei]rc://\\([^:/\n]+\\)\\([^/\n]+\\)?\\(?:/\\([^,/\n]+\\)\\(.*\\)\\)?"
  "Regexp used to match IRC URLs.")

;;;###autoload
(defun planner-erc-annotation-from-erc ()
  "Return an annotation for the current line.
This function can be added to `planner-annotation-functions'."
  (when (eq major-mode 'erc-mode)
    (if (erc-default-target)
        (if (erc-channel-p (erc-default-target))
            (if (and (get-text-property (point) 'erc-parsed)
                     (elt (get-text-property (point) 'erc-parsed) 1))
                (let ((nick
                       (car
                        (erc-parse-user
                         (elt (get-text-property (point) 'erc-parsed) 1)))))
                  (planner-make-link
                   (concat "irc://"
                           erc-server-announced-name "/"
                           (substring nick 1) ",isnick")
                   (concat "Chat with " nick " on "
                           erc-server-announced-name (erc-default-target))
                   t))
              (planner-make-link
               (concat "irc://"
                       erc-server-announced-name "/"
                       (erc-default-target))
               (concat "Chat on " erc-server-announced-name
                       (erc-default-target))
               t))
          (planner-make-link
           (concat "irc://" erc-server-announced-name "/"
                   (erc-default-target))
           (concat "Chat with " (erc-default-target) " on "
                   erc-server-announced-name)
           t))
      (planner-make-link
       (concat "irc://" erc-server-announced-name)
       (concat "Chat on " erc-server-announced-name)
       t))))

;;;###autoload
(defun planner-erc-browse-url (url)
  "If this is an IRC URL, jump to it."
  (when (string-match planner-irc-regexp url)
    (let ((server (match-string 1 url))
          (port (match-string 2 url))
          (target (match-string 3 url))
          (flags (match-string 4 url))
          buffer-list)
      ;; find existing buffer
      (setq buffer-list
            (erc-buffer-filter
             (lambda nil
               (let ((server-buffer (erc-server-buffer)))
                 (and server-buffer
                      (string= server
                               (with-current-buffer server-buffer
                                 erc-server-announced-name)))))))
      ;; switch to buffer or create new connection
      (if buffer-list
          (if (not (stringp target))
              ;; assume that the car is always the server buffer
              (switch-to-buffer (car buffer-list))
            (switch-to-buffer
             (or (catch 'found
                   (dolist (buffer buffer-list)
                     (when (string= target (buffer-name buffer))
                       (throw 'found buffer))))
                 (car buffer-list))))
        (erc-select :server server :port port)))
    t))


(planner-add-protocol "[ei]rc://" 'planner-erc-browse-url 'identity)

(add-hook 'planner-annotation-functions 'planner-erc-annotation-from-erc)
(custom-add-option 'planner-annotation-functions
                   'planner-erc-annotation-from-erc)

(provide 'planner-erc)

;;; planner-erc.el ends here
