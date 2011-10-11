;;; planner-w3m.el --- W3M integration for the Emacs Planner

;; Copyright (C) 2001, 2008 Free Software Foundation, Inc.

;; Author: John Wiegley <johnw@gnu.org>
;; Keywords: planner, w3m
;; URL: http://www.wjsullivan.net/PlannerMode.html

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

;; This module allows you to create tasks from a w3m buffer.

;;; Contributors:

;; John Sullivan provided a patch to check w3m-buffer-title.

;;; Code:

(require 'w3m)
(require 'planner)

;;;###autoload
(defun planner-w3m-annotation-from-w3m ()
  "If called from a w3m page, return an annotation.
Suitable for use in `planner-annotation-functions'."
  (when (eq major-mode 'w3m-mode)
    (planner-make-link
     w3m-current-url (or w3m-current-title
                         (w3m-buffer-title (current-buffer))) t)))

(add-hook 'planner-annotation-functions 'planner-w3m-annotation-from-w3m)
(custom-add-option 'planner-annotation-functions
                   'planner-w3m-annotation-from-w3m)

(provide 'planner-w3m)

;;; planner-w3m.el ends here
