;;; planner-ledger.el --- ledger support for planner

;; Copyright (C) 2004, 2008 Will Glozer (will AT glozer DOT net)
;; Parts copyright (C) 2004, 2005, 2007, 2008 Free Software Foundation, Inc.

;; Author: Will Glozer (will AT glozer DOT net)

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

;; This planner module provides integration between planner and
;; John Wiegley's ledger accounting program available at:
;;
;;    http://newartisans.com/johnw/ledger.tar.gz
;;
;; planner-ledger can insert a ledger balance overview and a list of pending
;; transactions into a planner day page.  To do so, simply add a hook:
;;
;;   (add-hook 'planner-goto-hook 'planner-ledger-insert-maybe)
;;
;; and make sure `planner-day-page-template' includes sections that match
;; `planner-ledger-balance-regexp' and `planner-ledger-pending-regexp'.
;;
;; planner-ledger can also create a new ledger entry based on a planner
;; task that matches `planner-ledger-payment-task-regexp', which by default
;; matches entries like:
;;
;;   #B0  _ payment due: Payee, $100.00 from 2004.07.01
;;
;; Bind `planner-ledger-add-entry-from-task' to a convenient key stroke and
;; execute it when in a payment task.

;;; Contributors

;; Travis B. Hartwell made this usable with new versions of ledger and
;; made it more flexible.

(require 'ledger)
(require 'planner)

;;; Code:

(defgroup planner-ledger nil
  "Planner-ledger provides integration between Planner and
John Wiegley's ledger accounting program."
  :group 'planner)

(defcustom planner-ledger-data-file
  nil
  "Ledger file to use.  This is the full path to the data file."
  :type  '(file :must-match t)
  :group 'planner-ledger)

(defcustom planner-ledger-balance-regexp
  "^\* Ledger *$"
  "Section marker for insertion of ledger balance."
  :type 'regexp
  :group 'planner-ledger)

(defcustom planner-ledger-pending-regexp
  "^\*\* Pending Transactions *$"
  "Section marker for insertion of pending ledger transactions."
  :type 'regexp
  :group 'planner-ledger)

(defcustom planner-ledger-balance-accounts
  '("Assets" "Liabilities" "-Equity")
  "Accounts to include or exclude from ledger balance overview."
  :type '(repeat string)
  :group 'planner-ledger)

(defcustom planner-ledger-balance-args
  '("-s" "-e" "next month" "balance")
  "Command line arguments for ledger balance."
  :type '(repeat string)
  :group 'planner-ledger)

(defcustom planner-ledger-register-args
  '("-U" "register")
  "Command line arguments for ledger register."
   :type '(repeat string)
   :group 'planner-ledger)

(defcustom planner-ledger-payment-task-regexp
  (concat planner-task-regexp
          "payment\\s-+due:\\s-+\\([^,]+?\\),\\s-*\\([[:graph:]]+\\)")
  "Regular expression matching planner tasks for ledger payment.
The first parenthesized group should match the payee. The second
group should match the amount.

Example task:
#A0 _ payment due: foobar, $1000.00 some comment here"
  :type 'regexp
  :group 'planner-ledger)

;;;###autoload
(defun planner-ledger-insert-maybe ()
  "Maybe insert ledger sections into a Planner page."
  (interactive)
  (planner-ledger-insert-balance-maybe)
  (planner-ledger-insert-pending-maybe))

(defun planner-ledger-insert-balance-maybe ()
  "Maybe insert ledger account balances a Planner page.
The accounts are specified in planner-ledger-balance-accounts."
  (interactive)
  (planner-ledger-clear-section-balance)
  (apply 'planner-ledger-insert-section-maybe
         planner-ledger-balance-regexp
         (append planner-ledger-balance-args
                 planner-ledger-balance-accounts)))

(defun planner-ledger-insert-pending-maybe ()
  "Maybe insert ledger pending transaction into a Planner page."
  (interactive)
  (planner-ledger-clear-section-pending)
  (apply 'planner-ledger-insert-section-maybe
         planner-ledger-pending-regexp
         (append planner-ledger-register-args)))

(defun planner-ledger-insert-section-maybe (regexp &rest ledger-args)
  "Maybe insert a ledger section into a Planner page.
Argument REGEXP is the section heading to find.  Optional argument
LEDGER-ARGS contains the arguments to pass to
`ledger-run-ledger'."
  (save-excursion
    (goto-char (point-min))
    (when (re-search-forward regexp nil t)
      (progn
        (newline 2)
        (apply 'planner-ledger-run-ledger ledger-args)))))

(defun planner-ledger-clear-section-balance ()
  "Clear the planner-ledger section for Ledger balance."
  (interactive)
  (save-excursion
    (planner-ledger-clear-section planner-ledger-balance-regexp "^\\*")))

(defun planner-ledger-clear-section-pending ()
  "Clear the planner-ledger section for pending transactions."
  (interactive)
  (save-excursion
    (planner-ledger-clear-section planner-ledger-pending-regexp "^\\*")))

(defun planner-ledger-clear-section (regexp-start regexp-end)
  "Clear a planner ledger section."
  (goto-char (point-min))
  (when (re-search-forward regexp-start nil t)
    (progn
      (forward-line)
      (delete-region (point) (if (re-search-forward regexp-end nil t)
                                 (line-beginning-position)
                               (point-max))))))

(defun planner-ledger-goto-section-end (regexp-start)
  "Goto the end of the current section or end of buffer.
Assumes that sections are marked with an asterisk."
  (if (re-search-forward regexp-start nil t)
      (line-beginning-position)
    (point-max)))

(defun planner-ledger-add-entry-from-task ()
  "Add a new ledger entry from the task at point."
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (re-search-forward planner-ledger-payment-task-regexp
                           (planner-line-end-position)
                           t)
        (let* ((payee  (match-string 1))
               (amount (match-string 2))
               (date   (planner-filename-to-calendar-date (buffer-name)))
               (buffer (find-buffer-visiting planner-ledger-data-file)))
          (unless buffer (setq buffer (find-file planner-ledger-data-file)))
          (pop-to-buffer buffer)
          (ledger-add-entry (format "%d/%02d/%02d %s %s"
                                    (extract-calendar-year date)
                                    (extract-calendar-month date)
                                    (extract-calendar-day date)
                                    payee
                                    amount)))
      (message "Not in a ledger payment task"))))

(defun planner-ledger-run-ledger (&rest ledger-args)
  "Run ledger for planner-ledger.

Run the ledger binary with ledger-run-ledger using the value of
`planner-ledger-data-file'.

If the file is open in a buffer, use the buffer. Otherwise
specify the file as an option to the ledger binary command and
avoid loading it in Emacs."
  (let ((buffer (get-file-buffer planner-ledger-data-file)))
    (if buffer
        (apply 'ledger-run-ledger buffer ledger-args)
      (apply #'call-process
             (append (list ledger-binary-path nil t nil
                           "-f" planner-ledger-data-file)
                     ledger-args)))))

(provide 'planner-ledger)

;;; planner-ledger.el ends here
