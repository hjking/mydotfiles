;;; vlog-skel.el --- smart skeletons for vlog-mode

;; Copyright (C)  2004 Sun Yijiang <sunyijiang@gmail.com>

;; Author:     Sun Yijiang
;; Maintainer: Sun Yijiang
;; Created:    Dec. 2004
;; Keywords:   languages, verilog

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:


;;; Code:

(require 'cl)
(require 'vlog-indent)

(defgroup vlog-skel nil
  "Skeleton settings for vlog-mode."
  :group 'verilog)

(defcustom vlog-skel-always-at-string "always @"
  "Default always block beginning for `vlog-skel-smart-always'."
  :type  'string
  :group 'vlog-skel)

(defcustom vlog-skel-default-clock "clk"
  "Default clock signal name for `vlog-skel-smart-always'."
  :type  'string
  :group 'vlog-skel)

(defcustom vlog-skel-default-reset "if (~reset_b)"
  "Default reset signal name for `vlog-skel-smart-always'."
  :type  'string
  :group 'vlog-skel)

(defcustom vlog-skel-user-name "vlog-mode user"
  "User name for vlog-mode."
  :type  'string
  :group 'vlog-skel)

(defcustom vlog-skel-company-name "www.gnu.org"
  "Company name for vlog-mode."
  :type  'string
  :group 'vlog-skel)

(defcustom vlog-skel-header-string "\
//^File Header ------------------------------------------< Powered By Emacs>--
// Copyright (C) %<time %Y> %<company>
// All rights reserved
// ---------------------------------------------------------------------------
// FILE NAME        : %<filename>
// MODULE NAME      : %<modulename>
// AUTHOR           : %<author>
// ---------------------------------------------------------------------------
// [RELEASE HISTORY]                               Last Modified : %<time %y-%m-%d>
// VERSION  DATE        AUTHOR      DESCRIPTION
// 1.0%<align>%<time %y-%m-%d>%<align>%<author>%<align>Original
// ---------------------------------------------------------------------------
// [DESCRIPTION]
// %<_>
// ---------------------------------------------------------------------------
// [SUBMODULE LIST]
// [ N o n e ]
// ---------------------------------------------------------------------------
// [PARAMETERS]
// PARAM_NAME       RANGE       DEFAULT     DESCRIPTION
// ----------       -----       -------     -----------
// [ N o n e ]
//$File Header ------------------------------------------< Powered By Emacs>--\n"
  "Header template for `vlog-skel-smart-header'.
You can use special markups as shown below:
%<time FORMAT> => Format time string with FORMAT. e.g. %<time %y-%m-%d>.
                  The format markups is the same with `format-time-string'.
%<filename>    => Current file name. Empty if there's none.
%<modulename>  => Current module name. Use `vlog-lib-get-current-module-name'.
%<author>      => Replced with `vlog-skel-user-name'.
%<company>     => Replced with `vlog-skel-company-name'.
%<align>       => Align current position. similar to `indent-relative'.
%<_>           => Default cursor position after the insertion of header."
  :type  'string
  :group 'vlog-skel)

(defvar vlog-skel-map nil
  "Keymap used in Vlog mode for smart skeletons.")

(defun vlog-skel-setup-keymap ()
  "Set up `vlog-skel-map' for vlog-skel."
  (setq vlog-skel-map (make-sparse-keymap))
  (define-key vlog-skel-map "\C-a" 'vlog-skel-smart-always-edge)
  (define-key vlog-skel-map "a"    'vlog-skel-smart-always-comb))

(defun vlog-skel-smart-always-comb ()
  "Insert a new level sensitive always block."
  (interactive)
  (beginning-of-line)
  (insert "// auto sense //\n")
  (insert vlog-skel-always-at-string "()")
  (vlog-indent-line)
  (insert "\n")
  (vlog-indent-line))

(defun vlog-skel-smart-always-edge ()
  "Insert a new edge sensitive always block."
  (interactive)
  (let ((edge "pos")
        (clk  vlog-skel-default-clock)
        (rst  vlog-skel-default-reset))
    (save-excursion
      (when (vlog-re-search-backward "always\\s-*@\\s-*(\\(pos\\|neg\\)edge" (point-min) t)
        (setq edge (match-string 1))
        (when (vlog-re-search-forward "\\s-*\\(\\sw+\\))" (line-end-position) t)
          (setq clk (match-string 1))
          (forward-line 1))
        (when (vlog-re-search-forward "\\s-*\\(if\\s-*([^() \t]+)\\)" (line-end-position) t)
          (setq rst (match-string 1)))))
    (beginning-of-line)
    (insert vlog-skel-always-at-string "(" edge "edge " clk ")")
    (vlog-indent-line)
    (insert "\n" rst)
    (vlog-indent-line)
    (insert "\n;")
    (vlog-indent-line)
    (insert "\nelse")
    (vlog-indent-line)
    (previous-line 1)
    (backward-char 1)))

(defun vlog-skel-smart-header ()
  "Insert header for current source. If there's a header already, locate it.
Set `vlog-skel-header-string' to customize your own header."
  (interactive)
  (let ((pt (save-excursion
              (goto-char (point-min))
              ;; Yes, you need to hack this
              (re-search-forward "/[/\\*].*< Powered By Emacs>" (point-max) t))))
    (if (not pt)
        (vlog-skel-insert-header)
      (goto-char pt)
      (beginning-of-line))))

(defun vlog-skel-insert-header ()
  "Insert header for current source."
  (let (end fstr)
    (when (stringp vlog-skel-header-string)
      (goto-char (point-min))
      (insert vlog-skel-header-string)
      (setq end (point))
      (goto-char (point-min))
      (while (re-search-forward "%<\\([^>]+\\)>" end t)
        (setq fstr (match-string-no-properties 1))
        (replace-match "")
        (insert (vlog-skel-format-string fstr)))
      (re-search-backward "%<_>" (point-min) t)
      (replace-match ""))))

(defun vlog-skel-format-string (fstr)
  "Return formated strings."
  (let (type data temp)
    (if (not (string-match "^\\(\\w+\\)\\([ \t]+\\(.+\\)\\)*$" fstr))
        fstr
      (setq type (match-string 1 fstr)
            data (match-string 3 fstr))
      (cond
       ((string= type "time")
        (format-time-string data))
       ((string= type "filename")
        (vlog-lib-get-current-file-name))
       ((string= type "modulename")
        (or (vlog-lib-get-current-module-name)
            (file-name-sans-extension (vlog-lib-get-current-file-name))))
       ((string= type "author")
        (if (stringp vlog-skel-user-name) vlog-skel-user-name ""))
       ((string= type "company")
        (if (stringp vlog-skel-company-name) vlog-skel-company-name ""))
       ((string= type "align")
        (vlog-skel-get-align-spaces))
       ((string= type "_")
        "%<_>")
       (t fstr)))))

(defun vlog-skel-get-align-spaces ()
  "Get align spaces for template markup %<align>. Enhanced version of \\t."
  (let ((ccol (current-column))
        col slist)
    (save-excursion
      (if (/= (forward-line -1) 0) " "
        (if (not (vlog-lib-move-to-column ccol)) " "
          (unless (= (current-column) ccol)
            (backward-char 1))
          (skip-chars-forward "^ \t")
          (skip-chars-forward " \t")
          (setq col (abs (- (current-column) ccol))
                slist nil)
          (while (/= 0 col)
            (push 32 slist)
            (setq col (1- col)))
          (concat slist))))))

(provide 'vlog-skel)

;;; vlog-skel.el ends here
