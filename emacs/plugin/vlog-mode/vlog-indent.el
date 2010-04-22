;;; vlog-indent.el --- handle verilog code indentation

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
(require 'vlog-lib)
(require 'vlog-align)

;;+ variables, constants and customs ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup vlog-indent nil
  "Indentation settings of vlog-mode."
  :group 'verilog)

(defcustom vlog-indent-align-port-list-to-paren nil
  "If t, indent ports inside parens."
  :group 'vlog-indent
  :type  'boolean)

(defcustom vlog-indent-align-else-to-if nil
  "If true, align `else' under matching `if'.  Otherwise else is
lined up with first character on line holding matching if."
  :group 'vlog-indent
  :type  'boolean)

(defcustom vlog-indent-level-beh 2
  "Indentation level for continued line after always @ (...)."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-block 0
  "Indentation level for blocks."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-block-beh 0
  "Indentation level for blocks after beh."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-block-inside 2
  "Indentation level for lines indise blocks."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-case-inside 4
  "Indentation level for lines inside case block."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-case-branch-inside 2
  "Indentation level for branch indise \"case\"."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-cond 3
  "Indentation level for continued line after conditions."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-default 4
  "Default indentation level."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-indent-level-port-list 4
  "Indentation level for ports indise port list."
  :group 'vlog-indent
  :type  'integer)

(defcustom vlog-use-generic-directive t
  "Non-nil means `vlog-indent-directives-re' is set to generic.
Nil means `vlog-indent-directives-re' is generated from
`vlog-indent-directives'"
  :group 'vlog-indent
  :type  'boolean)

(defvar vlog-indent-directives
  '("`case" "`celldefine" "`default" "`define" "`define" "`else" "`elsif"
    "`endcelldefine" "`endfor" "`endif" "`endprotect" "`endswitch" "`endwhile"
    "`for" "`format" "`if" "`ifdef" "`ifndef" "`include" "`let" "`protect"
    "`switch" "`timescale" "`time_scale" "`undef" "`while" "`file" "`line")
  "Directive words for indentation.")
(defvar vlog-indent-directives-re nil
  "Regexp built from `vlog-indent-directives'.")

(defvar vlog-indent-calc-begs
  '("always" "initial" "module" "macromodule" "primitive"
    "function" "task" "table" "specify" "generate"))
(defvar vlog-indent-calc-begs-re nil)

(defvar vlog-indent-paren-sexp-signs
  '("if" "for" "repeat" "while" "wait")
  "Keywords before a pair of parenthesis that make a statement sexp.")
(defvar vlog-indent-paren-sexp-signs-re nil
  "Regexp made of `vlog-indent-paren-sexp-signs'")

(defvar vlog-indent-paren-cond-signs
  '("if" "for" "while" "wait" "case" "casex" "casez" "repeat")
  "Keywords before a pair of parenthesis that make a cond.")
(defvar vlog-indent-paren-cond-signs-re nil
  "Regexp made of `vlog-indent-paren-cond-signs'")

(defvar vlog-indent-special-beg-daily-words
  '("begin" "fork"))
(defvar vlog-indent-special-beg-daily-words-re nil
  "Regexp made of `vlog-indent-special-beg-daily-words'")
(defvar vlog-indent-special-beg-scarce-words
  '("module" "macromodule" "primitive" "function"
    "task" "table" "specify" "generate"))
(defvar vlog-indent-special-beg-scarce-words-re nil
  "Regexp made of `vlog-indent-special-beg-scarce-words'")

(defvar vlog-indent-special-end-daily-words
  '("end" "join" "endcase" "else"))
(defvar vlog-indent-special-end-daily-words-re nil
  "Regexp made of `vlog-indent-special-end-daily-words'")
(defvar vlog-indent-special-end-scarce-words
  '("endmodule" "endprimitive" "endfunction" "endtask"
    "endtable" "endspecify" "endgenerate"))
(defvar vlog-indent-special-end-scarce-words-re nil)

(defvar vlog-indent-beh-words
  '("always" "initial"))

(defvar vlog-indent-block-beg-words
  '("begin" "fork" "case" "casex" "casez" "function" "task"
    "table" "specify" "generate" "module" "macromodule"))

(defvar vlog-indent-block-end-words
  '("join" "end" "endcase" "endfunction" "endtask"
    "endtable" "endspecify" "endgenerate" "endmodule"))

(defvar vlog-indent-defun-words
  '("module" "macromodule" "primitive" "endmodule" "endprimitive"))

(defvar vlog-indent-words nil)

(defvar vlog-indent-beh-words-re nil)
(defvar vlog-indent-block-beg-words-re nil)
(defvar vlog-indent-block-beg-words-re-1 nil)
(defvar vlog-indent-block-end-words-re nil)
(defvar vlog-indent-block-end-words-re-1 nil)
(defvar vlog-indent-defun-words-re nil)
(defvar vlog-indent-words-re nil)

;;+ indentations ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-indent-make-regexps ()
  "Make regexps used in indentation calculating.
using `vlog-indent-directives',

To override or modify default lists, set `vlog-indent-*' and then
call me."
  (setq vlog-indent-words
        (append vlog-indent-beh-words
                vlog-indent-block-beg-words
                vlog-indent-block-end-words
                vlog-indent-defun-words))
  (dolist (regexp
           '((vlog-indent-paren-sexp-signs . vlog-indent-paren-sexp-signs-re)
             (vlog-indent-paren-cond-signs . vlog-indent-paren-cond-signs-re)
             (vlog-indent-special-beg-daily-words . vlog-indent-special-beg-daily-words-re)
             (vlog-indent-special-beg-scarce-words . vlog-indent-special-beg-scarce-words-re)
             (vlog-indent-special-end-daily-words . vlog-indent-special-end-daily-words-re)
             (vlog-indent-special-end-scarce-words . vlog-indent-special-end-scarce-words-re)
             (vlog-indent-beh-words . vlog-indent-beh-words-re)
             (vlog-indent-block-beg-words . vlog-indent-block-beg-words-re)
             (vlog-indent-block-end-words . vlog-indent-block-end-words-re)
             (vlog-indent-defun-words . vlog-indent-defun-words-re)
             (vlog-indent-words . vlog-indent-words-re)
             (vlog-indent-calc-begs . vlog-indent-calc-begs-re)))
    (set (cdr regexp)
         (vlog-regexp-wrap (vlog-regexp-opt (symbol-value (car regexp)) nil))))
  (setq vlog-indent-block-beg-words-re-1
        (vlog-regexp-wrap (vlog-regexp-opt (append vlog-indent-block-beg-words
                                                   '("always" "assign"))
                                           nil)))
  (setq vlog-indent-block-end-words-re-1
        (vlog-regexp-wrap (vlog-regexp-opt (append vlog-indent-block-end-words
                                                   '("always" "assign"))
                                           nil)))
  (setq vlog-indent-directives-re
        (if vlog-use-generic-directive
            "`\\sw+\\>"
          (vlog-regexp-wrap (vlog-regexp-opt vlog-indent-directives nil)))))

(defun vlog-indent-level-at-pos (&optional pos)
  "Calculate line's indentation level at point POS.
Return nil if failed, and a indentation column if done."
  (let ((pt (if pos pos (point))))
    (if (not (and (numberp pt)
                  (>= pt (point-min))
                  (<= pt (point-max))))
        0
      (save-excursion
        (goto-char pt)
        (beginning-of-line)
        (skip-chars-forward " \t")
        (if (= (point) (line-end-position)) 0
          (current-column))))))

(defun vlog-indent-line (&optional show-info-only)
  "Indentation function in vlog-mode. Set as `indent-line-function'.
If SHOW-INFO-ONLY is non-nil, do no indentation, just message indent info."
  (interactive "P")
  (let* ((info (vlog-indent-figure-out))
         (prev (car info))      ;; syntactic type of previous line
         (icol (nth 1 info))    ;; indent column of previous line
         (curr (nth 2 info))    ;; syntactic type of current line
         (isym nil))            ;; indent symbol
    (setq isym
          (cond
           ;; input [1:0] i_port; etc.
           ((eq prev 'defun) nil)
           ;; if/for/repeat/while/wait (...)
           ((or (eq prev 'contl-paren-cond)
                (eq prev 'contl-else))
            (if (eq curr 'begin)
                'vlog-indent-level-block
              'vlog-indent-level-cond))
           ;; in parenthesis
           ((eq prev 'inparen)
            (unless vlog-indent-align-port-list-to-paren
              (save-excursion
                (beginning-of-line)
                (skip-chars-forward " \t" (line-end-position))
                (backward-up-list)
                (beginning-of-line)
                (skip-chars-forward " \t" (line-end-position))
                (if (and (looking-at
                          "\\(\\sw+\\)\\s-+\\(#\\s-*([^)]+)\\)*\\s-*\\(\\sw+\\)\\s-*(")
                         (not (string-match vlog-indent-paren-cond-signs-re (match-string 3))))
                    (list 'abs vlog-indent-level-port-list)))))
           ((or (eq prev 'else)
                (eq prev 'beh)
                (eq prev 'block-end))
            nil)
           ((eq prev 'case)
            'vlog-indent-level-case-inside)
           ((or (eq prev 'block)
                (eq prev 'contl-block-begin))
            'vlog-indent-level-block-inside)
           ((or (eq prev 'contl-common)
                (eq prev 'contl-paren-normal))
            (if (eq curr 'begin)
                'vlog-indent-level-block
              'vlog-indent-level-default))
           ((eq prev 'contl-paren-at-beh)
            (if (eq curr 'begin)
                'vlog-indent-level-block-beh
              'vlog-indent-level-beh))
           ((eq prev 'contl-comma) nil)
           ((eq prev 'contl-branch)
            'vlog-indent-level-case-branch-inside)
           ((eq prev 'none) (list 'abs 0))
           (t (if (numberp icol)
                  nil
                'none))))
    (if show-info-only
        (message (format "%s"
                         (list (cons prev (if isym isym 'no-indent)) icol)))
      (unless (eq isym 'none)
        (vlog-indent-do
         (if (and isym (listp isym))
             (if (numberp (cadr isym)) (cadr isym) 0)
           (+ icol (or (symbol-value isym) 0)))))
      (when vlog-align-do-align-with-indent
        (vlog-align-line prev)))
    (cons prev isym)))

(defun vlog-indent-do (icol)
  "Indent current line to ICOL."
  (let ((skip (or (looking-back "^[ \t]+")
                  (looking-back "^$")
                  (looking-at "\\S-"))))
    (save-excursion
      (beginning-of-line)
      (skip-chars-forward " \t" (line-end-position))
      (unless (= (current-column) icol)
        (delete-horizontal-space)
        (indent-to icol)))
    (if (and skip
             (looking-at "^[ \t]+"))
        (skip-chars-forward " \t"))))

(defun vlog-indent-figure-out ()
  "Figure out the indent type of current line. Return (pinfo picol cinfo).
Possible types are:
'none
'scomment
'inparen
'directive
'contl-paren-cond
'contl-paren-at-beh
'contl-paren-at-delay
'contl-paren-at-nil
'contl-paren-normal
'contl-branch
'contl-block-begin
'contl-comma
'contl-else
'contl-common
'else
'case
'block
'block-end
'beh
'defun
'defun-end
"
  (save-excursion
    (beginning-of-line)
    (let* ((begin (point))
           (limit (save-excursion
                    (if (vlog-re-search-backward
                         vlog-indent-calc-begs-re (point-min) t)
                        (point)
                      (point-min))))
           (pinfo nil) ;; syntactic info of previous line
           (cinfo nil) ;; syntactic info of current line
           (picol nil) ;; indent column of previous line
           (pline nil))
      (skip-syntax-forward "\\s-")
      (if (looking-at "\\(begin\\)\\|\\(fork\\)")
          (setq cinfo 'begin)
        (setq cinfo 'normal))
      (setq pinfo
            (catch 'syntax
              ;;
              ;; if in star comments /* ... */, done with type 'scomment
              (when (vlog-in-star-comments-p (point))
                (throw 'syntax 'scomment))
              ;;
              ;; if inside a pair of parens, and user wants to indent them,
              ;; done with 'paren
              (when (vlog-in-parens-p)
                (condition-case nil (backward-up-list 1) (error nil))
                (forward-char 1)
                (if (re-search-forward "\\S-" (line-end-position) t)
                    (setq picol (1- (current-column)))
                  (setq picol (save-excursion (end-of-line) (current-column))))
                (throw 'syntax 'inparen))
              ;;
              ;; directives
              (when (looking-at vlog-indent-directives-re)
                (setq picol 0)
                (throw 'syntax 'directive))
              ;;
              ;; search backward until we can figure it out
              ;; check syntacticlly previous line
              (setq pline (vlog-indent-look-back limit))
              (setq picol (cadr pline))
              (let ((ptype     (caar pline))
                    (ptype-str (cdar pline)))
                ;; we are now looking at bol
                (if ptype
                    ;; a continued line
                    (throw 'syntax
                           ;; 'paren 'branch 'block-beg 'normal
                           (cond
                            ((eq ptype 'paren)
                             ;; " @" ">@" "?@" "#@" "word_" "word?"
                             (cond
                              ;; if/for/repeat/while/wait (...)
                              ((string-match "\\(.*\\)_" ptype-str)
                               'contl-paren-cond)
                              ((or (string= ">@" ptype-str)
                                   (string= "?@" ptype-str))
                               'contl-paren-at-beh)
                              ((string= "#@" ptype-str)
                               'contl-paren-at-delay)
                              ((string= " @" ptype-str)
                               'contl-paren-at-nil)
                              (t
                               'contl-paren-normal)))
                            ((eq ptype 'branch)
                             (if (string-match vlog-indent-special-beg-daily-words-re ptype-str)
                                 'contl-block-begin
                               'contl-branch))
                            ((eq ptype 'block-beg)
                             'contl-block-begin)
                            ((eq ptype 'comma)
                             'contl-comma)
                            (t
                             (if (or (eq ptype 'else)
                                     (string= "else" ptype-str))
                                 'contl-else
                               'contl-common))))
                  ;; not a continued line
                  (if (looking-at "\\<else\\>")
                      (progn
                        (setq picol (vlog-indent-check-for-else limit))
                        (throw 'syntax 'else))
                    (let (result)
                      (setq result
                            (vlog-indent-check-for-newline ptype-str picol limit))
                      (setq picol (cdr result))
                      (throw 'syntax (car result))))))))
      (list pinfo picol cinfo))))

(defun vlog-indent-look-back (&optional limit)
  "Return t if current line is a continued line of previous line.
If LIMIT is non-nil, use it as search limit.

For a continued line, return a list (TYPE INDENT).  INDENT is
previous line's (more precisely \"syntacticlly previous line\",
because empty lines and comments are ignored) indentation colomn
while TYPE is a list describing previous line's syntactic type.

TYPE is a list, the car of TYPE is nil or non-nil.  If it's nil,
previous line is not close, so current line is a continued line;
If it's non-nil, it's a closed line, so current line is a new
line.  The cdr of TYPE is syntactic relative string (\"if\", \"while\"
e.g.) of previous line."
  (interactive)
  (let ((lim  (min (point) (if (numberp limit) limit (point-min))))
        (orig nil)
        (icol nil))
    (save-excursion
      ;; check previous line
      (if (= 0 (forward-line -1))
          (progn
            (end-of-line)
            ;; skip useless things backward from the end of previous line
            (vlog-skip-blank-and-useless-backward lim)
            (setq icol (vlog-indent-level-at-pos))
            ;; now we're at the end of line, check the last char first:
            (cond
             ;;-- a `;' is the end of everything in verilog, [CLOSED]
             ((= (preceding-char) ?\;)
              (list (cons nil ";") icol))

             ;;-- we're after a `)', so find the corresponding `(' and make
             ;;-- decision with that one.
             ((= (preceding-char) ?\))
              (setq orig (point))
              (condition-case nil (backward-list) (error nil))
              (if (or (= orig (point))
                      (< (point) lim))
                  ;; the other paren not found in our scope, [CLOSED]
                  (list (cons nil ")") icol)
                ;; the other paren found, and now we're looking at it
                ;; if `xxx @(...)', end of a sexp
                ;; if `xxx #(...)', not an end
                ;; if others, not an end
                (setq icol (vlog-indent-level-at-pos))
                (vlog-skip-blank-and-useless-backward)
                (let ((sign (preceding-char))
                      (word nil))
                  ;; something word sign (......) parsed; We're looking at `('
                  (cond
                   ;;
                   ;; `@' exists before (...)
                   ((= sign ?\@)
                    ;; check what's before the `@', result stored in word
                    (setq word
                          (save-excursion
                            (backward-char 1)
                            (re-search-backward "\\(\\s-\\|^\\)\\(\\sw+\\)"
                                                (line-beginning-position) t)
                            (match-string-no-properties 2)))
                    (cond
                     ;; `<BLANKS> @ (xxx...)', [OPEN]
                     ((not (stringp word))
                      (list (cons 'paren " @") icol))
                     ;; `initial @ (xxx...)' or `always @ (xxx...)', [OPEN]
                     ((or (string= word "always")
                          (string= word "initial"))
                      (list (cons 'paren ">@") icol))
                     ;; `repeat @ (xxx...)', [ClOSED]
                     ((string= word "repeat")
                      (list (cons nil "r@") icol))
                     ;; `<Unrecognized> @ (xxx...)', [OPEN]
                     (t
                      (list (cons 'paren "?@") icol))))
                   ;;
                   ;; `#' exists before (...), [OPEN]
                   ((= sign ?\#) (list (cons 'paren "#") icol))
                   ;;
                   ;; anything else before (...), we're looking at `('
                   (t
                    (setq word
                          (save-excursion
                            (re-search-backward "\\(\\s-\\|^\\)\\(\\sw+\\)"
                                                (line-beginning-position) t)
                            (match-string-no-properties 2)))
                    (unless (stringp word) (setq word ""))
                    ;; something like `if (...)' or `while (...)'
                    (if (string-match vlog-indent-paren-sexp-signs-re word)
                        ;; if/for/repeat/while/wait (...), OPEN
                        (list (cons 'paren (concat word "_")) icol)
                      (if (string-match "case[xz]?" word)
                          ;; case[xz]? (...), [CLOSED]
                          (list (cons nil "case") icol)
                        ;; dummies (...), [OPEN]
                        (list (cons 'paren (concat (if (stringp word) word " ") "?")) icol))))))))

             ;;-- for Verilog 2000
             ((and vlog-mode-v2k-enabled
                   (= (preceding-char) ?\*))
              (save-excursion
                (backward-char 1)
                (if (looking-back "always\\s-+@")
                    (list (cons 'paren ">@") icol)
                  (list (cons 'nil "") icol))))

             ;;-- beginning of buffer
             ((bobp)
              (list (cons nil "^") 0))

             ;;-- comma
             ((= (preceding-char) ?\,)
              (when (vlog-re-search-backward vlog-decl-type-words-re
                                             (line-beginning-position) t)
                (goto-char (match-end 0))
                (vlog-re-search-forward "\\]" (line-end-position) t)
                (skip-chars-forward " \t" (line-end-position))
                (setq icol (current-column)))
              (list (cons 'comma ",") icol))

             ;;-- not after `;' or `)'
             (t
              ;; we're at the end of syntacticlly previous line
              (if (vlog-re-search-backward vlog-indent-special-end-daily-words-re
                                           (line-beginning-position) t)
                  ;; in case we are at "BRANCH: begin xxx = yyy; end"
                  (if (string= (match-string-no-properties 0) "else")
                      (list (cons 'else "") icol)
                    (vlog-indent-goto-block-beg lim (match-string-no-properties 0))
                    (list (cons nil "}") icol))
                (let ((word   nil)
                      (column nil)
                      (lim (line-beginning-position))
                      (tag    nil))
                  (save-excursion
                    (if (vlog-re-search-backward    ;; consider `begin/fork:tag' cases
                         "^\\(\\s-*\\)\\(\\S-+\\)\\s-*\\(:\\)\\s-*\\(\\S-+\\)*" lim t)
                        (progn
                          (setq word   (match-string-no-properties 2))
                          (setq column (match-end 3))
                          (setq tag    (match-string-no-properties 4)))
                      (re-search-backward
                       "\\<\\(\\sw+\\)" (line-beginning-position) t)
                      (setq word (match-string-no-properties 1))))
                  (unless (stringp word) (setq word ""))
                  (if column
                      (if tag
                          (if (or (string= word "begin")
                                  (string= word "fork"))
                              ;; `begin/fork:tag', [CLOSED]
                              (list (cons 'block-beg (concat "{" word)) icol)
                            ;; `dummy:tag', maybe branch, [OPEN]
                            (list (cons 'branch tag) icol))
                        ;; `dummy:', maybe branch, [OPEN]
                        (list (cons 'branch (concat word ":")) icol))
                    ;; no identifier
                    (cond
                     ((or (string-match vlog-indent-special-beg-daily-words-re word)
                          (string-match vlog-indent-special-beg-scarce-words-re word))
                      (list (cons 'block-beg "{") icol))
                     ((or (string-match vlog-indent-special-end-daily-words-re word)
                          (string-match vlog-indent-special-end-scarce-words-re word))
                      (list (cons nil "}") icol))
                     (t (list (cons 'normal word) icol)))))))))
        (list (cons nil "^") 0)))))

(defun vlog-indent-check-for-else (limit)
  "Check matching `if' for current `else'.  Return indentation of match `if'.
If matching `if' not found within limit, icol is 0.

If `vlog-indent-align-else-to-if' is non-nil, align `else' to `if'."
  (save-excursion
    (let ((icol 0))
      (when (vlog-indent-goto-match-if limit)
        (unless vlog-indent-align-else-to-if
          (beginning-of-line)
          (skip-chars-forward " \t"))
        (setq icol (current-column)))
      icol)))

(defun vlog-indent-check-for-newline (type-str picol limit)
  "Check syntax and indentation for current common new line."
  (let ((type        nil)
        (icol        picol)
        (search-back t)
        token)
    ;; type-str may be: `case' `r@' `)' `^' `}word' `;'
    (cond
     ((string= type-str "case")
      (setq type 'case))
     ((string= type-str "r@")
      (setq type 'block))
     ((or (string= type-str ")")
          (string= type-str "^"))
      (setq type 'none
            icol 0))
     (t
      (if (looking-at "\\(end.*\\)\\|\\(join\\)")
          (if (vlog-indent-goto-block-beg limit (match-string-no-properties 0))
              (setq icol (vlog-indent-level-at-pos)
                    type 'block-end)
            (setq type 'none))
            ;;(throw 'done nil))
        (catch 'done
          (while t
            (if (and search-back
                     (vlog-re-search-backward vlog-indent-words-re (point-min) t)
                     (setq search-back t))
                (cond
                 ;; initial/always
                 ((looking-at vlog-indent-beh-words-re)
                  (setq type 'beh
                        icol (vlog-indent-level-at-pos))
                  (throw 'done t))
                 ;; module/endmodule ...
                 ((looking-at vlog-indent-defun-words-re)
                  (setq icol (vlog-indent-level-at-pos))
                  (if (looking-at "end")
                      (setq type 'defun-end)
                    (setq type 'defun))
                  (throw 'done t))
                 ;; begin/fork/function/task ...
                 ((looking-at vlog-indent-block-beg-words-re)
                  (if (looking-at "case[xz]?")
                      (setq type 'case)
                    (setq type 'block))
                  (setq icol (vlog-indent-level-at-pos))
                  (throw 'done t))
                 ;; end/join/endfunction/endtask ...
                 ((looking-at vlog-indent-block-end-words-re)
                  (setq token (match-string-no-properties 0))
                  (if (vlog-indent-goto-block-beg limit token)
                      (unless (or (string= token "end") (string= token "join"))
                        (setq search-back nil))
                    (setq type 'none)
                    (throw 'done nil))))
              (setq type 'none
                    icol 0)
              (throw 'done t)))))))
    (cons type icol)))

(defun vlog-indent-goto-block-beg (limit &optional end-word)
  "Goto the beginning of a block.  Make sure you're looking at
the `end-word' of a block when you call this function."
  (let ((level 1)
        ;; look for an ender if parameter END-WORD is not given
        (ender (if (stringp end-word)
                   end-word
                 (save-excursion
                   (looking-at "\\<\\sw+\\>")
                   (match-string-no-properties 0))))
        type beger stone)
    (if (and (stringp ender)
             (string-match
              ;; end<case casex casez function task table specify generate
              ;; module config> join
              vlog-indent-block-end-words-re
              ender))
        ;; what are we looking for?
        (progn
          (cond
           ;; `end'
           ((string= ender "end") (setq beger "begin"))
           ;; `join'
           ((string= ender "join") (setq beger "fork"))
           ;; `endfoo'
           (t (setq type (if (string= ender "endcase") 'case 'non-nested))
              (setq beger (if (eq type 'case)
                              "case[xz]?"
                            (substring ender 3)))))
          (setq beger (concat "\\<" beger "\\>"))
          (setq stone (concat "\\(" beger "\\)\\|\\(\\<" ender "\\>\\)"))
          ;; now let's search the beginning
          (if (eq type 'non-nested)
              ;; function/generate/specifu/task/table, so no recursive search
              (vlog-re-search-backward beger limit t)
            (catch 'done
              (while t
                (if (vlog-re-search-backward stone limit t)
                    (if (match-end 2)
                        (setq level (1+ level))
                      (setq level (1- level))
                      (when (= 0 level) (throw 'done t)))
                  (throw 'done nil))))))
      (message "`%s' does not seem like a block end" ender)
      nil)))

(defun vlog-indent-goto-block-end (limit &optional beg-word)
  "Goto the end of a block.  Make sure you're looking at the
`beg-word' of a block when you call this function."
  (let ((level 1)
        ;; look for an ender if parameter END-WORD is not given
        (beger (if (stringp beg-word)
                   beg-word
                 (and (looking-at "\\<\\sw+\\>")
                      (match-string-no-properties 0))))
        type ender stone)
    (if (and (stringp beger)
             (string-match
              ;; begin fork case casex casez function task table specify
              ;; generate module macromodule config
              vlog-indent-block-beg-words-re
              beger))
        (progn
          ;; skip this one first
          (forward-word 1)
          ;; what are we looking for?
          (cond
           ;; `begin'
           ((string= beger "begin") (setq ender "end"))
           ;; `fork'
           ((string= beger "fork") (setq ender "join"))
           ;; `other'
           (t (setq type (if (string-match "\\(case\\)[xz]?" beger)
                             'case
                           'non-nested)
                    ender (concat "end" (or (match-string 1 beger)
                                            beger)))))
          (setq ender (concat "\\<" ender "\\>"))
          (setq stone (concat "\\(\\<" beger "\\>\\)\\|\\(" ender "\\)"))
          ;; now let's search the beginning
          (if (eq type 'non-nested)
              ;; function/generate/specifu/task/table, so no recursive search
              (vlog-re-search-forward ender limit t)
            (catch 'done
              (while t
                (if (vlog-re-search-forward stone limit t)
                    (if (match-end 1)
                        (setq level (1+ level))
                      (setq level (1- level))
                      (when (= 0 level) (throw 'done t)))
                  (throw 'done nil))))))
      (message "`%s' does not seem like a block beginning" beger)
      nil)))

(defun vlog-indent-goto-match-if (limit)
  "Parse code backward, find match `if' for current `else'.
Striding across begin/fork/case[xz]? ... end/join/endcase blocks.
Return t if `if' is found, nil if limit met or match `if' not
found.  Make sure you're looking at `else' when you call this
function."
  (let ((level-else 1)
        (pos (point))
        result)
    (setq result
          (catch 'found
            (while t
              (if (vlog-re-search-backward
                   ;;      2:if       3:else       4:end 5:endcase        6:join
                   "\\<\\(\\(if\\)\\|\\(else\\)\\|\\(end\\(case\\)?\\)\\|\\(join\\)\\)\\>"
                   limit t)
                  (cond
                   ;; `if' found
                   ((match-end 2)
                    (when (= 0 (setq level-else (1- level-else)))
                      (throw 'found t)))
                   ;; `else' found
                   ((match-end 3)
                    (setq level-else (1+ level-else)))
                   ;; here come troubles ...
                   (t
                    (let ((nest 1)
                          regex)
                      (cond
                       ;; `end' found
                       ((and (match-end 4) (not (match-end 5)))
                        (setq regex "\\<\\(begin\\)\\|\\(end\\)\\>"))
                       ;; `endcase' found
                       ((match-end 5)
                        (setq regex "\\<\\(case[xz]?\\)\\|\\(endcase\\)\\>"))
                       ;; `join' found
                       ((match-end 6)
                        (setq regex "\\<\\(fork\\)\\|\\(join\\)\\>")))
                      (catch 'skip
                        (while t
                          (if (vlog-re-search-backward regex limit t)
                              (cond
                               ((match-end 1) ;; starter
                                (setq nest (1- nest))
                                (if (= 0 nest) (throw 'skip t)))
                               ((match-end 2) ;; ender
                                (setq nest (1+ nest))))
                            (throw 'found nil)))))))
                (throw 'found nil)))))
    (if result t
      (goto-char pos) nil)))

(defun vlog-indent-this-block ()
  "Indent the current block if we're looking at a block beg/end, works with:
begin/end, fork/join, case/endcase, <block>/end<block>, ..."
  (interactive)
  (save-excursion
    (let ((pt (point))
          (word (vlog-lib-word-atpt nil nil 'beg))
          (work t)
          dst)
      (if (string-match vlog-indent-block-beg-words-re word)
          (progn (setq dst (point))
                 (vlog-indent-goto-block-end (point-max) word))
        (if (string-match vlog-indent-block-end-words-re word)
            (progn (setq dst (point))
                   (vlog-indent-goto-block-beg (point-min) word))
          (if (string= "" word)
              (message "We're not on the beg/end of a block")
            (message "`%s' is not a beg/end of a block." word))
          (goto-char pt)
          (setq work nil)))
      (and work
           (indent-region (min (point) dst) (max (point) dst))))))

(provide 'vlog-indent)

;;; vlog-indent.el ends here
