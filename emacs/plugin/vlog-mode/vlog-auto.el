;;; vlog-auto.el --- do automations for vlog-mode

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

(defgroup vlog-auto nil
  "Customizations for code automation."
  :group 'verilog)

(defcustom vlog-auto-sense-abandon-old-list t
  "If non-nil, use the new calculated sensitive list instead of
the old one. If nil, only add new signals if any, do no removal."
  :type  'boolean
  :group 'vlog-auto)

(defcustom vlog-auto-sense-refill-old-list t
  "If non-nil, refill the original sensitive list while adding
new signals into it.  If nil, leave the original sensitive list
untouched while adding new signals into sensitive list; "
  :type  'boolean
  :group 'vlog-auto)

(defcustom vlog-auto-sense-ignore-error t
  "If non-nil, ignore the parsing error and use partial result to
update sensitive list.  If nil, any error during the parsing
process will stop auto sense."
  :type  'boolean
  :group 'vlog-auto)

(defcustom vlog-auto-sense-use-comma t
  "Verilog 2000 allows using comma to seperate signals in
sensitive lists, set this to non-nil to use this feature.  Note
that it works only when Verilog 2000 support has been turned on."
  :type  'boolean
  :group 'vlog-auto)

(defvar vlog-auto-rvalue-list nil
  "DO NOT touch me.")
(make-variable-buffer-local 'vlog-auto-rvalue-list)

(defun vlog-auto-sense ()
  "Make sensitive lists for all automated always blocks."
  (interactive)
  (let ((orig (point-marker)))
    (goto-char (point-min))
    ;; search and serve the AUTOs one by one, use `vlog-auto-sense-this-block'
    (while (and (re-search-forward vlog-lib-auto-keyword-re (point-max) t)
                (string= "sense" (match-string 2)))
      (vlog-auto-sense-this-block))
    (goto-char (marker-position orig))))

(defun vlog-auto-sense-update-this-block ()
  "Make sensitive lists for current always block."
  (interactive)
  (let ((orig (point-marker)))
    (if (not (vlog-re-search-backward "always" (point-min) t))
        (message "No always block found.")
      (skip-chars-backward " \t\n")
      (beginning-of-line)
      (if (not (looking-at "^//\\s-+auto sense\\s-+//\\s-*$"))
          (message (concat "This block is not auto-sensed. "
                           "Add \"// auto sense //\" before \"always\" first."))
        (vlog-auto-sense-this-block)))
    (goto-char (marker-position orig))))

(defun vlog-auto-sense-this-block ()
  "Make the sensitive list for current always block."
  ;; we're now looking at the end of `// auto sense //'
  (vlog-skip-blank-and-useless-forward)
  (let ((sep (if (and vlog-mode-v2k-enabled
                      vlog-auto-sense-use-comma)
                 "," " or"))
        beg end old-list new-list col sigs)
    ;; get current sensitive list, store it in `old-list'
    (when (and (looking-at "always")
               (setq beg (vlog-re-search-forward "(" (point-max) t))
               (setq end (save-excursion
                           (vlog-re-search-forward ")" (point-max) t))))
      (setq col (current-column))
      (while (vlog-re-search-forward "\\<\\(\\sw+\\)\\>" end t)
        (add-to-list 'old-list (match-string-no-properties 1) t))
      (setq old-list (remove "or" old-list))
      ;; step over parens after `always', parse our new list
      (save-excursion
        (goto-char end)
        ;; get the sensitive list
        (setq sigs (vlog-auto-get-always-block-signals)))
      ;; report error
      (and (memq (car sigs) '(err eob))
           (vlog-auto-output-parser-err (cdr sigs)))
      ;; generate new list
      (setq new-list nil)
      ;; remove parameters
      (let ((params (vlog-lib-get-module-parameters)))
        (dolist (sig vlog-auto-rvalue-list)
          (unless (or (assoc sig params)
                      (if vlog-auto-sense-abandon-old-list nil
                        (vlog-lib-str-memq sig old-list)))
            (push sig new-list))))
      ;; insert sensitive list
      (goto-char (1- end))
      (if old-list
          (progn
            ;; fill/refill the new/old list
            (when (or vlog-auto-sense-abandon-old-list
                      vlog-auto-sense-refill-old-list)
              (kill-region beg (1- end))
              (vlog-auto-sense-fill-sensitive-list
               (if vlog-auto-sense-abandon-old-list new-list old-list)))
            ;; append new-list if `vlog-auto-sense-abandon-old-list' is nil
            (when (and (not vlog-auto-sense-abandon-old-list) new-list)
              (insert sep "\n")
              (indent-to-column col)
              (insert "// ***** Added by vlog-auto ***** //\n")
              (indent-to-column col)
              (vlog-auto-sense-fill-sensitive-list new-list)))
        (vlog-auto-sense-fill-sensitive-list new-list)))))

(defun vlog-auto-sense-fill-sensitive-list (flist &optional fcolumn)
  "Fill the sensitive list FLIST, using fill-column FCOLUMN."
  (let ((icol (current-column))
        (col  (if (numberp fcolumn) fcolumn fill-column))
        (idx  1)
        (len  (length flist))
        (sep (if (and vlog-mode-v2k-enabled
                      vlog-auto-sense-use-comma)
                 "," " or"))
        firstp lastp)
    (when flist
      (dolist (sig flist)
        (setq firstp (= idx 1)
              lastp  (= idx len))
        (when (> (+ (current-column) (length sig) (if lastp 0 4)) col)
          (insert "\n")
          (indent-to-column icol))
        (insert (concat (if (or firstp (looking-back "^\\s-*")) "" " ")
                        sig (if lastp "" sep)))
        (setq idx (1+ idx))))))

(defun vlog-auto-get-always-block-signals ()
  "Parse current always block, find sensitive signals and store
them in `vlog-auto-rvalue-list'. Return parsing result, parser
state (nil if done and non-nil if failed) and buffer position
after parsing in a list '(RESULT STATE POSITION)."
  (let* ((stack   (list 'init))
         (inplace nil)
         xpect tokent result state)
    (setq vlog-auto-rvalue-list nil)
    (setq result (catch 'done
      (while stack
        (unless inplace
          ;; skip useless (spaces, directives, comments, delays) forward
          (when (eq 'beyond (vlog-skip-blank-and-useless-forward nil t))
            (throw 'done 'eob))
          ;; make syntax decision, if done, skip current token
          (unless (setq tokent (vlog-auto-get-current-token-type))
            (throw 'done 'err)))
        (setq inplace nil)
        ;; transfer to next state
        (cond
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'init)
          (if (memq tokent (list 'if 'case))
              (setq inplace t)
            (unless (memq tokent (list 'begin 'fork))
              (throw 'done 'err)))
          (setq stack (list tokent)))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((memq (car stack) (list 'if 'case))
          ;; parse and step over the parens following if/case[xz]
          (unless (vlog-auto-parse-values-inside-parens)
            (throw 'done 'err))
          (push (if (eq (car stack) 'if) 'pend 'xp-br) stack))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((memq (car stack) (list 'pend 'begin 'fork))
          (cond
           ((memq tokent (list 'common 'macro))
            (push 'xp-assign stack))
           ((memq tokent (list 'if 'case))
            (setq inplace t)
            (push tokent stack))
           ;; bigin/fork block begins
           ((memq tokent (list 'begin 'fork))
            (push tokent stack))
           ;; begin-end block ends
           ((and (eq tokent 'end)
                 (eq (cadr stack) 'begin))
            (pop stack)
            (setcar stack 'closed)
            (setq inplace t))
           ;; fork-join block ends
           ((and (eq tokent 'join)
                 (eq (cadr stack) 'fork))
            (pop stack)
            (setcar stack 'closed)
            (setq inplace t))
           ;; consider empty statements such as "if ; else ...."
           ((eq tokent 'semi)
            (push 'closed stack)
            (setq inplace t))
           (t (throw 'done 'err))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'xp-assign) ;; expecting assignment
          (unless (eq tokent 'assign)
            (throw 'done 'err))
          ;; find `;' and parse signals
          (let ((end (save-excursion
                       (vlog-re-search-forward ";" (point-max) t))))
            (unless end
              (throw 'done 'err))
            (vlog-auto-parse-values-in-region (point) end))
          (setcar stack 'closed)
          (setq inplace t))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'closed) ;; a statement is closed
          (cond
           ((not (cdr stack))
            (setq inplace t)
            (setq stack nil))
           ;; del preceding 'pend: xxx->pend->closed ==> xxx->closed
           ((memq (cadr stack) (list 'else 'pend))
            (setq inplace t)
            (setcdr stack (cddr stack)))
           ((eq (cadr stack) 'if)
            (pop stack)
            (setcar stack 'xp-else))
           ;; in begin/fork blocks
           ((memq (cadr stack) (list 'begin 'fork))
            (setcar stack 'pend))
           ;; a branch ends
           ((eq (cadr stack) 'branch)
            (pop stack)
            (setcar stack 'xp-br2))
           (t (throw 'done 'err))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'xp-br) ;; expecting branch
          (cond
           ((eq tokent 'column)
            (setcar stack 'branch)
            (push 'pend stack))
           ((memq tokent (list 'common 'digit 'comma))
            nil)
           (t (throw 'done 'err))))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'xp-br2) ;; expecting branch and endcase
          (if (not (eq tokent 'endcase))
              (setcar stack 'xp-br)
            ;; `endcase' found
            (unless (eq (cadr stack) 'case)
              (throw 'done 'err))
            (setq stack (cddr stack))
            (setq inplace t)
            (push 'closed stack)))
         ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
         ((eq (car stack) 'xp-else) ;; expecting else
          (if (eq tokent 'else)
              (progn
                (setcar stack 'else)
                (push 'pend stack))
            (setcar stack 'pend)
            (setq inplace t)))
         (t (throw 'done 'err))))
      (throw 'done t)))
    ;; erase result on errors, if `vlog-auto-sense-ignore-error' is nil.
    (and (memq result (list 'err 'eob))
         (not vlog-auto-sense-ignore-error)
         (setq vlog-auto-rvalue-list nil))
    ;; update state
    (setq state (if (eq (car stack) 'pend) (cadr stack) (car stack)))
    ;; return result
    (list result state (point))))

(defun vlog-auto-output-parser-err (errlst)
  "Output parser error."
  (let ((errstr "")
        (state (car errlst))
        (pos   (cadr errlst)))
    (cond
     ((eq state 'begin)
      (setq errstr (format "[Parsing error] at line %s (\"begin\" has no matching \"end\")."
                           (line-number-at-pos pos))))
     ((eq state 'xp-br)
      (setq errstr (format "[Parsing error] at line %s (\"case[xz]\" has no matching \"endcase\")."
                           (line-number-at-pos pos))))
     (t (setq errstr (format "[auto-sense error] at line %s (stack top: %s)."
                             (line-number-at-pos pos) state))))
    (message errstr)))

(defun vlog-auto-get-current-token-type ()
  "Decide the type of current toke, and then step over it.
Return t if done, and nil if errors encountered."
  (let (str pt)
    (cond
     ;; looking at a token
     ((looking-at "\\<\\(`\\|\\([0-9]+'\\)\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>")
      (setq str (match-string-no-properties 3))
      (goto-char (match-end 3))
      (if (match-end 1)
          (if (match-end 2)
              'digit
            ;; all directives have been skipped, so it must be a macro
            'macro)
        (cond
         ((string= "if"      str)          'if)
         ((string= "begin"   str)          'begin)
         ((string= "end"     str)          'end)
         ((string= "fork"    str)          'fork)
         ((string= "join"    str)          'join)
         ((string= "else"    str)          'else)
         ((string-match "^case[xz]?$" str) 'case)
         ((string= "endcase" str)          'endcase)
         ((string= "always"  str)          'always)
         (t                                'common))))
     ;; looking at `('
     ((looking-at "(")
      (forward-char 1)
      'lparen)
     ;; looking at an assignment
     ((looking-at "\\(<?=\\)\\([^=]\\|\\'\\)")
      (goto-char (match-end 1))
      'assign)
     ;; looking at `;'
     ((looking-at ";")
      (forward-char 1)
      'semi)
     ;; looking at `:'
     ((looking-at ":")
      (forward-char 1)
      'column)
     ;; looking at `,'
     ((looking-at ",")
      (forward-char 1)
      'comma)
     ;; maybe signal concatenation
     ((looking-at "{")
      (if (/= (point)       ;; look for the other }
              (save-excursion
                (progn (condition-case nil (forward-list) (error nil))
                       (setq pt (point)))))
          (progn
            (goto-char pt)
            'common)
        nil))
     (t nil))))

(defun vlog-auto-parse-values-inside-parens ()
  "Search paren pair, and parse signals inside it.
Signals are stored in `vlog-auto-rvalue-list'.
Return t if done, and nil if there are unbalanced parentheses."
  (let (beg end)
    (if (not (vlog-re-search-forward "(" (point-max) t)) nil
      (setq beg (point))
      (condition-case nil (up-list) (error nil))
      (if (/= ?\) (preceding-char)) nil
        (setq end (point))
        (vlog-auto-parse-values-in-region beg end)
        t))))

(defun vlog-auto-parse-values-in-region (beg end)
  "Parse and step over region from BEG to END for signals.
Signals are stored in `vlog-auto-rvalue-list'."
  (let (value ignorep)
    (goto-char beg)
    ;; any macro or number is ignored; hierarchical signals are not supported.
    (while (vlog-re-search-forward
            "\\<\\(`\\|\\([0-9]+'\\)\\)?\\([a-zA-Z_][a-zA-Z0-9_]*\\)\\>" end t)
      (setq value   (match-string-no-properties 3)
            ignorep (if (match-end 1) t nil))
      (when (not ignorep)
        (add-to-list 'vlog-auto-rvalue-list value)))
    (goto-char end)))

(provide 'vlog-auto)

;;; vlog-auto.el ends here
