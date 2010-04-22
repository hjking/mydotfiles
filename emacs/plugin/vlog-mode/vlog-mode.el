;;; vlog-mode.el --- a new major mode for editing verilog files

;; Copyright (C)  2004 Sun Yijiang <sunyijiang@gmail.com>

;; Author:     Sun Yijiang
;; Maintainer: Sun Yijiang
;; Inherited:  From verilog-mode.el (Michael McNamara <mac@verilog.com>)
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
(require 'font-lock)

(require 'vlog-lib)
(require 'vlog-indent)
(require 'vlog-skel)
(require 'vlog-auto)
(require 'vlog-signal)

;;+ variables, constants and faces ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup verilog nil
  "Customizations for vlog-mode."
  :group 'languages)

(defgroup vlog-mode nil
  "Customizations for vlog-mode."
  :group 'verilog)

(defgroup vlog-faces nil
  "vlog-mode faces settings."
  :group 'verilog)

(defvar vlog-mode-map nil
  "Keymap used in vlog-mode.")

(defvar vlog-mode-menu-map nil
  "Menu keymap for vlog-mode.")

(defcustom vlog-mode-make-keymap-hook nil
  "Normal hook that is run after `vlog-mode-map' is set.
You can add your own keymaps using this hook."
  :type    'hook
  :group   'vlog-mode)

(defcustom vlog-mode-auto-end-block t
  "If t, insert `end'/`join' after `begin'/`fork' automatically."
  :type  'boolean
  :group 'vlog-mode)

(defcustom vlog-mode-auto-end-module t
  "If t, insert `endmodule' after `module'/`macromodule' automatically."
  :type  'boolean
  :group 'vlog-mode)

(defcustom vlog-mode-double-comma-prefix nil
  "Prefix string to add after you typed double comma."
  :type  'string
  :group 'vlog-mode)

(defcustom vlog-mode-double-comma-suffix nil
  "Suffix string to add after you typed double comma."
  :type  'string
  :group 'vlog-mode)

(defvar vlog-mode-syntax-table nil
  "Syntax table used in `vlog-mode'.")

;; keyword sets and regexps --------------------------------------------------

;; --------- Keyword list for Verilog (Fromt IEEE 1364-1995 Standard) --------
;; always and assign begin buf bufif0 bufif1 case casex casez cmos deassign
;; default defparam disable edge else end endcase endmodule endfunction
;; endprimitive endspecify endtable endtask event for force forever fork
;; function highz0 highz1 if ifnone initial inout input integer join large
;; macromodule medium module nand negedge nmos nor not notif0 notif1 or output
;; parameter pmos posedge primitive pull0 pull1 pullup pulldown rcmos real
;; realtime reg release repeat rnmos rpmos rtran rtranif0 rtranif1 scalared
;; small specify specparam strong0 strong1 supply0 supply1 table task time
;; tran tranif0 tranif1 tri tri0 tri1 triand trior trireg vectored wait wand
;; weak0 weak1 while wire wor xnor xor

;; ------ Keyword list for Verilog 2000 (Fromt IEEE 1364-2005 Standard) ------
;; always and assign automatic begin buf bufif0 bufif1 case casex casez cell
;; cmos config deassign default defparam design disable edge else end endcase
;; endconfig endfunction endgenerate endmodule endprimitive endspecify
;; endtable endtask event for force forever fork function generate genvar
;; highz0 highz1 if ifnone incdir include initial inout input instance integer
;; join large liblist library localparam macromodule medium module nand
;; negedge nmos nor noshowcancelled not notif0 notif1 or output parameter pmos
;; posedge primitive pull0 pull1 pulldown pullup pulsestyle_onevent
;; pulsestyle_ondetect rcmos real realtime reg release repeat rnmos rpmos
;; rtran rtranif0 rtranif1 scalared showcancelled signed small specify
;; specparam strong0 strong1 supply0 supply1 table task time tran tranif0
;; tranif1 tri tri0 tri1 triand trior trireg unsigned use uwire vectored wait
;; wand weak0 weak1 while wire wor xnor xor

;; ----------------------- New keywords in Verilog 2000 ----------------------
;; automatic cell config design endconfig endgenerate generate genvar incdir
;; include instance liblist library localparam noshowcancelled
;; pulsestyle_ondetect pulsestyle_onevent showcancelled signed unsigned use
;; uwire

(defvar vlog-mode-keywordset-types
  '("buf" "bufif0" "bufif1" "cmos" "defparam" "event" "highz0" "highz1"
    "inout" "input" "integer" "large" "medium" "nmos" "output" "parameter"
    "pmos" "pull0" "pull1" "pullup" "pulldown" "rcmos" "real" "realtime"
    "reg" "rnmos" "rpmos" "rtran" "rtranif0" "rtranif1" "scalared" "small"
    "signed" "specparam" "strong0" "strong1" "supply" "supply0" "supply1"
    "time" "tran" "tranif0" "tranif1" "tri" "tri0" "tri1" "triand" "trior"
    "trireg" "vectored" "wand" "weak0" "weak1" "wire" "wor" "xnor" "xor")
  "Type keywords in verilog sources.")

(defvar vlog-mode-keywordset-types-v2k
  '("automatic" "localparam" "unsigned" "uwire")
  "Type keywords for Verilog 2000.")

(defvar vlog-mode-keywordset-structs
  '("initial" "always"
    "function" "task" "module" "primitive" "specify" "macromodule"
    "endfunction" "endtask" "endmodule" "endprimitive" "endspecify")
  "Structure keywords in verilog sources.")

(defvar vlog-mode-keywordset-structs-v2k
  '("config" "endconfig" "library"
    "pulsestyle_onevent" "pulsestyle_ondetect"
    "showcancelled" "noshowcancelled")
  "Additional structure keywords for Verilog 2000.")

(defvar vlog-mode-keywordset-keywords
  '("and" "assign" "begin" "case" "casex" "casez" "default" "deassign"
    "disable" "edge" "else" "end" "endcase" "endgenerate" "endtable" "for"
    "force" "forever" "fork" "generate" "if" "ifnone" "join" "nand" "negedge"
    "nor" "not" "notif0" "notif1" "or" "posedge" "repeat" "release" "table"
    "wait" "while")
  "Keywords in verilog sources.")

(defvar vlog-mode-keywordset-keywords-v2k
  '("design" "instance" "cell" "use" "liblist" "genvar" "include" "incdir")
  "Additional keywords for Verilog 2000.")

(defvar vlog-mode-keywordset-pragmas
  '("synopsys")
  "Pragma keywords in verilog sources.")

(defvar vlog-mode-keywordset-docs
  '("Fixme" "FIXME" "FixMe" "fixme"
    "Todo:" "TODO:" "ToDo:" "todo:"
    "Doc:" "DOC:" "doc:" "synthesis")
  "In-comments documentation keywords in verilog sources. Emacs
will highlight them with `vlog-mode-doc-face'.  To add you own,
just add new words to this list, and then don't forget to call
`vlog-mode-make-keywords' to rebuild regexps with the new list.")

(defcustom vlog-mode-keywords-use-generic-systask t
  "If non-nil, use system task keywords in `vlog-mode-keywordset-systasks'.
If nil, use generic system task keywords regexp."
  :group   'vlog-mode
  :type    'boolean)

(defvar vlog-mode-keywordset-systasks
  '("$bitstoreal" "$close" "$display" "$display" "$displayb" "$displayb"
    "$displayh" "$displayh" "$displayo" "$displayo" "$dist_chi_square"
    "$dist_erland" "$dist_exponential" "$dist_normal" "$dist_t"
    "$dist_uniform" "$dust_poisson" "$finish" "$hold" "$itor" "$monitor"
    "$monitorb" "$monitorh" "$monitoro" "$nochange" "$open" "$period"
    "$printtimescale" "$random" "$readmemb" "$readmemh" "$realtime"
    "$realtobits" "$recovery" "$rtoi" "$setup" "$setuphold" "$skew" "$stime"
    "$stop" "$strobe" "$strobeb" "$strobeh" "$strobeo" "$test$" "$time"
    "$timeformat" "$width" "$write" "$write" "$writeb" "$writeb" "$writeh"
    "$writeh" "$writeo" "$writeo")
  "System task names.")

(defvar vlog-mode-keywordset-systasks-v2k
  '("$signed" "$unsigned" "$ferror" "$fgetc" "$fgets" "$fflush" "$fread"
    "$fscanf" "$fseek" "$fsscanf" "$ftel" "$rewind" "$sformat" "$swrite"
    "$swriteb" "$swriteh" "$swriteo" "$ungetc" "$value$" "$removal" "$recrem"
    "$timeskew" "$fullskew" "$sdf_annotate" "$dumpports" "$dumpportsall"
    "$dumpportsoff" "$dumpportson" "$dumpportslimit" "$dumpportsflush")
  "System task names for Verilog 2000.")

(defvar vlog-mode-keywordset-types-regexp nil
  "Regexp of type keywords in verilog sources.")

(defvar vlog-mode-keywordset-structs-regexp nil
  "Regexp of structure keywords in verilog sources.")

(defvar vlog-mode-keywordset-keywords-regexp nil
  "Regexp of keywords in verilog sources.")

(defvar vlog-mode-keywordset-pragmas-regexp nil
  "Regexp of pragma keywords in verilog sources")

(defvar vlog-mode-keywordset-docs-regexp nil
  "Regexp of documentation keywords in verilog sources")

(defvar vlog-mode-keywordset-systasks-regexp nil
  "Regexp of system task name.")

(defvar vlog-mode-keywordset-systasks-regexp-generic
  "\\$[a-zA-Z][a-zA-Z0-9_]*"
  "Generic system task name regexp.")

(defvar vlog-mode-keywordset-number-regexp
  "\\<\\([\\-]?[0-9]+\\('[hdxboHDXBO][0-9a-fA-FxXzZ_]+\\)*\\)\\|\\([0-9.]+[eE]-?[0-9]+\\)\\>"
  "Regexp of number keywords in verilog sources.")

(defvar vlog-mode-keywordset-number-v2k-regexp
  "\\<\\([\\-]?[0-9]+\\('[sS]?[hdxboHDXBO][0-9a-fA-FxXzZ_]+\\)*\\)\\|\\([0-9.]+[eE]-?[0-9]+\\)\\>"
  "Regexp of number keywords in verilog sources for Verilog 2000.")

(defvar vlog-mode-keywordset-operator-regexp
  "[+-*/!~&|\\^<>=?:]"
  "Regexp of Operator keyword in verilog sources.")

(defvar vlog-mode-keywords-basic nil
  "Level 0 (basic) to highlight in vlog-mode.")

(defvar vlog-mode-keywords-medium nil
  "Level 1 (medium) keywords to highlight in vlog-mode.")

(defvar vlog-mode-keywords-max nil
  "Level 2 (maximum) keywords to highlight in vlog-mode.")

(defvar vlog-mode-v2k-enabled nil
  "DO NOT touch me.  Use `vlog-mode-enable-v2k'")

;; faces ---------------------------------------------------------------------
(defface vlog-mode-psl-tag-face
  '((((class color)
      (background dark))
     (:foreground "salmon"))
    (((class color)
      (background light))
     (:foreground "red"))
    (t (:italis t)))
  "Font lock mode face used to highlight psl tag."
  :group 'vlog-faces)

(defface vlog-mode-psl-content-face
  '((((class color)
      (background dark))
     (:foreground "pink"))
    (((class color)
      (background light))
     (:foreground "pink3"))
    (t (:italis t)))
  "Font lock mode face used to highlight psl contents."
  :group 'vlog-faces)

(defcustom vlog-mode-psl-tag-face 'vlog-mode-psl-tag-face
  "PSL tag face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-psl-content-face 'vlog-mode-psl-content-face
  "PSL content face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-type-face 'font-lock-type-face
  "Type face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-keyword-face 'font-lock-keyword-face
  "Keyword face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-systask-face 'font-lock-function-name-face
  "System task face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-struct-face 'font-lock-function-name-face
  "Structure face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-operator-face 'font-lock-variable-name-face
  "Operator face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-module-face 'font-lock-constant-face
  "Module, task, function and primitive face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-pragma-face
  (if (boundp 'font-lock-preprocessor-face)
      'font-lock-preprocessor-face
    'font-lock-constant-face)
  "Pragma face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-macro-face 'font-lock-constant-face
  "Macro face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-number-face 'font-lock-doc-face
  "Number face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-parameter-face 'font-lock-type-face
  "Parameter passing face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-doc-face 'font-lock-warning-face
  "Documentation face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

(defcustom vlog-mode-warning-face 'font-lock-warning-face
  "Warning face used in vlog-mode."
  :group 'vlog-faces
  :type  'variable)

;; options -------------------------------------------------------------------
(defcustom vlog-mode-auto-indent t
  "Toggle auto indentation when a new line is started."
  :group 'vlog-mode
  :type  'boolean)

(defcustom vlog-mode-auto-delete-empty-line t
  "Toggle auto deletion of empty lines when hit return."
  :group 'vlog-mode
  :type  'boolean)

(defcustom vlog-mode-auto-name-at-endmodule t
  "Toggle auto insertion of module name when feed the endmodule line."
  :group 'vlog-mode
  :type  'boolean)

(defcustom vlog-mode-endmodule-auto-name-prefix " // "
  "The string added before the module name after endmodule."
  :group 'vlog-mode
  :type  'string)

(defcustom vlog-mode-highlight-all-uppercase-words nil
  "Toggle highlighting of all-uppercase words."
  :group 'vlog-mode
  :type  'boolean)
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;+ vlog-mode ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode ()
  "Major mode for editing Verilog code."
  (interactive)
  ;;
  ;; major mode settings
  (kill-all-local-variables)
  (setq major-mode 'vlog-mode)
  (setq mode-name (if vlog-mode-v2k-enabled "Verilog-2000" "Verilog"))
  (vlog-mode-make-keymap)
  (use-local-map vlog-mode-map)
  (set (make-local-variable 'imenu-create-index-function)
       'vlog-imenu-create-index-function)
  ;;
  ;; set syntax table
  (make-local-variable 'vlog-mode-syntax-table)
  (unless (syntax-table-p vlog-mode-syntax-table)
    (setq vlog-mode-syntax-table (make-syntax-table))
    (vlog-mode-make-sytax-table vlog-mode-syntax-table))
  (set-syntax-table vlog-mode-syntax-table)
  ;;
  ;; set font-lock keywords
  (unless (and (stringp vlog-mode-keywordset-types-regexp)
               (stringp vlog-mode-keywordset-structs-regexp)
               (stringp vlog-mode-keywordset-keywords-regexp)
               (stringp vlog-mode-keywordset-pragmas-regexp)
               (stringp vlog-mode-keywordset-docs-regexp)
               (stringp vlog-mode-keywordset-systasks-regexp))
    (vlog-mode-make-keywords))
  (setq font-lock-defaults
        (list
         '(vlog-mode-keywords-basic vlog-mode-keywords-medium vlog-mode-keywords-max)
         nil nil nil nil))
  ;;
  ;; indentation
  (unless (and (stringp vlog-indent-directives)
               (stringp vlog-indent-paren-sexp-signs)
               (stringp vlog-indent-paren-cond-signs)
               (stringp vlog-indent-special-beg-daily-words)
               (stringp vlog-indent-special-beg-scarce-words)
               (stringp vlog-indent-special-end-daily-words)
               (stringp vlog-indent-special-end-scarce-words)
               (stringp vlog-indent-beh-words)
               (stringp vlog-indent-block-beg-words)
               (stringp vlog-indent-defun-words)
               (stringp vlog-indent-words)
               (stringp vlog-indent-calc-begs))
    (vlog-indent-make-regexps))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'vlog-indent-line)
  ;;
  ;; other settings
  (unless (stringp vlog-decl-type-words-re)
    (vlog-lib-make-regexp))
  (setq comment-start "//"))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ syntax and keymap ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-make-sytax-table (table)
  "Make syntax table for vlog-mode."
  (modify-syntax-entry ?\\ "\\"     table)
  (modify-syntax-entry ?+  "."      table)
  (modify-syntax-entry ?-  "."      table)
  (modify-syntax-entry ?=  "."      table)
  (modify-syntax-entry ?%  "."      table)
  (modify-syntax-entry ?<  "."      table)
  (modify-syntax-entry ?>  "."      table)
  (modify-syntax-entry ?&  "."      table)
  (modify-syntax-entry ?|  "."      table)
  (modify-syntax-entry ?`  "w"      table)
  (modify-syntax-entry ?_  "w"      table)
  (modify-syntax-entry ?#  "w"      table)  ;; add `#' for delay syntax.
  (modify-syntax-entry ?\' "w"      table)  ;; `'' is used in numbers.
  (modify-syntax-entry ?/  ". 124b" table)
  (modify-syntax-entry ?*  ". 23"   table)
  (modify-syntax-entry ?\n "> b"    table))

(defun vlog-mode-make-keymap ()
  "Make `vlog-mode-map', add menus, and run `vlog-mode-make-keymap-hook'."
  (unless (keymapp vlog-mode-map)
    (setq vlog-mode-map (make-sparse-keymap))
    (vlog-skel-setup-keymap)
    (define-key vlog-mode-map "\C-c\C-c" vlog-skel-map)
    (define-key vlog-mode-map "\C-m"     'vlog-mode-electric-return)
    ;; for debug use
    (define-key vlog-mode-map "\C-c\C-s"
      (lambda () (interactive)
        (message (format "%s" (vlog-indent-figure-out)))))
    (define-key vlog-mode-map "\C-ci"
      (lambda () (interactive) (vlog-indent-line t)))
    (define-key vlog-mode-map "\C-c\C-i" 'vlog-indent-this-block)
    (define-key vlog-mode-map (kbd "<C-M-SPC>") 'vlog-select-block-or-sexp)
    (define-key vlog-mode-map "\C-c\C-a" 'vlog-auto-sense)
    (define-key vlog-mode-map "\C-c\C-u" 'vlog-auto-sense-update-this-block)
    (define-key vlog-mode-map "\C-c\C-d" 'vlog-signal-trace-driver)
    (define-key vlog-mode-map "\C-c\C-n" 'vlog-signal-trace-driver-next)
    (define-key vlog-mode-map "\C-c "    'vlog-align-line)
    (define-key vlog-mode-map "\C-c{"    'vlog-goto-block-beg)
    (define-key vlog-mode-map "\C-c}"    'vlog-goto-block-end)
    (define-key vlog-mode-map "\C-c%"    'vlog-goto-block-match)
    (define-key vlog-mode-map "\C-c\C-h" 'vlog-skel-smart-header)
    (define-key vlog-mode-map "d"        'vlog-mode-electric-d)
    (define-key vlog-mode-map "e"        'vlog-mode-electric-e)
    (define-key vlog-mode-map "f"        'vlog-mode-electric-f)
    (define-key vlog-mode-map "k"        'vlog-mode-electric-k)
    (define-key vlog-mode-map "n"        'vlog-mode-electric-n)
    (define-key vlog-mode-map "b"        'vlog-mode-electric-b)
    (define-key vlog-mode-map "B"        'vlog-mode-electric-B)
    (define-key vlog-mode-map "h"        'vlog-mode-electric-h)
    (define-key vlog-mode-map "H"        'vlog-mode-electric-H)
    (define-key vlog-mode-map "D"        'vlog-mode-electric-D)
    (define-key vlog-mode-map "x"        'vlog-mode-electric-x)
    (define-key vlog-mode-map "X"        'vlog-mode-electric-X)
    (define-key vlog-mode-map ";"        'vlog-mode-electric-semi)
    (define-key vlog-mode-map " "        'vlog-mode-electric-space)
    (define-key vlog-mode-map ","        'vlog-mode-electric-comma)
    (define-key vlog-mode-map "\M-s"     'vlog-signal-smart-insert)
    (define-key vlog-mode-map [C-backspace]  'vlog-lib-hungry-back-delete)
    (define-key vlog-mode-map [f1]       'vlog-show-this-signal-width-echo)
    (dolist (cmd (list 'vlog-mode-electric-d 'vlog-mode-electric-e
                       'vlog-mode-electric-f 'vlog-mode-electric-k
                       'vlog-mode-electric-n 'vlog-mode-electric-b
                       'vlog-mode-electric-B 'vlog-mode-electric-h
                       'vlog-mode-electric-H 'vlog-mode-electric-D
                       'vlog-mode-electric-x 'vlog-mode-electric-X
                       'vlog-mode-electric-semi 'vlog-mode-electric-space
                       'vlog-mode-electric-comma))
      (put cmd 'delete-selection t))
    ;; Menus for vlog-mode
    (setq vlog-mode-menu-map (make-sparse-keymap "Verilog"))
    (define-key vlog-mode-map [menu-bar] (make-sparse-keymap))
    (define-key vlog-mode-map [menu-bar vlog-mode]
      (cons "Verilog" vlog-mode-menu-map))
    (define-key vlog-mode-menu-map [custom]
      '("Customize..." . vlog-customize))
    (define-key vlog-mode-menu-map [sep-custom] '("--")) ;; ------------------
    (define-key vlog-mode-menu-map [comment-region]
      '("Comment/Uncomment Region" . comment-dwim))
    (define-key vlog-mode-menu-map [indent-region]
      '("Indent Region" . indent-region))
    (define-key vlog-mode-menu-map [align-line]
      '("Align Current Line" . vlog-align-line))
    (define-key vlog-mode-menu-map [indent-line]
      '("Indent Current Line" . indent-for-tab-command))
    (define-key vlog-mode-menu-map [sep-indent] '("--")) ;; ------------------
    (define-key vlog-mode-menu-map [block-mark]
      '("Select This Block" . vlog-select-block-or-sexp))
    (define-key vlog-mode-menu-map [block-indent]
      '("Indent This Block" . vlog-indent-this-block))
    (define-key vlog-mode-menu-map [block-match]
      '("Goto Block Match" . vlog-goto-block-match))
    (define-key vlog-mode-menu-map [block-end]
      '("Goto Block End" . vlog-goto-block-end))
    (define-key vlog-mode-menu-map [block-beg]
      '("Goto Block Beg" . vlog-goto-block-beg))
    (define-key vlog-mode-menu-map [match-if]
      '("Goto Matching if" . vlog-goto-block-match))
    (define-key vlog-mode-menu-map [sep-block] '("--")) ;; ------------------
    (define-key vlog-mode-menu-map [show-signal-width]
      '("Show Width of Current Signal" . vlog-show-this-signal-width-echo))
    (define-key vlog-mode-menu-map [hungry-delete]
      '("Hungry Back Delete" . vlog-lib-hungry-back-delete))
    (define-key vlog-mode-menu-map [trace-next]
      '("Trace Signal Driver (Next)" . vlog-signal-trace-driver-next))
    (define-key vlog-mode-menu-map [trace-driver]
      '("Trace Signal Driver" . vlog-signal-trace-driver))
    (define-key vlog-mode-menu-map [sep-insert] '("--")) ;; ------------------
    (define-key vlog-mode-menu-map [update-sense]
      '("Update Sensitive List" . vlog-auto-sense-update-this-block))
    (define-key vlog-mode-menu-map [auto-sense]
      '("Generate Sensitive List" . vlog-auto-sense))
    (define-key vlog-mode-menu-map [smart-insert]
      '("Insert a Signal Assignment" . vlog-signal-smart-insert))
    (define-key vlog-mode-menu-map [always-comb]
      '("Insert Comb-logic Always Block" . vlog-skel-smart-always-comb))
    (define-key vlog-mode-menu-map [always-edge]
      '("Insert Edge-event Always Block" . vlog-skel-smart-always-edge))
    (define-key vlog-mode-menu-map [add-header]
      '("Add a Header for Current Buffer" . vlog-skel-smart-header)))
  (run-hooks 'vlog-mode-make-keymap-hook))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ font-lock highlighting ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-make-keywords ()
  "Make keywords of different font-lock decoration levels,
using `vlog-mode-keywordset-types', `vlog-mode-keywordset-structs',
`vlog-mode-keywordset-keywords', `vlog-mode-keywordset-pragmas',
`vlog-mode-keywordset-docs' and `vlog-mode-keywordset-systasks'.
Refer to `font-lock-maximum-decoration' for more infomation.

To override or modify default keywords, set `vlog-mode-keywordset-*' and then
call me."
  ;;
  ;; make keyword-regexps first
  (dolist (regexp
           '((vlog-mode-keywordset-types . vlog-mode-keywordset-types-regexp)
             (vlog-mode-keywordset-structs . vlog-mode-keywordset-structs-regexp)
             (vlog-mode-keywordset-keywords . vlog-mode-keywordset-keywords-regexp)
             (vlog-mode-keywordset-pragmas . vlog-mode-keywordset-pragmas-regexp)
             (vlog-mode-keywordset-docs . vlog-mode-keywordset-docs-regexp)
             (vlog-mode-keywordset-systasks . vlog-mode-keywordset-systasks-regexp)))
    (set (cdr regexp)
         (vlog-regexp-opt (symbol-value (car regexp)) nil)))
  ;;
  ;; set basic keywords
  (setq vlog-mode-keywords-basic
        (list
         ;; keywords
         (cons (vlog-regexp-wrap vlog-mode-keywordset-keywords-regexp)
               vlog-mode-keyword-face)
         ;; types
         (cons (vlog-regexp-wrap vlog-mode-keywordset-types-regexp)
               vlog-mode-type-face)
         ;; system tasks
         (cons (vlog-regexp-wrap
                (if vlog-mode-keywords-use-generic-systask
                    vlog-mode-keywordset-systasks-regexp-generic
                  (concat "\\$\\(" vlog-mode-keywordset-systasks-regexp "\\)")))
               vlog-mode-systask-face)
         ;; structures
         (cons (vlog-regexp-wrap vlog-mode-keywordset-structs-regexp)
               vlog-mode-struct-face)))
  ;;
  ;; set medium keywords
  (setq vlog-mode-keywords-medium
        (append vlog-mode-keywords-basic
                (list
                 ;; operators
                 (list vlog-mode-keywordset-operator-regexp
                       (list 0 vlog-mode-operator-face))
                 ;; module/task/primitive definitions
                 (list "\\<\\(\\(macro\\)?module\\|primitive\\|task\\)\\>\\s-*\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 3 vlog-mode-module-face))
                 ;; function definitions
                 (list "\\<function\\>\\s-+\\(integer\\|real\\(?:time\\)?\\|time\\)\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 2 vlog-mode-module-face t))
                 (list "\\<function\\>\\s-+\\(\\[[^]]+\\]\\)\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-keyword-face)
                       (list 2 vlog-mode-module-face 'append))
                 (list "\\<function\\>\\s-+\\(\\sw+\\)"
                       (list 1 vlog-mode-module-face 'append))
                 ;; autos
                 (list vlog-lib-auto-keyword-re
                       (list 1 vlog-mode-doc-face t)
                       (list 2 vlog-mode-module-face t)))
                ;; Verilog 2000 support
                (when vlog-mode-v2k-enabled
                  (list
                   (list "\\<\\(config\\|library\\)\\>\\s-+\\(\\sw+\\)"
                         (list 2 vlog-mode-module-face 'append))
                   ;; liblist
                   (list "\\<liblist\\>\\(\\(\\s-+\\sw+\\)+\\)"
                         (list 1 vlog-mode-module-face 'append))
                   ;; design, instance
                   (list "\\<\\(design\\|instance\\)\\>\\s-+\\([a-zA-Z][a-zA-Z0-9_.]*\\)"
                         (list 2 vlog-mode-struct-face 'append))))))
  ;;
  ;; set maximum keywords
  (setq vlog-mode-keywords-max
        (append vlog-mode-keywords-medium
                (list
                 ;; pragmas
                 (list (concat "//\\s-*\\(" vlog-mode-keywordset-pragmas-regexp
                               "\\)\\>\\s-*\\([^/\n]?+\\)\\(//.*\\)*$")
                       (list 1 vlog-mode-pragma-face t)
                       (list 2 vlog-mode-type-face t))
                 ;; macro definitions/uses
                 (list "`\\s-*[A-Za-z][A-Za-z0-9_]*"
                       (list 0 vlog-mode-macro-face))
                 ;; behavioural
                 (list "@"
                       (list 0 vlog-mode-operator-face))
                 ;; numbers
                 (list (if vlog-mode-v2k-enabled
                           vlog-mode-keywordset-number-v2k-regexp
                         vlog-mode-keywordset-number-regexp)
                       (list 0 vlog-mode-number-face))
                 ;; identifiers
                 (list "\\(begin\\|fork\\)\\s-*:\\s-*\\(\\sw+\\)"
                       (list 2 vlog-mode-macro-face 'append))
                 ;; delays
                 (list "#\\s-*[0-9]+"
                       (list 0 vlog-mode-parameter-face 'append))
                 ;; parameter passing
                 (list "#\\s-*([^)]+)"
                       (list 0 vlog-mode-parameter-face 'append))
                 ;; instantiation
                 (list "\\(\\.\\sw+\\)[ \t\n]*(\\([^()]+\\))"
                       (list 1 vlog-mode-type-face 'append)
                       (list 2 vlog-mode-operator-face 'append))
                 (list "\\(\\.\\sw+\\)[ \t\n]*\\((\\s-*)\\)"
                       (list 1 vlog-mode-warning-face 'append)
                       (list 2 vlog-mode-warning-face 'append))
                 (list "\\(\\sw+\\)[ \t\n]*\\(#\\s-*([^()]+)\\)*[ \t\n]*\\<\\(\\sw+\\)\\>[ \t\n]*("
                       (list 1 vlog-mode-struct-face 'append)
                       (list 3 vlog-mode-module-face 'append))
                 ;; inline PSL assertions
                 (list "^[ \t]*//[ \t]*\\(psl\\|PSL\\)\\(.*\\)$"
                       (list 1 vlog-mode-psl-tag-face t)
                       (list 2 vlog-mode-psl-content-face t))
                 ;; documents
                 (list (concat "//\\s-*\\(" vlog-mode-keywordset-docs-regexp "\\)")
                       (list 1 vlog-mode-doc-face t)))
                ;; All-uppercase words (Macros)
                (when vlog-mode-highlight-all-uppercase-words
                  (list
                   (list "\\<[A-Z][A-Z0-9_]*\\>" (list 0 vlog-mode-number-face 'append))))))
  t)

(defun vlog-mode-enable-v2k ()
  "Enable Verilog 2000 support."
  (interactive)
  (setq vlog-mode-v2k-enabled t)
  ;; add keywords for Verilog 2000
  (setq vlog-mode-keywordset-types
        (append vlog-mode-keywordset-types
                vlog-mode-keywordset-types-v2k))
  (setq vlog-mode-keywordset-structs
        (append vlog-mode-keywordset-structs
                vlog-mode-keywordset-structs-v2k))
  (setq vlog-mode-keywordset-keywords
        (append vlog-mode-keywordset-keywords
                vlog-mode-keywordset-keywords-v2k))
  (setq vlog-mode-keywordset-systasks
        (append vlog-mode-keywordset-systasks
                vlog-mode-keywordset-systasks-v2k))
  (vlog-mode-make-keywords)
  ;; add config...endconfig block support for Verilog 2000
  (setq vlog-indent-special-end-scarce-words
        (append vlog-indent-special-end-scarce-words '("endconfig")))
  (setq vlog-indent-block-end-words
        (append vlog-indent-block-end-words '("endconfig")))
  (setq vlog-indent-block-beg-words
        (append vlog-indent-block-beg-words '("config")))
  (setq vlog-indent-special-beg-scarce-words
        (append vlog-indent-special-beg-scarce-words '("config")))
  (setq vlog-indent-calc-begs
        (append vlog-indent-calc-begs '("config")))
  (vlog-indent-make-regexps)
  ;; add localparam align support for Verilog 2000
  (setq vlog-decl-type-words
        (append vlog-decl-type-words '("localparam")))
  (vlog-lib-make-regexp)
  (add-hook 'vlog-mode-make-keymap-hook
            (lambda ()
              (define-key vlog-mode-map "s" 'vlog-mode-electric-s)
              (define-key vlog-mode-map "S" 'vlog-mode-electric-S))))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ vlog-mode electric keys ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-mode-electric-return ()
  "Start a new line. if `vlog-mode-auto-indent' is t, do indentation also.  If
`vlog-mode-auto-name-at-endmodule' is t, add module name after endmodule
automatically, with prefix `vlog-mode-endmodule-auto-name-prefix'."
  (interactive)
  (let ((block-end nil)
        (mod-name  nil)
        (icol      nil))
    (if (and vlog-mode-auto-delete-empty-line
             (looking-back "^\\s-+"))
        (delete-horizontal-space)
      (vlog-align-line))
    (setq block-end
          (cond ((looking-back "\\<begin\\s-*") "end")
                ((looking-back "\\<fork\\s-*")  "join")
                (t "")))
    (when block-end
      (setq icol (vlog-indent-level-at-pos)))
    ;; insert module name if looking at endmodule
    (when (and vlog-mode-auto-name-at-endmodule
               (looking-back "\\<endmodule\\s-*")
               (setq mod-name (vlog-lib-get-module-name)))
      (delete-horizontal-space)
      (insert (concat vlog-mode-endmodule-auto-name-prefix mod-name)))
    (insert "\n")
    (if (not (and vlog-mode-auto-end-block
                  (memq last-command '(vlog-mode-electric-n
                                       vlog-mode-electric-k))
                  icol))
        (when vlog-mode-auto-indent
          (funcall indent-line-function))
      (insert "\n")
      (vlog-lib-indent-to-column icol)
      (insert block-end)
      (forward-line -1)
      (funcall indent-line-function))))

(defun vlog-mode-electric-d ()
  "Insert `d', and then do something else."
  (interactive)
  (if (looking-back "\\<en")
      (progn (insert "d")
             (vlog-indent-line))
    (vlog-mode-electric-bitwidth "d")))

(defun vlog-mode-electric-e ()
  "Insert `e', and then do something else."
  (interactive)
  (insert "e")
  (when (looking-back "\\<\\(else\\)\\|\\(case\\)")
    (vlog-indent-line)))

(defun vlog-mode-electric-f ()
  "Insert `f', and then do something else."
  (interactive)
  (insert "f")
  (when (looking-back "\\<if")
    (vlog-indent-line)))

(defun vlog-mode-electric-k ()
  "Insert `k', and then do something else."
  (interactive)
  (insert "k")
  (when (looking-back "\\<fork")
    (vlog-indent-line)))

(defun vlog-mode-electric-n ()
  "Insert `n', and then do something else."
  (interactive)
  (insert "n")
  (when (looking-back "\\<\\(beg\\|jo\\)in")
    (vlog-indent-line)))

(defun vlog-mode-electric-bitwidth (c)
  "Convert 3b to 3'b automatically."
  (and (looking-back "\\<[0-9]+")
       (insert "'"))
  (insert c))

(defun vlog-mode-electric-b ()
  "Add possible ' before inserting b, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "b"))

(defun vlog-mode-electric-B ()
  "Add possible ' before inserting B, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "B"))

(defun vlog-mode-electric-h ()
  "Add possible ' before inserting h, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "h"))

(defun vlog-mode-electric-H ()
  "Add possible ' before inserting H, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "H"))

(defun vlog-mode-electric-D ()
  "Add possible ' before inserting D, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "D"))

(defun vlog-mode-electric-x ()
  "Add possible ' before inserting x, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "x"))

(defun vlog-mode-electric-X ()
  "Add possible ' before inserting X, for numbers."
  (interactive)
  (vlog-mode-electric-bitwidth "X"))

(defun vlog-mode-electric-s ()
  "Add possible ' before inserting s, for numbers (Verilog 2000)."
  (interactive)
  (vlog-mode-electric-bitwidth "s"))

(defun vlog-mode-electric-S ()
  "Add possible ' before inserting S, for numbers (Verilog 2000)."
  (interactive)
  (vlog-mode-electric-bitwidth "S"))

(defun vlog-mode-electric-space ()
  "Expand `reg 2' into `reg [1:0]' automatically, works with
defined parameters.
Also add `endmodule' after `module' (`endcase' after `case'),
if `vlog-mode-auto-end-module' (`vlog-mode-auto-end-block') is t."
  (interactive)
  (cond
   ;; auto expand width decl
   ((looking-back (concat "\\(" vlog-decl-type-words-re "\\)"
                          "\\s-+\\([0-9a-zA-Z_]+\\)"))
    (let ((beg    (match-beginning 2))
          (end    (match-end 2))
          (decl   (match-string-no-properties 1))
          (str    (match-string-no-properties 2))
          (width  nil)
          (wstr   ""))
      (when (string-match "^[0-9]+$" str)
        (setq width (string-to-int str)))
      (setq wstr (if width
                     (if (> width 1) (int-to-string (1- width)) nil)
                   (if (and (not (string= decl "parameter"))
                            (vlog-lib-get-module-para-val str))
                       (concat str "-1")
                     "")))
      (if (and wstr (string= wstr ""))
          (insert " ")
        (kill-region beg end)
        (vlog-lib-indent-to-column (car vlog-align-declaration-stop-list))
        (if wstr (insert "[" wstr ":0" "]"))
        (delete-horizontal-space)
        (vlog-lib-indent-to-column (nth 1 vlog-align-declaration-stop-list))
        (vlog-align-line))))
   ;; auto endcase
   ((and vlog-mode-auto-end-block
         (looking-back "\\<case")
         (eq last-command 'vlog-mode-electric-e))
    (let ((icol (vlog-indent-level-at-pos)))
      (save-excursion
        (end-of-line)
        (newline)
        (vlog-lib-indent-to-column icol)
        (insert "endcase")))
    (insert " "))
   ;; auto endmodule
   ((and vlog-mode-auto-end-module
         (looking-back "^\\(\\s-*\\)\\(macro\\)*module")
         (eq last-command 'vlog-mode-electric-e))
    (save-excursion
      (end-of-line)
      (newline)
      (when (match-end 1)
        (insert (match-string 1)))
      (insert "endmodule"))
    (insert " "))
   (t (insert " "))))

(defun vlog-mode-electric-semi ()
  "Insert `;', and then do something else."
  (interactive)
  (insert ";")
  (vlog-align-line))

(defun vlog-mode-electric-comma ()
  "Insert `,', and then do something else."
  (interactive)
  (if (/= ?\, (preceding-char))
      (insert ",")
    (backward-delete-char 1)
    (insert
     (or vlog-mode-double-comma-prefix "")
     "<="
     (or vlog-mode-double-comma-suffix ""))))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;+ block navigation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun vlog-goto-block-beg ()
  "Go to beginning of the block, works with:
begin/end, fork/join, case/endcase, <block>/end<block>, ...

If the cursor is standing on `begin', then go to the matching `end'.
If the cursor is not on `begin', then search the nearest `end'."
  (interactive)
  (let ((word (vlog-lib-word-atpt nil nil 'beg)))
    (push-mark)
    (if (string-match vlog-indent-block-end-words-re word)
        (vlog-indent-goto-block-beg (point-min) word)
      (vlog-re-search-backward vlog-indent-block-beg-words-re-1 
                               (point-min) t))))

(defun vlog-goto-block-end ()
  "Go to end of the block, works with:
begin/end, fork/join, case/endcase, <block>/end<block>, ...

If the cursor is standing on `end', then go to the matching `begin'.
If the cursor is not on `end', then search the nearest `begin'."
  (interactive)
  (let ((word (vlog-lib-word-atpt nil nil 'beg)))
    (push-mark)
    (if (string-match vlog-indent-block-beg-words-re word)
        (vlog-indent-goto-block-end (point-max) word)
      (vlog-re-search-forward vlog-indent-block-end-words-re-1
                              (point-max) t))))

(defun vlog-goto-block-match ()
  "Go to matching beginning or the end of the block, works with:
begin/end, fork/join, case/endcase, <block>/end<block>, ..."
  (interactive)
  (let ((word (vlog-lib-word-atpt nil nil 'beg)))
    (catch 'done
      (cond ((string-match vlog-indent-block-beg-words-re word)
             (push-mark)
             (vlog-indent-goto-block-end (point-max) word)
             (throw 'done t))
            ((string-match vlog-indent-block-end-words-re word)
             (push-mark)
             (vlog-indent-goto-block-beg (point-min) word)
             (throw 'done t))
            ((string= "else" word)
             (push-mark)
             (vlog-indent-goto-match-if (point-min))
             (throw 'done t))
            (t
             (if (string= "" word)
                 (message "We're not on the beg/end of a block")
               (message "`%s' is not a beg or end of a block." word))
             (throw 'done nil))))))

(defun vlog-select-block-or-sexp ()
  "Select a block if cursor is on a block beg/end, works with:
begin/end, fork/join, case/endcase, <block>/end<block>, ...
Otherwise, run command `mark-sexp'."
  (interactive)
  (let* ((info (vlog-lib-word-atpt t t))
         (word (car  info))
         (beg  (cadr info))
         (end  (cddr info)))
    (if (not (stringp word))
        (progn (mark-sexp)
               (message "Not at block beg/end, call `mark-sexp' instead."))
      (if (string-match vlog-indent-block-beg-words-re word)
          (progn
            (goto-char beg)
            (push-mark
             (save-excursion
               (vlog-indent-goto-block-end (point-max) word)
               (point))
             nil t))
        (if (string-match vlog-indent-block-end-words-re word)
            (progn
              (goto-char end)
              (push-mark
               (save-excursion
                 (goto-char beg) ;; goto beg first
                 (vlog-indent-goto-block-beg (point-min) word)
                 (point))
               nil t))
          (mark-sexp)
          (message "Not at block beg/end, call `mark-sexp' instead."))))))
;;- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun vlog-customize ()
  "Customize verilog support."
  (interactive)
  (customize-group 'verilog))

(provide 'vlog-mode)

;;; vlog-mode.el ends here
