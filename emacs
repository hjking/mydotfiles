
(put 'upcase-region 'disabled nil)
;; Text mode
(setq default-major-mode 'text-mode)
;;(setq text-mode-hook '(lambda nil (setq fill-column 100) (auto-fill-mode 1)))
(setq text-mode-hook '(lambda nil (auto-fill-mode 1)))

;; Chinese
(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'euc-cn)
(set-clipboard-coding-system 'euc-cn)
(set-terminal-coding-system 'euc-cn)
(set-buffer-file-coding-system 'euc-cn)
(set-selection-coding-system 'euc-cn)
(modify-coding-system-alist 'process "*" 'euc-cn)
(setq default-process-coding-system '(euc-cn . euc-cn))
(setq-default pathname-coding-system 'euc-cn)

;; auto insert
(setq auto-insert t)
(setq auto-insert-query t)
(add-hook 'find-file-hooks 'auto-insert)
;;(setq auto-insert-directory "~/insert/")
;;
;;Time stamp
(add-hook 'write-file-hooks 'time-stamp)
(setq time-stamp-format "%:u %02m/%02d/%04y %02H:%02M:%02S")

;; no bell ring when TAB
(setq visible-bell t)
;; yes/no ==> y/n
(fset 'yes-or-no-p 'y-or-n-p)
;; display row and column number
(setq column-number-mode t)
(setq line-number-mode t)
;; highlight selected area
(transient-mark-mode t)
;; show paren
(show-paren-mode t)
;; no temp file
(setq-default make-backup-files nil)

(auto-image-file-mode t)

(setq x-select-enable-clipboard t)
;; display time
;;(display-time-mode 1)
;;(setq display-time-24hr-format t)

(global-font-lock-mode t)

;; display font
;;(set-default-font "clean 8x16")
(set-default-font "9x15")

;; session
;;(add-to-list 'load-path "<path to session>")
;;(require 'session)
;;(add-hook 'after-init-hook 'session-initialize)

;; color
(add-to-list 'load-path "~/.emacs.d/plugin/color_th")
(require 'color-theme)
(setq color-theme-is-global t)
(color-theme-jedit-grey)

;; Tab width
(setq default-tab-width 4)

;; Verilog-mode
(add-to-list 'load-path "~/.emacs.d/plugin/verilog")
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t );; load verilog mode only when needed
(add-to-list 'auto-mode-alist '("\\.[ds]?v\\'" . verilog-mode))
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)));;any file in verilog mode should have their keywords colorized
(setq verilog-indent-level             3
      verilog-indent-level-module      3
      verilog-indent-level-declaration 3
      verilog-indent-level-behavioral  3
      verilog-indent-level-directive   1
      verilog-case-indent              2
      verilog-auto-newline             t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        t
      verilog-auto-endcomments         t
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'declarations
      verilog-highlight-p1800-keywords nil
      verilog-linter			 "my_lint_shell_command"
)
(add-hook 'verilog-mode-hook '(lambda () (add-hook 'local-write-file-hooks (lambda() (untabify (point-min) (point-max))))))
