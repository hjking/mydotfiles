;;
;; Filename: auto-complete-conf.el
;; Description: Setting for auto-complete.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-08-31 15:46:50
;; available for Emacs 22/23

(message ">>>>> Loading [ auto-complete ] Customizations ....")
(require 'auto-complete)
;;(require 'auto-complete-c)
(require 'auto-complete-config)
;;(require 'auto-complete-etags)
;;(require 'auto-complete-extension)
;;(require 'auto-complete-octave)
;(require 'auto-complete-verilog)
;(require 'auto-complete-yasnippet)

(global-auto-complete-mode t)

(ac-config-default)

;; menu height
(setq ac-menu-height 20)
;; trigger key
(setq ac-set-trigger-key "TAB")

;; add mode to auto-complete mode
;(add-to-list 'ac-modes 'new-mode)

;; Just ignore case
(setq ac-ignore-case t)
;; Ignore case if completion target string doesn't include upper characters
;(setq ac-ignore-case 'smart)
;; Distinguish case
;(setq ac-ignore-case nil)

;; color
(set-face-background 'ac-candidate-face "lightgray")
(set-face-underline 'ac-candidate-face "darkgray")
(set-face-background 'ac-selection-face "steelblue")

(setq ac-use-menu-map t)

(setq ac-dwim t)  ;; do what i mean mode
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)
;; delay time of show menu
(setq ac-auto-show-menu 1)

;; dictionary directories
;(setq auto-comp-dict-load-path (concat auto-comp-load-path "dict"))
(add-to-list 'ac-dictionary-directories "~/.emacs.d/plugin/auto-complete/dict")
;;(add-to-list 'ac-dictionary-directories auto-comp-dict-load-path)

;; User defined dictionary files
;; (add-to-list 'ac-user-dictionary-files "~/.dict")   ;; default

;; User defined dictionary
;; (add-to-list 'ac-user-dictionary "foobar@example.com")
;;

(setq ac-sources (append ac-sources '(ac-source-dictionary)))
;; Change default sources
;; (setq-default ac-sources '(ac-source-words-in-all-buffer))
(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))

;; key binding
;;  ;; Use M-n/M-p to select candidates
;; (global-set-key "\M-/" 'auto-complete)
(define-key ac-mode-map (kbd "M-TAB")             'auto-complete)
(define-key ac-complete-mode-map (kbd "<return>") 'ac-complete)
(define-key ac-complete-mode-map (kbd "M-j")      'ac-complete)
(define-key ac-completing-map    "\M-/"           'ac-stop)
(define-key ac-complete-mode-map (kbd "M-n")      'ac-next)
(define-key ac-complete-mode-map (kbd "M-p")      'ac-previous)
(define-key ac-menu-map "\C-n" 'ac-next)
(define-key ac-menu-map "\C-p" 'ac-previous)

;;  (set-default ac-sources '(ac-source-yasnippet
;;                 ac-source-abbrev
;;                 ac-source-imenu
;;                 ac-source-filename
;;                 ac-source-words-in-buffer
;;                 ac-source-words-in-all-buffer
;;                 ac-source-files-in-current-dir
;;                 ac-source-words-in-same-mode-buffers))
;;(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))

;;  (setq ac-trigger-commands
;;    '(self-insert-command
;;      autopair-insert-or-skip-quote
;;      autopair-backspace
;;      c-electric-backspace
;;      c-electric-backspace-kill))


;; auto complete for verilog mode
;(defun ac-settings-4-verilog ()
;  (setq ac-sources
;        '(ac-source-verilog
;          ac-source-yasnippet
;          ac-source-abbrev
;          ac-source-words-in-buffer
;          ac-source-words-in-all-buffer
;          ac-source-files-in-current-dir
;          ac-source-filename
;          ac-source-symbols
;          ac-source-imenu))
;  ;(add-to-list ac-user-dictionary-files "verilog-dict")
;)
;;(add-hook 'verilog-mode-hook 'ac-settings-4-verilog)
;(add-hook 'verilog-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-verilog)))

;; auto complete for lisp mode
;(defun ac-settings-4-lisp ()
;  (setq ac-omni-completion-sources '(("\\<require\s+'" ac-source-emacs-lisp-features)
;                                     ("\\<load\s+\"" ac-source-emacs-lisp-features)))
;  (setq ac-sources
;        '(ac-source-yasnippet
;          ac-source-symbols
;          ac-source-abbrev
;          ac-source-words-in-buffer
;          ac-source-words-in-same-mode-buffers
;          ac-source-imenu
;          ac-source-files-in-current-dir
;          ac-source-filename))
;  ;(add-to-list ac-user-dictionary-files "lisp-dict")
;)
;(dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
;                    'svn-log-edit-mode))
;  (add-hook hook 'ac-settings-4-lisp)
;)
;;(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))

;; auto complete for text mode
;(defun ac-settings-4-text ()
;            (setq ac-sources
;                  '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-buffer
;                    ac-source-words-in-all-buffer
;                    ac-source-imenu)))
;(add-hook 'text-mode-hook   'ac-settings-4-text)

;; auto complete for eshell mode
;(defun ac-settings-4-eshell ()
;            (setq ac-sources
;                  '(ac-source-yasnippet
;                    ac-source-abbrev
;                    ac-source-words-in-buffer
;                    ac-source-words-in-all-buffer
;                    ac-source-files-in-current-dir
;                    ac-source-filename
;                    ac-source-symbols
;                    ac-source-imenu)))
;(add-hook 'eshell-mode-hook 'ac-settings-4-eshell)

