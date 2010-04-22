
;; auto complete setting
;; available for Emacs 22/23

(message ">>>>> Loading [ auto-compression ] Customizations ....")
(add-to-list 'load-path "~/.emacs.d/plugin/auto-complete")
(require 'auto-complete)
(require 'auto-complete-config)
(setq ac-menu-height 20)
(ac-set-trigger-key "TAB")

(require 'auto-complete)
;;(require 'auto-complete-c)
(require 'auto-complete-config)
;;(require 'auto-complete-etags)
;;(require 'auto-complete-extension)
;;(require 'auto-complete-octave)
(require 'auto-complete-verilog)
(require 'auto-complete-yasnippet)
(global-auto-complete-mode t)
;;  ;; Use M-n/M-p to select candidates
(define-key ac-mode-map (kbd "M-TAB")             'auto-complete)
(define-key ac-complete-mode-map (kbd "<return>") 'ac-complete)
(define-key ac-complete-mode-map (kbd "M-j")      'ac-complete)
(define-key ac-complete-mode-map (kbd "M-n")      'ac-next)
(define-key ac-complete-mode-map (kbd "M-p")      'ac-previous)

(setq ac-dwim t)  ;; do what i mean mode
(setq ac-candidate-menu-height 20)
(setq ac-candidate-max ac-candidate-menu-height)

(set-face-background  'ac-candidate-face "lightgray")
(set-face-underline   'ac-candidate-face "darkgray")
(set-face-background  'ac-selection-face "steelblue")

;;  (set-default ac-sources '(ac-source-yasnippet
;;                 ac-source-abbrev
;;                 ac-source-imenu
;;                 ac-source-filename
;;                 ac-source-words-in-buffer
;;                 ac-source-words-in-all-buffer
;;                 ac-source-files-in-current-dir
;;                 ac-source-words-in-same-mode-buffers))
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers))

;;  (setq
;;    ac-trigger-commands
;;    '(self-insert-command
;;      autopair-insert-or-skip-quote
;;      autopair-backspace
;;      c-electric-backspace
;;      c-electric-backspace-kill))

(add-hook 'auto-complete-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-filename)))

;; auto complete for verilog mode
;;  (defun ac-settings-4-verilog ()
;;              (setq ac-sources
;;                    '(ac-source-verilog
;;                      ac-source-yasnippet
;;                      ac-source-abbrev
;;                      ac-source-words-in-buffer
;;                      ac-source-words-in-all-buffer
;;                      ac-source-files-in-current-dir
;;                      ac-source-filename
;;                      ac-source-symbols
;;                      ac-source-imenu)))
;;  (add-hook 'verilog-mode-hook 'ac-settings-4-verilog)
(add-hook 'verilog-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-verilog)))

;; auto complete for lisp mode
;;  (defun ac-settings-4-lisp ()
;;    (setq ac-omni-completion-sources '(("\\<require\s+'" ac-source-emacs-lisp-features)
;;                                       ("\\<load\s+\"" ac-source-emacs-lisp-features)))
;;    (setq ac-sources
;;          '(ac-source-yasnippet
;;            ac-source-symbols
;;            ac-source-abbrev
;;            ac-source-words-in-buffer
;;            ac-source-words-in-all-buffer
;;            ac-source-imenu
;;            ac-source-files-in-current-dir
;;            ac-source-filename)))
;;  (dolist (hook (list 'lisp-mode-hook 'emacs-lisp-mode-hook 'lisp-interaction-mode-hook
;;                      'svn-log-edit-mode))
;;    (add-hook hook 'ac-settings-4-lisp)
;;  )
(add-hook 'emacs-lisp-mode-hook (lambda () (add-to-list 'ac-sources 'ac-source-symbols)))

;; auto complete for text mode
;;  (defun ac-settings-4-text ()
;;              (setq ac-sources
;;                    '(ac-source-yasnippet
;;                      ac-source-abbrev
;;                      ac-source-words-in-buffer
;;                      ac-source-words-in-all-buffer
;;                      ac-source-imenu)))
;;  (add-hook 'text-mode-hook   'ac-settings-4-text)

;; auto complete for eshell mode
;;  (defun ac-settings-4-eshell ()
;;              (setq ac-sources
;;                    '(ac-source-yasnippet
;;                      ac-source-abbrev
;;                      ac-source-words-in-buffer
;;                      ac-source-words-in-all-buffer
;;                      ac-source-files-in-current-dir
;;                      ac-source-filename
;;                      ac-source-symbols
;;                      ac-source-imenu)))
;;  (add-hook 'eshell-mode-hook 'ac-settings-4-eshell)

