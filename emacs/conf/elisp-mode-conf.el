
(message ">>>>> Loading [ Elisp Mode ] Customizations ....")
(defun my-elisp-startup ()
  "Setup Emacs Lisp."
  (interactive)
  ;; Byte compile this file as soon as its saved.
  (setq byte-compile-warnings nil)
  (imenu-add-to-menubar "Functions")
  (define-key emacs-lisp-mode-map [f6] 'eval-buffer)
  (define-key emacs-lisp-mode-map [(meta f6)] 'emacs-lisp-byte-compile-and-load)
  (define-key emacs-lisp-mode-map [return] 'newline-and-indent)
  (define-key emacs-lisp-mode-map [?\C-c?t] 'xsteve-trace-function)
  (modify-syntax-entry ?- "w")
  (hs-minor-mode t)
  (turn-on-eldoc-mode)
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
    '(lambda () (byte-compile-file buffer-file-name))
    nil t
  )
  ;; When editing elisp code, we want hippie expand to reference emacs
  ;; lisp symbols. (Note: We are shifting this onto the front of the
  ;; list, so put this so -partially is called first)
  (make-local-variable 'hippie-expand-try-functions-list)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol)
  (add-to-list 'hippie-expand-try-functions-list
               'try-complete-lisp-symbol-partially)
  ;; Define lisp key macros
;;    (local-set-key "\C-css" 'insert-elisp-seperator-line)
;;    (local-set-key "\C-csh" 'insert-elisp-section-header)
;;    (local-set-key "\C-csb" 'insert-elisp-big-header)
)

(defun esk-remove-elc-on-save ()
  "If you're saving an elisp file, likely the .elc is no longer valid."
  (make-local-variable 'after-save-hook)
  (add-hook 'after-save-hook
    (lambda ()
      (if (file-exists-p (concat buffer-file-name "c"))
          (delete-file (concat buffer-file-name "c")))))
)

(add-hook 'lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'my-elisp-startup)
(add-hook 'emacs-lisp-mode-hook 'turn-on-auto-fill)
(add-hook 'emacs-lisp-mode-hook 'esk-remove-elc-on-save)
