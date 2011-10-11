;;
;; Filename: eshell-conf.el
;; Description: Setting for eshell.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-22 09:40:50

(message ">>>>> Loading [ eshell ] Customizations ....")

;; Eshell, the Emacs Shell
(when (require 'eshell nil t)
  (when (not (fboundp 'eshell))
    (autoload 'eshell "eshell" nil t))

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-a") 'eshell-bol)
              (local-set-key (kbd "<up>") 'previous-line)
              (local-set-key (kbd "<down>") 'next-line)))

  (setq eshell-cd-on-directory nil)

  (setq eshell-save-history-on-exit t
        eshell-hist-ignoredups      nil)

  (setq eshell-default-target-is-dot t
        eshell-pushd-tohome          t)

  (setq eshell-cmpl-cycle-completions nil)
  (setq eshell-banner-message (ted-random-emacs-haiku ""))

  (when (facep 'header-line) ; Is there a better test for header-line-ness?
    (add-hook 'eshell-mode-hook
            (lambda ()
              (shiftf header-line-format mode-line-format nil))))

  (if (memq system-type '(darwin berkeley-unix))
      (defun ted-eshell-C-t ()
        "Request status of the running Eshell command.
  Only works on BSD."
        (interactive)
        ;; An anamorphic `when' would be nice here.
        (let ((proc (eshell-interactive-process)))
          (if proc
              (progn
                (process-send-string proc (string 20)))
            (call-interactively 'transpose-chars))))
    (defun ted-eshell-C-t ()
      (interactive)
      (ding)))
  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "C-t") 'ted-eshell-C-t)))
  (defun ted-eshell-prompt ()
    (let ((user (or (getenv "USER") (user-login-name) "ted"))
          (host (car (split-string
                      (or (getenv "HOST") (system-name) "unknown")
                      "\\.")))
          (char (if (= (user-uid) 0) "#" ":")))
      (format "\n%s@%s%s " user host char)))
  (setq eshell-prompt-function 'ted-eshell-prompt)
  (setq eshell-prompt-regexp "^[^#:\n]*[#:] ")

  (autoload 'ansi-color-filter-apply "ansi-color")
  (add-hook 'eshell-preoutput-filter-functions 'ansi-color-filter-apply)

  (add-hook 'eshell-mode-hook
            (lambda ()
              (local-set-key (kbd "s-p")
                             'eshell-previous-matching-input-from-input)))

  (when (featurep 'xemacs)
    (eval-after-load "esh-cmd"
      '(defun eshell-find-alias-function (name)
         "Check whether a function called `eshell/NAME' exists."
         (let* ((sym (intern-soft (concat "eshell/" name)))
                (file (symbol-file sym)))
           ;; If the function exists, but is defined in an eshell module
           ;; that's not currently enabled, don't report it as found
           (if (and file
                    (string-match "\\(em\\|esh\\)-\\(.*\\)\\(\\.el\\)?\\'" file))
               (let ((module-sym
                      (intern (file-name-sans-extension
                               (file-name-nondirectory
                                (concat "eshell-" (match-string 2 file)))))))
                 (if (and (functionp sym)
                          (or (null module-sym)
                              (eshell-using-module module-sym)
                              (memq module-sym (eshell-subgroups 'eshell))))
                     sym))
             ;; Otherwise, if it's bound, return it.
             (if (functionp sym)
                 sym))))))

  (setq eshell-aliases-file "~/.alias")

  (defun eshell/less (file)
    "Pager view of FILE."
    (view-file file)
    0)
  (defalias 'eshell/more 'eshell/less)

  (defun eshell/rmb ()
    "Remove Emacs backup files in this directory."
    (mapconcat (lambda (filename)
                 (delete-file filename)
                 filename)
               (directory-files default-directory nil "~\\'" t)
               ", "))
  (setq eshell-scroll-show-maximum-output nil)
  (defalias 'eshell/clear 'ted-clear)

  (defun eshell/info (subject)
    "Read the Info manual on SUBJECT."
    (let ((buf (current-buffer)))
      (Info-directory)
      (let ((node-exists (ignore-errors (Info-menu subject))))
        (if node-exists
            0
          (switch-to-buffer buf)
          (eshell-print (format "There is no Info manual on %s.\n"
                                subject))
          1))))

  (defun eshell/emacs (&rest args)
    "Open a file in Emacs. Some habits die hard."
    (if (null args)
        (bury-buffer)
      (mapc 'find-file (mapcar 'expand-file-name
                               (eshell-flatten-list args))))
    0)
    (defalias 'eshell/emacsclient 'eshell/emacs)

  (defun eshell/vi (file)
    "Open a file with Viper."
    (with-current-buffer (find-file file)
      (setq viper-mode t)
      (viper-mode))
    0)

  (defalias 'eshell/concat 'eshell/cat)

  (eval-after-load "em-ls"
    '(progn
       (defvar ted-eshell-ls-keymap
         (let ((map (make-sparse-keymap)))
           (define-key map (kbd "RET")      'ted-eshell-ls-find-file-at-point)
           (define-key map (kbd "<return>") 'ted-eshell-ls-find-file-at-point) ;%
           (define-key map (if (featurep 'xemacs)
                               (kbd "<button2>")
                             (kbd "<mouse-2>"))
             'pat-eshell-ls-find-file-at-mouse-click)
           map))

       (defadvice eshell-ls-decorated-name (after ted-electrify-ls activate)
         "Eshell's `ls' now lets you click or RET on file names to open them."
         (add-text-properties 0 (length ad-return-value)
                              (list 'help-echo "RET, middle-click: visit this file"
                                    'mouse-face 'highlight
                                    'keymap ted-eshell-ls-keymap)
                              ad-return-value)
         ad-return-value)

       (defun ted-eshell-ls-find-file-at-point (point)
         "RET on Eshell's `ls' output to open files."
         (interactive "d")
         (find-file (buffer-substring-no-properties
                     (previous-single-property-change point 'help-echo)
                     (next-single-property-change point 'help-echo))))

       ;; Not defined in Emacs.
       (unless (fboundp 'event-point)
         (defun event-point (event)
           "Return the character position of mouse EVENT."
           (posn-point (event-end event))))

       (defun pat-eshell-ls-find-file-at-mouse-click (event)
         "Middle click on Eshell's `ls' output to open files.
       From Patrick Anderson via the EmacsWiki."
         (interactive "e")
         (ted-eshell-ls-find-file-at-point (event-point event))))))

