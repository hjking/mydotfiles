;;; vim-modes.el

;; Copyright (C) 2009, 2010 Frank Fischer

;; Author: Frank Fischer <frank.fischer@mathematik.tu-chemnitz.de>,
;;
;; This file is not part of GNU Emacs.

;;; Code:

(vim:deflocalvar vim:mode-string)
(defun vim:update-mode-line (ident)
  "Updates the mode-line to show the specified identifier `ident'."
  (setq vim:mode-string (concat "<" (or ident "?") ">"))
  (force-mode-line-update))


(defun vim:mode-name (mode)
  "Converts a mode-name to vim-mode naming conventions, e.g.
'normal is converted to 'vim:normal-mode."
  (intern (concat "vim:" (symbol-name mode) "-mode")))

(vim:deflocalvar vim:active-mode nil
  "The currently active vim-mode.") 

(vim:deflocalvar vim:active-command-function nil
  "The command function of the currently active vim-mode.")

(defun vim:activate-mode (mode)
  "Activates a certain vim-mode, disabling the currently active one."
  (when vim:active-mode
    (funcall vim:active-mode -1))
  (when mode
    (funcall (vim:mode-name mode) 1)))


(defmacro* vim:define-mode (name doc
                                 &rest body
                                 &key
                                 ident
                                 message
                                 command-function
                                 (cursor ''box)
                                 keymaps)
  "Defines a new VIM-mode with certain `name', mode-line-identifier `ident',
activation `message', a `command-function' to be called when a
vim-command should be executed, a `cursor' shape and a list of `keymaps'."
  (let* ((mode-name (vim:mode-name name))
         (pred-name (intern (concat (symbol-name mode-name) "-p")))
         (on-name (intern (concat "vim:activate-" (symbol-name name) "-mode")))
         (cursor-name (intern (concat (symbol-name mode-name)
                                      "-cursor"))))
    `(progn
       (defcustom ,cursor-name ,cursor
         ,(concat "The cursor-type for vim-mode " (symbol-name name) ".")
         :group 'vim-mode)
       
       (define-minor-mode ,mode-name ,doc nil nil nil
         (when ,mode-name
           ,@(when ident `((vim:update-mode-line ,ident)))
           ,@(when message `((let (message-log-max) (message ,message))))
           (setq vim:active-mode ',mode-name)
           (setq vim:active-command-function
                 ,(if command-function
                      command-function
                    'vim:default-command-function))
           (vim:set-cursor ,cursor-name)
           (vim:set-keymaps ',mode-name ,keymaps))
         ,@(progn
             (while (keywordp (car body)) (pop body) (pop body))
             body))

       (defun ,pred-name ()
         ,(concat "Returns t iff vim-mode is in " (symbol-name name) " mode.")
         (and ,mode-name t))
       
       (defun ,on-name ()
         ,(concat "Activates " (symbol-name name) " mode.")
         (interactive)
         (vim:activate-mode ',name)))))

(font-lock-add-keywords 'emacs-lisp-mode '("vim:define-mode"))

(provide 'vim-modes)

;;; vim-modes.el ends here

