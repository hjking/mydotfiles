
;; Filename: org-conf.el
;; Description: Setting for org.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-09-13 15:04:34

(message ">>>>> Loading [ org ] Customization File ....")
(add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))
(setq load-path (cons "~/.emacs.d/plugin/org/lisp" load-path))
(require 'org-install)
(setq org-hide-leading-star t)
(setq org-log-done 'time) ;; mark DONE item with time
(setq org-startup-folded nil )  ;; open org in unfolded view

(setq org-publish-project-alist
      '(("note-org"
         :base-directory "~/.emacs.d/org"
         :publishing-directory "~/.emacs.d/org/publish"
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
         :auto-index nil
         :index-filename "index.org"
         :index-title "index"
         :link-home "index.html"
         :section-numbers nil
         :style "<link rel=\"stylesheet\" href=\"./style/emacs.css\" type=\"text/css\"/>")
        ("note-static"
         :base-directory "~/.emacs.d/org"
         :publishing-directory "~/.emacs.d/org/publish"
         :recursive t
         :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|swf\\|zip\\|gz\\|txt\\|el"
         :publishing-function org-publish-attachment)
        ("note" 
         :components ("note-org" "note-static")
         :author "hong_jin@founder.com"
         )))

(setq org-todo-keywords
           '((sequence "TODO" "NEXT(n)" "STARTED(s)" "|" "DONE" "CANCELLED" "WaitingOn" "Action")
             (sequence "REPORT" "BUG" "KNOWNCAUSE" "|" "FIXED")
             (sequence "OPEN(O!)" "|" "CLOSED(C!)")
             ))
(setq org-todo-keyword-faces 
      (quote (("TODO" :foreground "red" :weight bold)
              ("NEXT" :foreground "blue" :weight bold)
              ("STARTED" :foreground "blue" :weight bold)
              ("DONE" :foreground "forest green" :weight bold)
              ("WAITING" :foreground "orange" :weight bold)
              ("SOMEDAY" :foreground "magenta" :weight bold)
              ("CANCELLED" :foreground "forest green" :weight bold)
              ("OPEN" :foreground "blue" :weight bold)
              ("CLOSED" :foreground "forest green" :weight bold)
              ("PHONE" :foreground "forest green" :weight bold))))

;; Fast todo selection allows changing from any task todo state to any other state
(setq org-use-fast-todo-selection t)

(setq org-tag-alist '(("@work" . ?w)
                      ("@home" . ?h)
                      ("@errand" . ?e)
                      ("@office" . ?o)
                      ("laptop" . ?l)))
; Allow setting single tags without the menu
(setq org-fast-tag-selection-single-key (quote expert))

; For tag searches ignore tasks with scheduled and deadline dates
(setq org-agenda-tags-todo-honor-ignore-options t)

;; automatically assign tags to tasks based on state changes
(setq org-todo-state-tags-triggers
      (quote (("CANCELLED" ("CANCELLED" . t))
              ("WAITING" ("WAITING" . t))
              ("SOMEDAY" ("WAITING" . t))
              (done ("WAITING"))
              ("TODO" ("WAITING") ("CANCELLED"))
              ("NEXT" ("WAITING"))
              ("STARTED" ("WAITING"))
              ("DONE" ("WAITING") ("CANCELLED")))))

;; Let org-mode use ido
(setq org-completion-use-ido t)
(setq org-remember-templates
    '(("Todo" ?t "* TODO %?\n %i\n %a" org-default-notes-file "Tasks")
      ("Idea" ?i "* %^{Title}\n %i\n %a" org-default-notes-file "Ideas")
      ("Journal" ?j "* %U %?\n\n %i\n %a" org-default-notes-file)))
(setq org-default-notes-file "~/.emacs.d/templates/notes_tmpl")

(global-set-key "\C-c l" 'org-store-link)
(global-set-key "\C-c a" 'org-agenda)
(global-set-key "\C-c b" 'org-iswitchb)

;;  (add-hook 'org-mode-hook 'turn-on-font-lock)  ; Org buffers only
;; Wrap long lines
(add-hook 'org-mode-hook 'toggle-truncate-lines)

;; flyspell mode for spell checking everything
(add-hook 'org-mode-hook 'turn-on-flyspell 'append)

;; Make TAB the yas trigger key in the org-mode-hook and turn on flyspell mode
;;    (add-hook 'org-mode-hook
;;              (lambda ()
;;                ;; yasnippet
;;                (make-variable-buffer-local 'yas/trigger-key)
;;                (org-set-local 'yas/trigger-key [tab])
;;                (define-key yas/keymap [tab] 'yas/next-field-group)
;;                ;; Undefine C-c [ and C-c ] since this breaks my org-agenda files when directories are include
;;                ;; It expands the files in the directories individually
;;                (org-defkey org-mode-map "\C-c["    'undefined)
;;                (org-defkey org-mode-map "\C-c]"    'undefined)
;;                (local-set-key (kbd "C-c M-o") 'bh/mail-subtree)) 'append)
;;    
;;    (defun bh/mail-subtree ()
;;      (interactive)
;;      (org-mark-subtree)
;;      (org-mime-subtree))


