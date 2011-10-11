;; Outline is line-oriented and does not distinguish end-of-block.

;; `outline-minor-mode.el' is also used to collapse Lisp code (i.e., to see in
;; the buffer just the definition of a function instead of the whole body)

;; See also the library `foldout' and `hs-minor-mode', for instance, in the
;; Emacs manual.

;; outline mode commands for Emacs
(require 'outline)

;; bind the function `open-line' to `M-o' instead of `C-o' (by default)
(global-set-key (kbd "M-o") 'open-line)

;; bind the outline minor mode functions to an easy to remember prefix key
;; (more accessible than the horrible prefix `C-c @')
(setq outline-minor-mode-prefix (kbd "C-o"))

;; make other `outline-minor-mode' files (LaTeX, etc.) feel like Org files
(require 'outline-magic)
(add-hook 'outline-minor-mode-hook
          (lambda ()
            (define-key outline-minor-mode-map
              (kbd "<backtab>") 'outline-cycle)

            (define-key outline-minor-mode-map
              (kbd "<M-left>") 'outline-promote)
            (define-key outline-minor-mode-map
              (kbd "<M-right>") 'outline-demote)
            (define-key outline-minor-mode-map
              (kbd "<M-up>") 'outline-move-subtree-up)
            (define-key outline-minor-mode-map
              (kbd "<M-down>") 'outline-move-subtree-down)))

;; extra support for outline minor mode
(require 'out-xtra)



;; Org-style folding for a `.emacs' (and much more)

(defun my-outline-regexp ()
  "Calculate the outline regexp for the current mode."
  (let ((comment-starter (replace-regexp-in-string
                          "[[:space:]]+" "" comment-start)))
    (when (string= comment-start ";")
      (setq comment-starter ";;"))
;;    (concat "^" comment-starter "\\*+")))
    (concat "^" comment-starter "[*]+ ")))

(defun my-outline-minor-mode-hook ()
  (interactive)
  (setq outline-regexp (my-outline-regexp))

  ;; highlight the headings
  ;; see http://www.gnu.org/software/emacs/manual/html_node/emacs/Font-Lock.html
  ;; use M-x customize-apropos face to customize faces
  ;; to find the corresponding face for each outline level see org-faces.el
  (let ((heading-1-regexp
         (concat (substring outline-regexp 0 -1) "\\{1\\} \\(.*\\)"))
        (heading-2-regexp
         (concat (substring outline-regexp 0 -1) "\\{2\\} \\(.*\\)"))
        (heading-3-regexp
         (concat (substring outline-regexp 0 -1) "\\{3\\} \\(.*\\)"))
        (heading-4-regexp
         (concat (substring outline-regexp 0 -1) "\\{4,\\} \\(.*\\)")))
    (font-lock-add-keywords
     nil
     `((,heading-1-regexp 1 'org-level-1 t)
       (,heading-2-regexp 1 'org-level-2 t)
       (,heading-3-regexp 1 'org-level-3 t)
       (,heading-4-regexp 1 'org-level-4 t)))))

(add-hook 'outline-minor-mode-hook
          'my-outline-minor-mode-hook)

;; Add the following as the top line of your `.emacs':
;;
;; ; -*- mode: emacs-lisp; mode: outline-minor; -*-
;;
;; Now you can add `;;*' and `;;**', etc. as headings in your `.emacs' and
;; cycle using `M-tab', `M-left' and `M-right' will collapse or expand all
;; headings respectively. I am guessing you mean to make segments such as
;; `;;* SHORTCUTS' and `;;* VARIABLES', this will do that, but not too much
;; more.

;;* Level A1

;; Some text.

;;** Level A2

;; Some text.

;;*** Level A3

;; Some text.

;;* Level B1

;;** Level B2

;;*** Level B3

