
;; Filename: cua-conf.el
;; Description: Setting for CUA mode
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-01-24 15:22:32
;;
;;
(message ">>>>> Loading [ CUA Mode Customization ] ....")
;; CUA mode sets up key bindings used in many other applications (`C-x',
;; `C-c', `C-v' and `C-z').
;; The `C-x' and `C-c' keys only do cut and copy when the region is active, so
;; in most cases, they do not conflict with the normal function of these
;; prefix keys.
;; If you really need to perform a command which starts with one of the prefix
;; keys even when the region is active, you have three options:
;; - press the prefix key twice very quickly (within 0.2 seconds),
;; - press the prefix key and the following key within 0.2 seconds, or
;; - use the SHIFT key with the prefix key, i.e. `C-S-x' or `C-S-c'.
;;
;; You can customize `cua-enable-cua-keys' to completely disable the CUA
;; bindings, or `cua-prefix-override-inhibit-delay' to change the prefix
;; fallback behavior.

;; CUA mode also provides enhanced rectangle support with visible rectangle
;; highlighting. Check out "Emacs Column Editing" at
;; http://www.vimeo.com/1168225?pg=embed&sec=1168225.
;;
;; `C-RET' runs the command `cua-set-rectangle-mark'
;; `M-n' runs the command `cua-sequence-rectangle'
;; ;; activate CUA mode
(cua-mode t)
;;(setq use-cua t)
(setq cua-auto-tabify-rectangles nil) ;; Don't tabify after rectangle commands
(transient-mark-mode 1) ;; No region when it is not highlighted
(setq cua-keep-region-after-copy t) ;; Standard Windows behaviour

;; (setq cua-rectangle-mark-key "")
;; standard Windows behavior
;; (when is-after-emacs-23
;;   (setq cua-keep-region-after-copy t)
;;   (setq cua-remap-control-z nil)
;;   (setq cua-remap-control-v nil))

;; (autoload 'cua--init-rectangles "cua-rect")
;; (cua--init-rectangles)

;; (when is-after-emacs-23
;;   (define-key cua--cua-keys-keymap [(control z)] nil)
;;   (define-key cua--cua-keys-keymap [(control v)] nil)
;;   (define-key cua--cua-keys-keymap [(meta v)] nil))

;;  (eal-define-keys
;;  'cua--rectangle-keymap
;;  `(("M-f"     forward-word-remember)
;;   ("M-b"     backward-word-remember)
;;   ("C-c C-f" cua-fill-char-rectangle)
;;   ("'"       cua-insert-char-rectangle)
;;   ("<right>" cua-resize-rectangle-right)
;;   ("<left>"  cua-resize-rectangle-left)
;;   ("<down"   cua-resize-rectangle-down)
;;   ("<up>"    cua-resize-rectangle-up)))

;;    ;; fix funny things of cursor moving commands
;;    (add-hook 'cua-mode-hook
;;              (lambda ()
;;                (dolist (cmd '(forward-char
;;                               backward-char
;;                               previous-line
;;                               next-line
;;                               forward-paragraph
;;                               backward-paragraph
;;                               beginning-of-buffer
;;                               end-of-buffer))
;;                  (put cmd 'CUA nil))))

;; shift + click select region
(define-key global-map (kbd "<S-down-mouse-1>") 'ignore) ; turn off font dialog
(define-key global-map (kbd "<S-mouse-1>") 'mouse-set-point)
(put 'mouse-set-point 'CUA 'move)

