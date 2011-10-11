
(message ">>>>> Loading [ Frame Customization ] ....")
;; display file name on the frame title
;;(setq frame-title-format "%n%F/%b")
(setq default-frame-alist '((width . 105) (height . 40) (menu-bar-lines . 1)))
;; (setq frame-title-format '(
;;    "Emacs:"
;;    (:eval ( user-full-name))
;;    "@"
;;    (:eval (getenv-internal "HOSTNAME"))
;;    ":"
;;    (:eval (or (buffer-file-name) (buffer-name))))
;; )
(setq frame-title-format
  '("%b - "
   (:eval ( user-full-name))
   "@"
   (:eval (or (system-name) (getenv-internal "HOSTNAME")))
   ":"
   (:eval (or (buffer-file-name) (buffer-name)))
   " - Emacs " emacs-version
  )
)

;; menu bar
(setq menu-bar-mode t)
;;
;; tool bar
;; hide toolbar with emacs version >=21
(if (>= emacs-major-version 21)
  (tool-bar-mode -1)
;;(setq tool-bar-mode nil)
)

;; list of frame parameters for creating the initial frame
;;      (setq initial-frame-alist '((top . 0) (left . 0)))
;;      (setq initial-frame-alist
;;          (append (list
;;                   '(internal-border-width . 2)
;;                   '(line-spacing          . 1))
;;                  initial-frame-alist))
;;
;;      ;; list of default values for frame creation
;;      (setq default-frame-alist
;;          (cond ((= (x-display-pixel-height) 1280)
;;                 '((left . 0) (height . 70)))
;;
;;                ((= (x-display-pixel-height) 1024)
;;                 '((left . 0) (height . 60)))
;;
;;                ((= (x-display-pixel-height) 800)
;;                 (cond (win32p
;;                        '((left . 0) (height . 55)))
;;                       (linuxp
;;                        '((left . 0) (height . 47)
;;                          (vertical-scroll-bars . right)))))
;;                ((= (x-display-pixel-height) 768)
;;                 '((left . 0) (height . 46)))))

;;*** ---- Dialog Boxes
;; don't use dialog boxes to ask questions
(setq use-dialog-box nil)
;; don't use a file dialog to ask for files
(setq use-file-dialog nil)
