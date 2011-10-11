;;
;; Filename: desktop-conf.el
;; Description: Setting for desktop.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-13 18:23:54
;;
(require 'desktop)
;;  (desktop-save-mode 1)
(setq desktop-load-locked-desktop t)
(setq desktop-dirname "~/.emacs.d/")
(setq destop-base-file-name "emacs.desktop")
(setq desktop-buffers-not-to-save
    (concat "\\(" "^nn\\.a[0-9]+\\|\\.log\\|(ftp)\\|^tags\\|^TAGS"
            "\\|\\.emacs.*\\|\\.diary\\|\\.newsrc-dribble\\|\\.bbdb"
            "\\)$"))
(add-to-list 'desktop-modes-not-to-save 'dired-mode)
(add-to-list 'desktop-modes-not-to-save 'Info-mode)
(add-to-list 'desktop-modes-not-to-save 'info-lookup-mode)
(setq history-length 250)
(add-to-list 'desktop-globals-to-save 'file-name-history)

(desktop-load-default)
(desktop-read)
