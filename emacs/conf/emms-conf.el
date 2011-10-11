
;; Filename: emms-conf.el
;; Description: Setting for emms.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-01-25 15:59:09
;;
(message ">>>>> Loading [ EMMS ] Customizations File ....")
(require 'emms-setup)
(emms-standard)
(emms-default-players)
(setq emms-repeat-playlist t
      emms-source-file-default-directory "/home/music/"
      emms-lyrics-coding-system nil     ;; let emacs to identify the encode of lyrics
      emms-lyrics-dir "/home/music/lyrics")
;; show info at mode-line
(require 'emms-mode-line)
(emms-mode-line 1)
;; show time of music
(require 'emms-playing-time)
(emms-playing-time 1)
;; show lyrics
(require 'emms-lyrics)
(emms-lyrics 1)
;; auto identify encode
(require 'emms-i18n)
;; auto save and import playlist
(require 'emms-history)
(emms-history-load)
;;
;;  (global-set-key (kbd "<f6>") 'emms-play-directory)
;;  (global-set-key (kbd "<f5>") 'emms-playlist-mode-go)

;;  (global-set-key (kbd "s-n") 'emms-next)
;;  (global-set-key (kbd "s-p") 'emms-previous)
;;  (global-set-key (kbd "s-s") 'emms-shuffle)
;;  (global-set-key (kbd "s-<RET>") 'emms-play-directory-tree)
;;  (global-set-key (kbd "s-<SPC>") 'emms-stop))
