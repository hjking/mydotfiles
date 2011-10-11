
;; Filename: erc-conf.el
;; Description: Setting for erc.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2011-03-22 10:11:45
;;
(message ">>>>> Loading [ ERC ] Customizations ....")

;; ERC, an Emacs IRC client
(when (require 'erc nil t)
  (autoload 'erc-select-read-args "erc" nil nil) ; needed for XEmacs
  (autoload 'erc-select "erc" nil t)
  (autoload 'erc-select-ssl "erc" nil t)

  (setq erc-server                         "irc.freenode.net"
        erc-port                           6667
        erc-user-full-name                 "King Hung"
        erc-email-userid                   "hon9jin@gmail.com"
        erc-nick                           '("hjking" "kinghom" "hon9jin")
        erc-password                       nil ; set this in local config
        erc-nickserv-passwords             nil ; set this in local config
        erc-anonymous-login                t
        erc-auto-query                     'bury
        erc-join-buffer                    'bury
        erc-max-buffer-size                30000
        erc-prompt-for-password            nil
        erc-join-buffer                    'buffer
        erc-command-indicator              "CMD"
        erc-echo-notices-in-current-buffer t
        erc-send-whitespace-lines          nil
        erc-hide-list                      '("JOIN" "PART" "QUIT")
        erc-ignore-list                    '("jibot"))

  (setq erc-quit-reason-various-alist
        '(("brb"    "I'll be right back.")
          ("lunch"  "Having lunch.")
          ("dinner" "Having dinner.")
          ("food"   "Getting food.")
          ("sleep"  "Sleeping.")
          ("work"   "Getting work done.")
          (".*"     (yow))))

  (setq erc-autojoin-channels-alist
        '(
          ("freenode.net" "#clojure" "#clojure.de" "#muse")
  ;;        ("freenode.net" "#couchdb" "#haskell")
          ("irc.int.ru" "#unix")
          ("irc.fu-berlin.de" "#unix.ru" "#fidorus")
          ("fu-berlin.de" "#unix.ru" "#fidorus")
          ))

  (setq erc-part-reason-various-alist erc-quit-reason-various-alist
        erc-part-reason               'erc-part-reason-various
        erc-quit-reason               'erc-quit-reason-various)
  (defvar ted-erc-autojoin t
    "Whether or not ERC should autojoin on connect.")

  (defvar ted-erc-identify t
    "Whether or not ERC should identify with NickServ on connect.")

  (defun ted-freenode (autojoin)
    (interactive (list (y-or-n-p "Autojoin channels? ")))
    (setq ted-erc-autojoin autojoin
          ted-erc-identify nil)
    (erc :server "irc.freenode.net"
         :port 8001
         :nick "hober"
         :password (cdr (assoc "hober" (cadr (assoc 'freenode erc-nickserv-passwords))))))

  (defun ted-w3c-irc ()
    (interactive)
    (erc :server "irc.w3.org" :port 6665 :nick "hober"))

  (defun ted-moz-irc ()
    (interactive)
    (erc :server "irc.mozilla.org" :port 6667 :nick "hober"))
)
