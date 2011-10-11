(message ">>>>> Loading [ Calendar Customization ] ....")
(setq diary-file "~/.emacs.d/diary")
(setq calendar-load-hook
  '( lambda ()
     (set-face-foreground 'diary-face "skyblue")
     (set-face-background 'holiday-face "slate blue")
     (set-face-foreground 'holiday-face "white")
   )
)
;; remove some holidays
(setq holiday-bahai-holidays nil)       ; get rid of Baha'i holidays
(setq holiday-general-holidays nil)     ; get rid of too U.S.-centric holidays
(setq holiday-hebrew-holidays nil)      ; get rid of religious holidays
(setq holiday-islamic-holidays nil)     ; get rid of religious holidays
(setq holiday-oriental-holidays nil)    ; get rid of Oriental holidays
(setq holiday-solar-holidays nil)
;; add some Belgian holidays
(setq holiday-local-holidays
      '(
        (holiday-fixed 01 01 "New Year's Day")
        (holiday-fixed 02 14 "Valentine's Day")
        (holiday-fixed 05 01 "Labor Day")
        ;; holidays with variable dates
        (holiday-float 5 0 2 "Mother's Day")
        (holiday-float 6 0 3 "Father's Day"))
)
;; user defined holidays
(setq holiday-other-holidays nil)  ; default
;; mark dates of holidays in the calendar
(setq mark-holidays-in-calendar t)
;; set location
(setq calendar-location-name "Osaka")
(setq calendar-remove-frame-by-deleting t)
;; week start with Mon
(setq calendar-week-start-day 1)

(setq calendar-view-diary-initially-flag t)
(setq calendar-mark-diary-entries-flag t)
(setq diary-number-of-entries 7)
(setq calendar-date-display-form '(year "-" month "-" day))
(setq calendar-time-display-form '(24-hours ":" minutes
                                    (if time-zone " (") time-zone (if time-zone ")")))
(setq diary-date-forms '((year "-" month "-" day)
                   (month "/" day "[^/0-9]")
                   (month "/" day "/" year "[^0-9]")
                   (monthname " *" day "[^,0-9]")
                   (monthname " *" day ", *" year "[^0-9]")
                   (dayname "\\W")))

(add-hook 'diary-display-hook 'fancy-diary-display)
(add-hook 'today-visible-calendar-hook 'calendar-mark-today)

