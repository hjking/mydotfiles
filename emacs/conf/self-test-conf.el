
;; Filename: self-test-conf.el
;; Description: Setting for selftest
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:41:23
;;
(message ">>>>> Loading [ selftest ] Customizations ....")

(when (require 'selftest nil t)
  (define-selftest exercise
    "Did I get >=20min of exercise yesterday"
    :group 'health
    :when 'always)

  (define-selftest early-to-rise
    "Did I get up at or before 7:30 AM today"
    :group 'health
    :when 'weekday)

  (define-selftest desk
    "Is my desk area clean and clutter-free"
    :group 'house
    :when 'weekday)

  (define-selftest dishes
    "Did I do dishes yesterday"
    :group 'house
    :when 'always)

  (define-selftest trash
    "Did I take out the trash yesterday"
    :group 'house
    :when 'always)

  (define-selftest books
    "Did I read from my book yesterday"
    :group 'mind
    :when 'always)

  (define-selftest backups
    "Have I backed up my computers in the last 30 days"
    :group 'tech
    :when 'always)

  (defun ted-selftest-work-p ()
    "Is today a day I should take my work-related tests?"
    (let ((day-of-week (nth 6 (decode-time (current-time)))))
      (member day-of-week
              ;; Tue Wed Thu Fri Sat
              '(2 3 4 5 6))))

  (define-selftest tickets
    "Did I close a ticket yesterday"
    :group 'work
    :when 'ted-selftest-work-p)

  (define-selftest billing
    "Are my records of my billable hours more up-to-date"
    :group 'work
    :when 'weekday)

  (define-selftest pride
    "Am I excited to give my scrum report"
    :group 'work
    :when 'weekday)

  (define-selftest oss
    "Did I contribute to open source last week"
    :group 'tech
    :when 'monday)

  (define-selftest microformats
    "Did I make a contribution to Microformats last week"
    :group 'tech
    :when 'monday)

  (define-selftest html5
    "Did I make a contribution to HTML 5 last week"
    :group 'tech
    :when 'monday)

  (define-selftest emacs
    "Did I make a contribution to Emacs last week"
    :group 'tech
    :when 'monday)

  (define-selftest sleep
    "Did I get a full night's sleep last night"
    :group 'health
    :when 'always)

  (defun ted-selftest-alcohol-p ()
    "Is today a day I should take my alcohol test?"
    (let ((day-of-week (nth 6 (decode-time (current-time)))))
      (member day-of-week
              ;; Tue Wed Thu
              '(2 3 4))))

  (define-selftest alcohol
    "Did I have at most one drink yesterday"
    :group 'health
    :when 'ted-selftest-alcohol-p)

  (define-selftest phone-home
    "Did I talk to my parents last week"
    :group 'family
    :when 'monday)

  (define-selftest love
    "Did I tell Erin I love her yesterday"
    :group 'family
    :when 'always)

  (define-selftest attention
    "Did I keep up with the firehose yesterday"
    :group 'mind
    :when 'always)

  (define-selftest blogging
    "Did I post to one of my blogs yesterday"
    :group 'mind
    :when 'always)

  (define-selftest meals
    "Did I have three square meals yesterday"
    :group 'health
    :when 'always)

  ;; At some point yesterday, was my email inbox smaller than the low
  ;; point from the day before?
  (define-selftest inbox
    "Did I shrink my inbox over the last 2 days"
    :group 'mind
    :when 'always)

  (define-selftest cell-phone
    "Did I charge my cell phone yesterday"
    :group 'misc
    :when 'always)

  (define-selftest teeth
    "Did I brush my teeth at least twice yesterday"
    :group 'health
    :when 'always)

  (define-selftest voicemail
    "Was my voicemail inbox empty yesterday"
    :group 'mind
    :when 'always)

  (define-selftest meta
    "Did I run my personal unit tests yesterday"
    :group 'misc
    :when 'always)

  (define-selftest old-friends
    "Did I contact an old friend last week"
    :group 'misc
    :when 'monday)

  (define-selftest breakfast-with-erin
    "Did I have breakfast with Erin this morning"
    :group 'family
    :when 'weekday)

  (define-selftest water
    "Did I have at least 4 glasses of water yesterday"
    :group 'health
    :when 'always)

  (define-selftest laundry
    "Did I do or help with >=1 load of laundry last week"
    :group 'house
    :when 'monday)

  (define-selftest cooking
    "Did I prepare or help prepare >=1 dinner last week"
    :group 'house
    :when 'monday)

  (define-selftest elevator
    "Did I take the elevator at work at most one time yesterday?"
    :group 'work
    :when 'ted-selftest-work-p)

  ;; (define-selftest NAME
  ;;   "DOCSTRING"
  ;;   :group 'GROUP
  ;;   :when 'always)
)

