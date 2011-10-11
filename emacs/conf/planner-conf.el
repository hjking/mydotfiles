;;
;; Filename: planner-conf.el
;; Description: Setting for planner.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 13:56:48
;;

(when (require 'planner nil t)
  (require 'planner-bbdb)
  (require 'planner-id)
  (require 'planner-multi)
  (require 'planner-gnus)
  (when (featurep 'xtla) (require 'planner-xtla))
  (require 'planner-accomplishments) ; M-x planner-accomplishments-show after M-x plan
  (require 'planner-tasks-overview) ; M-x planner-tasks-overview

  ;;(setq planner-renumber-tasks-automatically t)
  (setq planner-task-dates-favor-future-p t)
  (setq planner-carry-tasks-forward 7)
  (global-set-key [(super ?p)] 'plan)
  (global-set-key [(control super ?p)] 'planner-create-task-from-buffer)
  (global-set-key [(meta super p)] 'planner-annotation-as-kill)
  (planner-calendar-insinuate) ; n and N show the planner page from the calendar buffer

  (when (file-readable-p diary-file)
    (require 'planner-diary)
    (setq planner-diary-number-of-days 5)
  ;;(setq planner-day-page-template "* Tasks\n\n\n* Schedule\n\n\n* Notes\n\n\n")
    (setq planner-day-page-template "* Tasks\n\n\n* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>\n\n* Notes\n\n\n"))

  ;; alternate 2nd method to insert the diary contents in the file instead of calling planner-diary-entries-here
  ;;(setq planner-day-page-template "* Tasks\n\n\n* Diary\n\n\n* Notes\n\n\n")
  ;;(setq planner-diary-use-diary t)
  ;;(planner-diary-insinuate)

  (define-key planner-mode-map [(shift return)] 'planner-edit-task-description)
  (define-key planner-mode-map [f2] 'xsteve-planner-save)
  (defun xsteve-planner-save ()
    (interactive)
    (planner-fix-tasks)
    (planner-save-buffers))
)
