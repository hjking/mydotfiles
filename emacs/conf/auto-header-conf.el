;;
;; Filename: auto-header-conf.el
;; Description: Setting for auto-header.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-09 10:16:12
;;
(setq header-full-name "Hong Jin")
(setq header-email-address "bestkindy@gmail.com")

;; set which items will be updated when save
(setq header-update-on-save
    '(  filename
        modified
        counter
        copyright))

;; set format
(setq header-field-list
'(  filename
    blank
    copyright
    version
    author
    created
    blank
    description
    ;;blank
    ;;modified_by
    ;; blank
    ;;status
    ;;blank
  ))

