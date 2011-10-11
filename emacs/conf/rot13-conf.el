;;
;; Filename: rot13-conf.el
;; Description: Setting for rot13
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:43:03
;;
(defvar rot13-translate-table
  (let ((str (make-string 127 0)) (i 0))
    (while (< i 127)
      (aset str i i) (setq i (1+ i)))
    (setq i 0)
    (while (< i 26)
      (aset str (+ i ?a) (+ (% (+ i 13) 26) ?a))
      (aset str (+ i ?A) (+ (% (+ i 13) 26) ?A))
      (setq i (1+ i))) str)
  "String table for rot 13 translation.")

(defun rot13-string (string)
  "Return Rot13 encryption of STRING."
  (with-temp-buffer
    (insert string)
    (rot13-region (point-min) (point-max))
    (buffer-string)))

(defun rot13-region (start end)
  "Rot13 encrypt the region between START and END in current buffer."
  (interactive "r")
  (translate-region start end rot13-translate-table))

