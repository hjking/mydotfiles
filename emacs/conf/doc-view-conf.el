;;
;; Filename: doc-view-conf.el
;; Description: Setting for doc-view.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:51:28
;;
(message ">>>>> Loading [ doc view ] Customizations ....")

;; view PDF/PostScript/DVI files in Emacs
(when (try-require 'doc-view)

  ;; `doc-view' integrates with the usual bookmark facility. So simply use
  ;; `C-x r m' (`bookmark-set') to jump back to the last page you've read
  ;; in a PDF document.


  ;; DPI resolution used to render the documents
  ;; `doc-view-enlarge' (`+') and `doc-view-shrink' (`-') work fine to zoom
  ;; in or out
  (setq doc-view-resolution 96)

  ;; DPI your screen supports
  (setq doc-view-display-size 96)

  ;; You can open the *text* of the current doc in a new buffer, by pressing
  ;; `C-c C-t' in doc-view-mode

  (setq doc-view-conversion-refresh-interval 3)
)

;; Another option, without `doc-view', is `! pdtotext ? - RET'


;; antiword will be run on every doc file you open
;; TODO sudo aptitude install antiword
(autoload 'no-word "no-word" "word to txt")
(add-to-list 'auto-mode-alist '("\\.doc\\'" . no-word))

;; un-xls
;; TODO sudo aptitude install xlhtml
(defun no-xls (&optional filename)
  "Run xlhtml and w3m -dump on the entire buffer.
Optional FILENAME says what filename to use.
This is only necessary for buffers without
proper `buffer-file-name'.  FILENAME should
be a real filename, not a path."
  (interactive "fExcel File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "xlhtml -nc -te %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.xls\\'" . no-xls))

;; no-ppt
;; TODO sudo aptitude install ppthtml
;; FIXME Not that good! (some text repeated multiple times)
(defun no-ppt (&optional filename)
  "Run ppthtml and w3m -dump on the entire buffer.
Optional FILENAME says what filename to use.
This is only necessary for buffers without
proper `buffer-file-name'.  FILENAME should
be a real filename, not a path."
  (interactive "fPowerPoint File: ")
  (when (and filename
             (not (buffer-file-name)))
    (write-file (make-temp-file filename)))
  (erase-buffer)
  (shell-command
   (format "ppthtml %s | w3m -dump -T text/html" (buffer-file-name))
   (current-buffer))
  (setq buffer-file-name nil)
  (set-buffer-modified-p nil))
(add-to-list 'auto-mode-alist '("\\.ppt\\'" . no-ppt))

