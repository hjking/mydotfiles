(message ">>>>> Loading [ htmlize ] Customizations ....")
(require 'htmlize)
(setq htmlize-html-major-mode 'html-mode)
;; output type of generated HTML
(setq htmlize-output-type 'css)
;; charset declared by the resulting HTML documents
(setq htmlize-html-charset "utf-8")
;; non-ASCII characters (codes in the 128-255 range) are copied to HTML
;; without modification -- if your HTML is in Unicode
(setq htmlize-convert-nonascii-to-entities nil)
;; key binding
(global-set-key (kbd "M-P") 'htmlize-buffer)
