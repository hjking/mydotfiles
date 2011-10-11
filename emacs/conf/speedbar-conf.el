
;; everything browser (into individual source files), or Dired on steroids
(require 'speedbar-XXX)

;; number of spaces used for indentation
(setq speedbar-indentation-width 2)

;; expand/collapse LaTeX sections
(speedbar-add-supported-extension '(".tex" ".bib" ".w" ".nw"))

;; jump to speedbar frame
(global-set-key (kbd "<f4>") 'speedbar-get-focus)

;; bind the arrow keys in the speedbar tree
;; [http://www.uweb.ucsb.edu/~dreamtheorist/emacs.html]
(define-key speedbar-key-map (kbd "<right>") 'speedbar-expand-line)
(define-key speedbar-key-map (kbd "<left>") 'speedbar-contract-line)

;; parameters to use when creating the speedbar frame in Emacs
(setq speedbar-frame-parameters '((width . 30)
                                  (height . 45)
                                  (foreground-color . "blue")
                                  (background-color . "white")))

;; same frame speedbar
(require 'sr-speedbar)
(global-set-key (kbd "<f4>") 'sr-speedbar-toggle)


