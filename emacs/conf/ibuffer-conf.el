
;; ibuffer setting
;;
;;
(message ">>>>> Loading [ ibuffer ] Customizations File ....")
(require 'ibuffer)
(require 'ibuf-ext nil t)
(when (featurep 'ibuffer)
  (global-set-key (kbd "C-x C-b")   'ibuffer)
  (define-key ibuffer-mode-map "r"  'ywb-ibuffer-rename-buffer)
  (define-key ibuffer-mode-map (kbd "C-x C-f")  'ywb-ibuffer-find-file)
  (define-key ibuffer-mode-map " "  'scroll-up)
  (add-hook 'ibuffer-mode-hook
            (lambda ()
              (setq ibuffer-filter-groups
                    '(
                      ("*buffer*" (name . "\\*.*\\*"))
                      ("TAGS" (name . "^TAGS\\(<[0-9]+>\\)?$"))
                      ("dired" (mode . dired-mode))
                      ("perl" (mode . cperl-mode))
                      ("elisp" (or (mode . emacs-lisp-mode)
                                   (mode . lisp-interaction-mode)))
                      ))))
  (setq ibuffer-saved-filters
      '(("t" ((or (mode . latex-mode)
                 (mode . plain-tex-mode))))
        ("c" ((or (mode . c-mode)
                 (mode . c++-mode))))
        ("p" ((mode . cperl-mode)))
        ("e" ((or (mode . emacs-lisp-mode)
                  (mode . lisp-interaction-mode))))
        ("d" ((mode . dired-mode)))
        ("s" ((mode . shell-mode)))
        ("i" ((mode . image-mode)))
        ("h" ((mode . html-mode)))
        ("gnus" ((or (mode . message-mode)
                     (mode . mail-mode)
                     (mode . gnus-group-mode)
                     (mode . gnus-summary-mode)
                     (mode . gnus-article-mode))))
        ("pr" ((or (mode . emacs-lisp-mode)
                   (mode . cperl-mode)
                   (mode . c-mode)
                   (mode . c++-mode)
                   (mode . php-mode)
                   (mode . java-mode)
                   (mode . idl-mode)
                   (mode . lisp-interaction-mode))))
        ("m" ((mode . muse-mode)))
        ("w" ((or (mode . emacs-wiki-mode)
                  (mode . muse-mode))))
        ("*" ((name . "*")))
        ))
  )
;;;###autoload
(defun ywb-ibuffer-rename-buffer ()
  (interactive)
  (call-interactively 'ibuffer-update)
  (let* ((buf (ibuffer-current-buffer))
         (name (generate-new-buffer-name
                (read-from-minibuffer "Rename buffer(to new name): "
                                      (buffer-name buf)))))
    (with-current-buffer buf
      (rename-buffer name)))
  (call-interactively 'ibuffer-update))
(defun ywb-ibuffer-find-file ()
  (interactive)
  (let ((default-directory (let ((buf (ibuffer-current-buffer)))
      (if (buffer-live-p buf)
        (with-current-buffer buf
          default-directory)
        default-directory))))
    (call-interactively 'ido-find-file)))

