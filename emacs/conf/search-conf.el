
  (message ">>>>> Loading [ Search and Replace Customization ] ....")
  ;; highlight during searching
  (setq query-replace-highlight t)
  ;; highlight incremental search
  (setq search-highlight t)
  ;; Non-nil if searches should ignore case
  (setq case-fold-search t)
  ;; always exit searches at the beginning of the expression found
  (add-hook 'isearch-mode-end-hook 'custom-goto-match-beginning)
  (defun custom-goto-match-beginning ()
    "Use with isearch hook to end search at first char of match."
    (when isearch-forward (goto-char isearch-other-end)))
  ;; repeat search
  (defun isearch-occur ()
    "Invoke `occur' from within isearch."
    (interactive)
    (let ((case-fold-search isearch-case-fold-search))
      (occur (if isearch-regexp isearch-string (regexp-quote isearch-string)))))
  ;;  (define-key isearch-mode-map (kbd "C-o") 'isearch-occur)

  ;; `M-x flush-lines' deletes each line that contains a match for REGEXP

  ;; *** Grep search
  ;; ignore case distinctions in the default grep command
  (setq grep-command "grep -n -i -e ")

  ;; I-search with initial contents.
  ;; original source: http://platypope.org/blog/2007/8/5/a-compendium-of-awesomeness
  (defvar isearch-initial-string nil)

  (defun isearch-set-initial-string ()
    (remove-hook 'isearch-mode-hook 'isearch-set-initial-string)
    (setq isearch-string isearch-initial-string)
    (isearch-search-and-update))

  (defun isearch-forward-at-point (&optional regexp-p no-recursive-edit)
    "Interactive search forward for the symbol at point."
    (interactive "P\np")
    (if regexp-p (isearch-forward regexp-p no-recursive-edit)
      (let* ((end (progn (skip-syntax-forward "w_") (point)))
             (begin (progn (skip-syntax-backward "w_") (point))))
        (if (eq begin end)
            (isearch-forward regexp-p no-recursive-edit)
            (setq isearch-initial-string (buffer-substring begin end))
            (add-hook 'isearch-mode-hook 'isearch-set-initial-string)
            (isearch-forward regexp-p no-recursive-edit)))))
