;;
;; Filename: cperl-conf.el
;; Description: Setting for cperl.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-16 14:39:27

(message ">>>>> Loading [ Perl Mode ] Customizations ....")

(autoload 'cperl-mode "cperl-mode" "cperl mode" t)
(defalias 'perl-mode 'cperl-mode)
(add-to-list 'auto-mode-alist '("\\.\\([pP][Llm]\\|al\\)\\'" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("perl5" . cperl-mode))
(add-to-list 'interpreter-mode-alist '("miniperl" . cperl-mode))

(defun my-perl-startup ()
  "Setup perl."
  (interactive)
  (local-set-key '[pause] 'perldb)
  (setq gud-perldb-command-name "perl -w ") ; For warnings
  (setq tab-width 4)
  (setq indent-tabs-mode nil)  ; Autoconvert tabs to spaces
  (setq perl-indent-level 2)
  (setq perl-tab-always-indent nil) ; Indent if at left margin, else tab
  (setq perl-continued-statement-offset 2)
  (setq perl-continued-brace-offset -2)
)
(add-hook 'perl-mode-hook 'my-perl-startup)
(add-hook 'cperl-mode-hook
  (lambda ()  (local-set-key (kbd "C-h f") 'cperl-perldoc)))

