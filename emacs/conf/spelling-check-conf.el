
;; Filename: spelling-check-conf.el
;; Description: Setting for flyspell
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-14 14:36:44

(message ">>>>> Loading [ Spell Correction Customization ] ....")

;; on-the-fly spelling checking
(autoload 'flyspell-mode "flyspell" "On-the-fly spelling checking" t)
;; don't consider that a word repeated twice is an error
(setq flyspell-mark-duplications-flag nil)
;; enable the likeness criteria
(setq flyspell-sort-corrections nil)
;; don't use `M-TAB' to correct word (only use `C-.')
(setq flyspell-use-meta-tab nil)
;; `flyspell-auto-correct-word' is bound to `C-.'
;; Press it one time to correct the word under the cursor.
;; If several spellings are possible, they appear in the minibuffer. Just
;; keep hitting `C-.' to replace the word with the successive suggestions.
;; dash character (`-') is considered as a word delimiter
(setq flyspell-consider-dash-as-word-delimiter-flag t)
;; don't print messages for every word (when checking the entire buffer)
;; as it causes an enormous slowdown
;;  (setq flyspell-issue-message-flag nil)

;; flyspell comments and strings in programming modes
;; (preventing it from finding mistakes in the code)
(add-hook 'autoconf-mode-hook   'flyspell-prog-mode)
(add-hook 'autotest-mode-hook   'flyspell-prog-mode)
(add-hook 'c++-mode-hook        'flyspell-prog-mode)
(add-hook 'c-mode-hook          'flyspell-prog-mode)
(add-hook 'cperl-mode-hook      'flyspell-prog-mode)
(add-hook 'emacs-lisp-mode-hook 'flyspell-prog-mode)
(add-hook 'makefile-mode-hook   'flyspell-prog-mode)
(add-hook 'nxml-mode-hook       'flyspell-prog-mode)
(add-hook 'python-mode-hook     'flyspell-prog-mode)
(add-hook 'sh-mode-hook         'flyspell-prog-mode)
(add-hook 'verilog-mode-hook    'flyspell-prog-mode t)
(add-hook 'java-mode-common-hook 'flyspell-prog-mode t)

