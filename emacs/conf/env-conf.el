
(message ">>>>> Checking [ OS ] ....")
;; OS type
(defconst win32p
    (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst cygwinp
    (eq system-type 'cygwin)
  "Are we running on a WinTel cygwin system?")

(defconst linuxp
    (or (eq system-type 'gnu/linux)
        (eq system-type 'linux))
  "Are we running on a GNU/Linux system?")

(defconst unixp
  (or linuxp
      (eq system-type 'usg-unix-v)
      (eq system-type 'berkeley-unix))
  "Are we running unix")

(defconst linux-x-p
    (and window-system linuxp)
  "Are we running under X on a GNU/Linux system?")

(defvar mswin  (equal window-system 'w32)  "Non-nil means windows system.")
(defvar cygwin (equal system-type 'cygwin) "Non-nil means cygwin system.")

(defvar last-region-beg     nil "Beginning of last region.")
(defvar last-region-end     nil "End of last region.")
(defvar last-region-is-rect nil "Last region is rectangle or not.")

(defvar c-modes '(c-mode c++-mode awk-mode java-mode) "*C modes.")
(defvar dev-modes (append c-modes '(python-mode perl-mode makefile-gmake-mode)) "*Modes for develop.")

(defmacro GNULinux (&rest body)
  (list 'if (string-match "linux" (prin1-to-string system-type))
        (cons 'progn body))
  )

(defmacro Windows (&rest body)
  (list 'if (string-match "windows-nt" (prin1-to-string system-type))
        (cons 'progn body))
  )

(defmacro XLaunch (&rest body)
  (list 'if (eq window-system 'x)
        (cons 'progn body)))

(message ">>>>> Checking [ Emacs Version ] ....")
;; GNU Emacs
(defmacro GNUEmacs (&rest body)
  "Execute any number of forms if running under GNU Emacs."
  (list 'if (string-match "GNU Emacs" (version))
        (cons 'progn body)))

(defmacro GNUEmacs23 (&rest body)
  (list 'if (string-match "GNU Emacs 23" (version))
        (cons 'progn body)))

(defmacro GNUEmacs22 (&rest body)
  (list 'if (string-match "GNU Emacs 22" (version))
        (cons 'progn body)))

(defmacro GNUEmacs21 (&rest body)
  (list 'if (string-match "GNU Emacs 21" (version))
        (cons 'progn body)))

;; XEmacs
(defvar running-xemacs
  (string-match "XEmacs" emacs-version))

(defmacro XEmacs (&rest body)
  "Execute any number of forms if running under XEmacs."
  (list 'if (string-match "XEmacs" (version))
        (cons 'progn body)))

(defconst xemacsp (featurep 'xemacs)
  "Are we running XEmacs?")

;; Emacs version
(defconst is-before-emacs-21 (>= 21 emacs-major-version) "Before version 21")
(defconst is-after-emacs-22  (<= 22 emacs-major-version) "After version 22")
(defconst is-after-emacs-23  (<= 23 emacs-major-version) "After version 23")
(defconst emacs>=21p
    (and (not xemacsp) (or (= emacs-major-version 21) (= emacs-major-version 22) (= emacs-major-version 23)))
  "Are we running GNU Emacs 21 or above?")

(message ">>>>> Setting [ Emacs Variables ] ....")
;; Emacs veriables
(GNUEmacs
  (list emacs-version emacs-major-version emacs-minor-version
       system-type system-name system-configuration
       window-system
       (when (boundp 'aquamacs-version) aquamacs-version)))

(message ">>>>> Setting [ XEmacs Variables ] ....")
(XEmacs
    ;; don't offer migration of the init file
    (setq load-home-init-file t))

(message " 0 >>>>> Checking Environment... Done")
