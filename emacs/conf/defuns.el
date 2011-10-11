(defvar missing-packages-list nil
  "List of packages that try-require can not find.")

(defun prepend-path ( my-path )
    (setq load-path (cons (expand-file-name my-path) load-path)))

(defun append-path ( my-path )
    (setq load-path (append load-path (list (expand-file-name my-path)))))

;; attempt to load a feature/library, failing silently
(defun try-require (feature)
  "Attempt to load a library or module. Return true if the
library given as argument is successfully loaded. If not, instead
of an error, just add the package to a list of missing packages."
  (condition-case err
      ;; protected form
    (progn
      (message "Checking for library `%s'..." feature)
        (if (stringp feature)
            (load-library feature)
          (require feature))
        (message "Checking for library `%s'... Found" feature))
    ;; error handler
    (file-error  ; condition
      (progn
        (message "Checking for library `%s'... Missing" feature)
        (add-to-list 'missing-packages-list feature 'append))
      nil)
  )
)

;;;###autoload
(defun am-add-hooks (hooks function &optional append local)
  "Call `add-hook' on hook list HOOKS use arguments FUNCTION, APPEND, LOCAL.
HOOKS can be one list or just a hook."
  (if (listp hooks)
      (mapc
       `(lambda (hook)
          (add-hook hook ',function append local))
       hooks)
    (add-hook hooks function append local)))

;;;###autoload
(defun am-intern (&rest strings)
  "`intern' use STRINGS."
  (intern
   (apply
    'concat
    (mapcar
     (lambda (element)
       (if (stringp element) element (symbol-name element)))
     strings))))

;;;###autoload
(defun am-variable-is-t (symbol)
  "Return SYMBOL's value is t or not."
  (and (boundp symbol) (symbol-value symbol)))

;;;###autoload
(defmacro am-def-active-fun (symbol &optional fun-name)
  "Make definition of function judge variable is active or not."
  `(defun ,(if fun-name fun-name symbol) ()
     ,(concat "`" (symbol-name symbol) "' is t or not.")
     (am-variable-is-t ',symbol)))

;;;###autoload
(defun am-forward-word-or-to-word ()
  "`forward-word' or `forward-to-word'.
If after excute `forward-to-word', current position
is at next line, then rollback and excute `forward-word'"
  (interactive)
  (let ((noo (line-number-at-pos)) no)
    (save-excursion
      (forward-to-word 1)
      (setq no (line-number-at-pos)))
    (if (> no noo)
        (forward-word)
      (forward-to-word 1))))

;;;###autoload
(defmacro am-with-temp-mode (mode &rest body)
  "Create a temporary buffer with mode MODE, and evaluate BODY there like `progn'.
See also `with-temp-buffer'."
  `(with-temp-buffer
     (funcall ,mode)
     ,@body))

;;;###autoload
(defun am-equal-ignore-case (str1 str2)
  "STR1 equal ignore case to STR2 or not."
  (string= (downcase str1) (downcase str2)))

;; open my Emacs init file
(defun my-open-dot-emacs ()
  "Opening `~/.emacs'."
  (interactive)
  (find-file "~/.emacs")
)

;;; === insert filename ===
(defun my-insert-file-name ()
  "Insert the buffer-file-name at point."
  (interactive)
  (insert buffer-file-name)
)

;;; === insert date ===
(defun my-insert-date-stamp ()
  "Insert a time stamp at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%m-%d" (current-time)))
)

;;; === insert date and time ===
(defun my-insert-date-time-stamp ()
  "Insert date and time at point."
  (interactive)
;;  (shell-command "date +'%Y-%B-%d (%H:%M)' | tr -d '\n' " (quote (4)) nil) )
  (insert (format-time-string "%Y-%m-%d %3a %H:%M:%S" (current-time)))
)

(defun my-insert-date (prefix)
  "Insert the current date in ISO format. With prefix-argument,
  add day of week. With two prefix arguments, add day of week and
  time."
  (interactive "P")
  (let ((format (cond ((not prefix) "%Y-%m-%d")
                      ((equal prefix '(4)) "%Y-%m-%d %a")
                      ((equal prefix '(16)) "%Y-%m-%d %a %H:%M"))))
    (insert (format-time-string format))
   )
)

;;; === insert-braces ===
(defun my-insert-braces ()
  "Insert matched braces, leave point inside."
  (interactive "*")
  (let (blink-paren-function) ;nil it temporarily
    (execute-kbd-macro
      (if (and (eq major-mode 'cc-c++-mode) (not (looking-at ";")))
        "{};" "{}"
      )
    )
  )
  (backward-sexp 1)
  (if
    (save-excursion
        (forward-char -1)
        (looking-at "\\$")
    )
    nil
    (reindent-then-newline-and-indent)
;;    (c-indent-exp)
    (forward-char 1)
    (newline-and-indent)
  )
)

;;;; Function Set for Copy
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; === copy line ===
(defun my-copy-line (&optional arg)
  "Save current line into Kill-Ring without mark the line"
  (interactive "P")
  (let ((beg (line-beginning-position))
     (end (line-end-position arg)))
  (copy-region-as-kill beg end))
)

;;; === copy word ===
(defun my-copy-word (&optional arg)
  "Copy word at point"
  (interactive "P")
  (let ((beg (progn (if (looking-back "[a-zA-Z0-9]" 1) (backward-word 1)) (point)))
     (end (progn (forward-word arg) (point))))
  (copy-region-as-kill beg end))
)

;;; === copy region ===
(defun my-copy-paragraph (&optional arg)
  "Copy paragraphe at point"
  (interactive "P")
  (let ((beg (progn (backward-paragraph 1) (point)))
     (end (progn (forward-paragraph arg) (point))))
  (copy-region-as-kill beg end))
)

(defun copy-file-path ()
  "Copy the current buffer's file path or dired path to kill-ring."
  (interactive)
  (if (equal major-mode 'dired-mode)
      (kill-new default-directory)
    (kill-new (buffer-file-name))
    )
  (message "File path copied.")
)

;;; === duplicate current line===
(defun my-duplicate-line ()
  "Duplicate current line."
  (interactive)
  (progn
    (my-kill-ring-save-line) ; save line
    (save-excursion ; duplicate line
      (end-of-line)
      (insert "\n")
      (yank)
    )
    (let ( (n (my-get-col)) ) ; move to new line, goto same column
      (forward-line +1)
      (move-to-column n)
    )
  )
)

;;; === delete current line ===
(defun my-delete-line ()
  "Delete current line."
  (interactive)
  (progn
    (beginning-of-line) (kill-line 1)
  )
)

;;; === delete ^M ===
(defun my-delete-crtl-M ()
  "Delete all ^M (dos --> unix line endings)."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "+" "")
    )
  )
)

;; convert a buffer from DOS `^M' end of lines to Unix end of lines
(defun dos-to-unix ()
  "Remove all visible ^M from the current buffer."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\r" nil t)
      (replace-match ""))
  )
)

;; convert a buffer from Unix end of lines to DOS `^M' end of lines
(defun unix-to-dos ()
  "Convert a buffer from unix to dos."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward "\n" nil t)
      (replace-match "\r\n"))
   )
)

(defun dos2unix ()
  "dos2unix on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'unix))

(defun unix2dos ()
  "unix2dos on current buffer."
  (interactive)
  (set-buffer-file-coding-system 'dos))

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
  See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (next-line 1)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
   See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (when newline-and-indent
    (indent-according-to-mode)))

;; Autoindent open-*-lines
(defvar newline-and-indent t
  "Modify the behavior of the open-*-line functions to cause them to autoindent.")

;;; === move line ===
;; move (shift) a line of text up or down like you would do in Eclipse
;; pressing Alt-Up (or Down)
(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (let ((col (current-column))
        start
        end)
    (beginning-of-line)
    (setq start (point))
    (end-of-line)
    (forward-char)
    (setq end (point))
    (let ((line-text (delete-and-extract-region start end)))
      (forward-line n)
      (insert line-text)
      ;; restore point to original column in moved line
      (forward-line -1)
      (forward-char col)
    )
  )
)

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n)))
)

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n))
)

;;; === match paren ===
(defun my-match-paren ()
  "Move to the parenthesis matching the one under the cursor."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))
  )
)

;;;###autoload
;;;use % to jumo to the matching parenthesis
(defun goto-match-paren (arg)
  "Go to the matching paren if on a paren, otherwise insert %."
  (interactive "p")
  (let ((prev-char (char-to-string (preceding-char)))
        (next-char (char-to-string (following-char))))
    (cond ((string-match "[[{(<]" next-char) (forward-sexp 1))
          ((string-match "[\]})>]" prev-char) (backward-sexp 1))
          (t (self-insert-command (or arg 1))))
  )
)

(defun match-parenthesis (arg)
   "Match the current character according to the syntax table.
   Based on the freely available match-paren.el by Kayvan Sylvan.
   I merged code from goto-matching-paren-or-insert and match-it.
   You can define new \"parentheses\" (matching pairs).
   Example: angle brackets. Add the following to your .emacs file:
      (modify-syntax-entry ?< \"(>\" )
      (modify-syntax-entry ?> \")<\" )
   You can set hot keys to perform matching with one keystroke.
   Example: f6 and Control-C 6.
      (global-set-key \"\\C-c6\" 'match-parenthesis)
      (global-set-key [f6] 'match-parenthesis) "
     (interactive "p")
     (let ((syntax (char-syntax (following-char))))
       (cond
         ((= syntax ?\()
          (forward-sexp 1) (backward-char))
         ((= syntax ?\))
          (forward-char) (backward-sexp 1))
         (t (message "No match"))
       )
     )
)


;;;; Function Set for Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; switch wrap lines : on=>off / off=>on
(defun get-mode-name ()
  "Display `major-mode' and `mode-name'"
  (interactive)
  (message "major-mode:%s, mode-name:%s" major-mode mode-name))

(defun my-wrap-mode-on ()
  "Minor mode for making buffer not wrap long lines to next line."
  (interactive)
  (setq truncate-lines nil))

(defun my-wrap-mode-off ()
  "Minor mode for making buffer wrap long lines to next line."
  (interactive)
  (setq truncate-lines t))

(defun my-toggle-wrap-mode ()
  "Switch wrap mode from wrap to non-wrap, or vice-versa."
  (interactive)
  (if (eq truncate-lines nil)
      (my-wrap-mode-off)
    (my-wrap-mode-on)
  )
)

;;; === switch major mode ===
;; {{
(defvar switch-major-mode-last-mode nil)
(make-variable-buffer-local 'switch-major-mode-last-mode)

(defun major-mode-heuristic (symbol)
  (and (fboundp symbol) (string-match ".*-mode$" (symbol-name symbol)))
)

(defun my-switch-major-mode (mode)
  "Switch major mode"
  (interactive
    (let ((fn switch-major-mode-last-mode) val)
      (setq val
        (completing-read
          (if fn
            (format "Switch major mode to (default %s): " fn)
            "Switch major mode to: "
          )
          obarray 'major-mode-heuristic t nil nil (symbol-name fn)
        )
      )
      (list (intern val))
    )
  )
  (let ((last-mode major-mode))
    (funcall mode)
    (setq switch-major-mode-last-mode last-mode)
  )
)
;; }} end of switch major mode


;;;; Function Set for Buffer and File
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun reload-dotemacs ()
  "Reload ~/.emacs."
  (interactive)
  (load-file "~/.emacs")
)

;;; === load .emacs.elc ===
(defun my-reload-dotemacselc ()
  "Byte compiles and loads the .emacs.elc file."
  (interactive)
  (progn
    (byte-compile-file "~/.emacs")
    (load-file "~/.emacs.elc")
  )
)

(defun my-autocompile-dotemacs nil
  "Auto compile ~/.emacs when it's saved"
  (interactive)
  (require 'bytecomp)
  (let ((dotemacs (expand-file-name "~/.emacs")))
    (if (string= (buffer-file-name) (file-chase-links dotemacs))
      (byte-compile-file dotemacs)
    )
  )
)
(add-hook 'after-save-hook 'my-autocompile-dotemacs)

;; replace(refresh) current buffer text with the text of the visited file on disk
(defun my-revert-buffer ()
  "Unconditionally revert current buffer."
  (interactive)
  (flet ((yes-or-no-p (msg) t))
    (revert-buffer))
)

;;; === save-buffer-kill-buffer ===
(defun my-save-buffer-kill-buffer (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills it."
  (interactive "P")
  (let ((buf (current-buffer)))
    (if (and (buffer-file-name buf)
      (buffer-modified-p)
      (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
      (save-buffer nil)
    )
    (delete-windows-on buf)
    (kill-buffer buf)
  )
)

(defun delete-current-file ()
  "Delete the file associated with the current buffer."
  (interactive)
  (let (currentFile)
    (setq currentFile (buffer-file-name))
    (when (yes-or-no-p (concat "Delete File: " currentFile))
      (kill-buffer (currentFile))
      (delete-file currentFile)
      (message (concat "Delete File: " currentFile))
  ))
)

(defun clean-up-buffer-or-region ()
  "Untabifies, indents and deletes trailing whitespace from buffer or region."
  (interactive)
  (save-excursion
    (unless (region-active-p)
      (mark-whole-buffer))
    (untabify (region-beginning) (region-end))
    (indent-region (region-beginning) (region-end))
    (save-restriction
      (narrow-to-region (region-beginning) (region-end))
      (delete-trailing-whitespace)))
)

;;; === kill-buffer-other-window ===
(defun my-kill-buffer-other-window (arg)
  "Kill the buffer in the ARGth other window, or the current buffer if no
other window."
  (interactive "p")
  (let ((buf (save-window-excursion
           (other-window arg)
           (current-buffer))))
    (delete-windows-on buf)
    (kill-buffer buf)
  )
)

;;; === my-save-buffer-kill-frame ===
(defun my-save-buffer-kill-frame (arg)
  "Saves buffer, if necessary (with ARG, w/o asking), and then kills
it and its frame."
  (interactive "P")
  (let ((buf (current-buffer))
    (delete-frame nil)
    (kill-buffer nil))
    (if (and (buffer-file-name buf)
         (buffer-modified-p)
         (or arg (y-or-n-p (format "Save buffer %s? " (buffer-name)))))
    (save-buffer nil))
    (setq kill-buffer (or (not (buffer-modified-p buf))
              (not (buffer-file-name buf))
              (yes-or-no-p (concat "Buffer "
                           (buffer-name buf)
                           " modified; kill anyway? "))))
    (setq delete-frame (if (and (one-window-p)
                (or arg
                    (unwind-protect
                    (y-or-n-p "Delete frame as well? ")
                      (message ""))))
               (selected-frame)
             nil))
    (delete-windows-on buf)
    (if kill-buffer (progn (if (string-match "XEmacs" (emacs-version))
                   (set-buffer-modified-p nil buf)
                 (save-excursion
                   (set-buffer buf)
                   (set-buffer-modified-p nil)))
               (kill-buffer buf)))
    (and delete-frame (delete-frame))
  )
)

;;;###autoload
(defun kill-buffer-when-shell-command-exit ()
  "Close current buffer when `shell-command' exit."
  (let ((process (ignore-errors (get-buffer-process (current-buffer)))))
    (when process
      (set-process-sentinel process
                            (lambda (proc change)
                              (when (string-match "\\(finished\\|exited\\)" change)
                                (kill-buffer (process-buffer proc))))))))

;;;###autoload
(defun execute-command-on-file (file command)
  "execute COMMAND on FILE"
  (interactive
   (list (read-file-name "File execute command on: ")
         (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (if file
      (when (yes-or-no-p (concat command " file `" file "'?"))
        (shell-command (concat command " \"" file "\"")))
    (message "Executing command `%s'..." command)
    (shell-command command)))

;;;###autoload
(defun execute-command-on-current-file (command)
  "execute COMMAND on current BUFFER, if the BUFFER is related with a FILE,
  then execute `revert-buffer-no-confirm'"
  (interactive
   (list (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (let* ((file (buffer-file-name)))
    (execute-command-on-file file command)
    (if file
        (revert-buffer-no-confirm))))

;;;###autoload
(defun execute-command-on-current-dir (command)
  "execute COMMAND in current DIR."
  (interactive
   (list (let* ((input ""))
           (while (string= input "")
             (setq input (read-string "Command: ")))
           input)))
  (let* ((file (buffer-file-name)))
    (execute-command-on-file default-directory command)
    (if file
        (revert-buffer-no-confirm))))

;;;###autoload
(defmacro def-execute-command-on-file-command (command)
  "Make definition of command which execute command on file."
  `(defun ,(intern (subst-char-in-string ?\ ?- command)) (file)
     ,(concat "Run command `" command "' on file FILE.")
     (interactive (list (read-file-name (concat "File to " ,command ": "))))
     (execute-command-on-file file ,command)))

;;;###autoload
(defmacro def-execute-command-on-current-file-command (command)
  "Make definition of command which execute command on current file."
  `(defun ,(am-intern (subst-char-in-string ?\ ?- command) "-current-file") ()
     ,(concat "Execute command `" command "' on current file.")
     (interactive)
     (execute-command-on-current-file ,command)))

;;;###autoload
(defmacro def-execute-command-on-current-dir-command (command)
  "Make definition of command which execute command on current directory."
  `(defun ,(am-intern (subst-char-in-string ?\ ?- command) "-current-dir") ()
     ,(concat "Execute command `" command "' on current directory.")
     (interactive)
     (execute-command-on-current-dir ,command)))

(defun my-file-executable-p (file)
  "Make sure the file FILE exists and is executable."
  (if file
    (if (file-executable-p file)
          file
        (message "WARNING: Can't find executable `%s'" file)
        ;; sleep 1 s so that you can read the warning
        (sit-for 1))
    (error "my-file-executable-p: missing operand")
  )
)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun my-make-directory-yes-or-no (dir)
  "Ask user to create the DIR, if it does not already exist."
  (if dir
    (if (not (file-directory-p dir))
          (if (yes-or-no-p (concat "The directory `" dir
                                   "' does not exist currently. Create it? "))
              (make-directory dir t)
            (error
             (concat "Cannot continue without directory `" dir "'"))))
    (error "my-make-directory-yes-or-no: missing operand")
  )
)

(defun last-command-other-window()
  "Execute last command in other window."
  (interactive)
  (unless (memq last-command
                '(last-command-other-window
                  eval-defun
                  ))
    (other-window 1)
    (funcall last-command)
    (other-window 1)))

;; To overcome the fact that that we can't bytecompile the .emacs while it is being read
;;  (when (file-newer-than-file-p "~/.emacs" "~/.emacs.elc")
;;    (defun byte-compile-dotemacs-if-needed ()
;;      (when (y-or-n-p "byte-compiling .emacs to .emacs.elc?")
;;        (byte-compile-file "~/.emacs")
;;      )
;;      (remove-hook 'find-file-hooks 'byte-compile-dotemacs-if-needed)
;;    )
;;    (add-hook 'find-file-hooks 'byte-compile-dotemacs-if-needed)
;;  )

;;;###autoload
(defalias 'apply-define-key 'eal-define-keys-commonly)
;;;###autoload
(defalias 'define-key-list 'eal-define-keys-commonly)

;;;###autoload
(defun apply-args-list-to-fun (fun-list args-list)
  "Apply args list to function FUN-LIST.
FUN-LIST can be a symbol, also can be a list whose element is a symbol."
  (let ((is-list (and (listp fun-list) (not (functionp fun-list)))))
    (dolist (args args-list)
      (if is-list
          (dolist (fun fun-list)
            (apply-args-to-fun fun args))
        (apply-args-to-fun fun-list args)))))

;;;###autoload
(defun apply-args-to-fun (fun args)
  "Apply args to function FUN."
  (if (listp args)
      (eval `(,fun ,@args))
    (eval `(,fun ,args))))

;;;###autoload
(defun list-colors-display-htm (&optional list)
  "Create HTML page which lists all the defined colors."
  (interactive)
  (if (and (null list) window-system)
      (progn
        (setq list (x-defined-colors))
        ;; Delete duplicate colors.
        (let ((l list))
          (while (cdr l)
            (if (facemenu-color-equal (car l) (car (cdr l)))
                (setcdr l (cdr (cdr l)))
              (setq l (cdr l)))))))
  (with-output-to-temp-buffer "*Colors*"
    (save-excursion
      (set-buffer standard-output)
      (insert "<html>\n"
              "<head>\n"
              "<meta http-equiv=\"Content-Style-Type\" content=\"text/css\">\n"
              "<title>Colors</title>\n"
              "</head>\n"
              "<body>\n"
              "<h1>Colors</h1>\n"
              "<p>\n"
              "<pre>\n")
      (let (s)
        (while list
          (insert (format (concat "<span style=\"background-color:%s\">%-20s</span>"
                                  "  "
                                  "<span style=\"color:%s\">%s</span>"
                                  "\n")
                          (html-color (car list)) (car list)
                          (html-color (car list)) (car list)))
          (setq list (cdr list))))
      (insert "</pre>"
              "</body>"
              "</html>"))))

;;;###autoload
(defun html-color (string)
  "Convert colors names to rgb(n1,n2,n3) strings."
  (format "rgb(%d,%d,%d)"
          (/ (nth 0 (x-color-values string)) 256)
          (/ (nth 1 (x-color-values string)) 256)
          (/ (nth 2 (x-color-values string)) 256)))

;;;###autoload
(defmacro def-command-max-window (command)
  "Make definition of command which after execute command COMMAND
   execute `delete-other-windows'."
  `(defun ,(am-intern command "-max-window") ()
     ,(concat "After run command `" command "' execute command `delete-other-windows'.")
     (interactive)
     (call-interactively ',(intern command))
     (delete-other-windows)))

;;;###autoload
(defun delete-current-window (&optional frame)
  "Delete window which showing current buffer."
  (interactive
   (list (and current-prefix-arg
              (or (natnump (prefix-numeric-value current-prefix-arg))
                  'visible))))
  (if (one-window-p)
      (bury-buffer)
    (delete-windows-on (current-buffer) frame)))

;;;###autoload
(defmacro def-turn-on (command &optional is-on)
  "Make definition of command whose name is COMMAND-on when IS-ON is t
   and COMMAND-off when IS-ON is nil."
  (let ((on (if is-on "on" "off")))
    `(defun ,(am-intern command "-" on) ()
       ,(concat "Turn " on " `" command "'.")
       (interactive)
       (funcall ',(intern command) ,(if is-on 1 -1)))))

;;;###autoload
(defun unset-key (keymap key)
  "Remove binding of KEY in map KEYMAP.
   KEY is a string or vector representing a sequence of keystrokes."
  (define-key keymap key nil))

;;; === count characters in a region ===
(defun my-count-region (beginPos endPos)
  "Print number of words and chars in region."
  (interactive "r")
  (message "Counting ...")
  (save-excursion
    (let (wCnt charCnt)
      (setq wCnt 0)
      (setq charCnt (- endPos beginPos))
      (goto-char beginPos)
      (while (and (< (point) endPos)
                  (re-search-forward "\\w+\\W*" endPos t))
             (setq wCnt (1+ wCnt))
      )
      (message "Words: %d. Chars: %d." wCnt charCnt)
    )
  )
)

;;; === update number ===
;; similar to "C-a" in vim
;;;###autoload
(defun ywb-html-preview-region (beg end)
  (interactive "r")
  (let ((file (make-temp-file "region-" nil ".html")))
    (write-region beg end file)
    (browse-url file)
  )
)
(defvar wcy-rotate-text-definations
  '(("[0-9]+" . (lambda (arg)
                  (format "%d" (+ arg (string-to-number (match-string 0))))))
    ("zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"))
  " a list of ROT text defination. each element is a defination.
    element can be a list of string or a cons. (REGEXP . func)
    if REGEXP matched, func is called with no args, return value is the next value."
)
(defun wcy-rotate-text-aux (arg)
  (catch 'break
    (mapc
     #'(lambda (def)
         (let ((regexp (if (functionp (cdr def))
                           (car def)
                         (mapconcat 'regexp-quote def "\\|")))
               (func (if (functionp (cdr def))
                         (cdr def)
                       #'(lambda (arg)
                           (let* ((len (length def))
                                  (rest (member (match-string 0) def))
                                  (pos (- len (length rest))))
                             (format "%s" (nth (mod (+ pos arg) len) def)))))))
           (if (re-search-forward regexp (line-end-position) t nil)
               (throw 'break (funcall func arg)))))
     wcy-rotate-text-definations)
    nil)
)

(defun my-rotate-text(arg)
  (interactive "p")
  (save-excursion
    (let ((x (wcy-rotate-text-aux arg)))
      (if x (replace-match x))
    )
  )
)
;;; ==== end of update number

;;; === save line ===
(defun my-kill-ring-save-line ()
  "Add current line to kill-ring "
  (interactive)
  (progn
    (save-excursion
      (beginning-of-line)
      (setq beg (point))
      (end-of-line)
      (kill-new (buffer-substring beg (point)))
    )
  )
)

;;; === open dired ===
(defun my-dired-open-in-current-buffer ()
  "Open the currently selected file/directory in the same buffer as this one."
  (interactive)
  (find-alternate-file (dired-get-filename))
)

;;; === Set The Size and Position of Emacs Frames ===
(defun my-arrange-frame (w h x y)
  "Set the width, height, and x/y position of the current frame"
  (let ((frame (selected-frame)))
    (delete-other-windows)
    (set-frame-position frame x y)
    (set-frame-size frame w h)
  )
)
;; (my-arrange-frame 70 80 2 22)

;;; === add executable to some files ===
(setq my-shebang-patterns
      (list "^#!/usr/.*/perl\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/python\\(\\( \\)\\|\\( .+ \\)\\)-w *.*"
            "^#!/usr/.*/sh"
            "^#!/usr/.*/csh"
            "^#!/usr/.*/csh -f"
            "^#!/usr/.*/bash"
            "^#!/bin/sh"
            "^#!/.*/perl"
            "^#!/.*/awk"
            "^#!/.*/sed"
            "^#!/bin/bash")
)
(add-hook 'after-save-hook
  (lambda ()
    (if (not (= (shell-command (concat "test -x " (buffer-file-name))) 0))
      (progn
        ;;This puts message in *Message* twice, but minibuffer output looks better
        (message (concat "Wrote " (buffer-file-name)))
        (save-excursion
          (goto-char (point-min))
          ;; Always checks every pattern even after match. Inefficient but easy
          (dolist (my-shebang-pat my-shebang-patterns)
            (if (looking-at my-shebang-pat)
              (if (= (shell-command
                (concat "chmod u+x " (buffer-file-name)))
                 0)
                (message (concat "Wrote and made executable " (buffer-file-name)))
              )
            )
          )
        )
      )
     ;; This puts message in *Message* twice, but minibuffer output looks better
     (message (concat "Wrote " (buffer-file-name)))
    )
  )
)

;; popup a terminal
(defun my-popup-term ()
  (interactive)
  (apply 'start-process "terminal" nil popup-terminal-command)
)

;; repeat last command passed to "shell-mode"
(defun repeat-shell-command ()
  "Repeat most recently executed shell command."
  (interactive)
  (save-buffer)
  (or shell-command-history (error "Nothing to repeat."))
  (shell-command (car shell-command-history)))
;; (global-set-key (kbd "C-c j") 'repeat-shell-command)

(defun his-imenu()
  "Call imenu, showing completions."
  (interactive)
  (setq unread-command-events (list 9))
  (imenu (imenu-choose-buffer-index)))

;;  save your last macro by typing M-x save-macro
(defun save-macro (name)
    "save a macro. Take a name as argument
     and save the last defined macro under
     this name at the end of your .emacs"
     (interactive "SName of the macro :")  ; ask for the name of the macro
     (kmacro-name-last-macro name)         ; use this name for the macro
     (find-file "~/.emacs")                ; open the .emacs file
     (goto-char (point-max))               ; go to the end of the .emacs
     (newline)                             ; insert a newline
     (insert-kbd-macro name)               ; copy the macro
     (newline)                             ; insert a newline
     (switch-to-buffer nil)                ; return to the initial buffer
     )

;; insert 128-bit random number
(defun my-insert-rand128 ()
  "Insert 128-bit random number (in hex) at point."
  (interactive)
  (shell-command
   "dd if=/dev/urandom count=1 2>/dev/null | md5sum -b | cut -d' ' -f1 | tr -d '\n' "
   (quote (4)) nil)
  )

;; note: may use delete-trailing-whitespace
(defun jj-delete-trailing-spaces ()
  "Delete trailing spaces (or tabs) in all lines."
  (interactive)
  (progn
    (save-excursion
      (goto-line 1) (replace-regexp "[ \t]+$" "") ) ) )

(defun my-nsplit-line (n)
  "Split line into pieces of length N."
  (interactive "nSplit into pieces of length n: ")
  (progn
    (if (< n 1) (error "n must be greater than zero"))
    (let ((beg) (end) (stp))
      (end-of-line)
      (setq end (point))
      (beginning-of-line)
      (setq beg (point))
      (setq stp (point))
      (while (< beg end)
        (goto-column n)
        (insert "\n")
        (setq beg (+ n beg)) )
      (goto-char stp) )
    )
)

;;
(defun my-count-matches-region (r)
  "Count occurences of REGEXP in region."
 (interactive "s Enter regexp: ") ; elips.ps.gz p.335
  (progn
;;    (message "%d" (region-beginning))
;;    (message "%d" (region-end))
;;    (message "%S" r)
    (how-many r (region-beginning) (region-end))
    )
  )


