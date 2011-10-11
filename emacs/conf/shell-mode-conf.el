
(message ">>>>> Loading [ Shell Mode ] Customizations ....")
(when linuxp
    (setq shell-file-name "/bin/bash"))
(when win32p
    (setq shell-file-name "C:/cygwin/bin/bash.exe"))
;; (setq popup-terminal-command '("/bin/bash"))

;; close shell buffer when "exit"
(setq comint-use-prompt-regexp-instead-of-fields nil)
(autoload 'ansi-color-for-comint-mode-on "ansi-color" nil t)
;; close when exit shell
(defun my-shell-mode-hook-func  ()
  (set-process-sentinel (get-buffer-process (current-buffer))
                            #'my-shell-mode-kill-buffer-on-exit)
)
(defun my-shell-mode-kill-buffer-on-exit (process state)
  (message "%s" state)
  (if (or
        (string-match "exited abnormally with code.*" state)
        (string-match "finished" state))
        (kill-buffer (current-buffer)
      )
  )
)

(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on t)
(add-hook 'shell-mode-hook 'my-shell-mode-hook-func)
(add-hook 'term-mode-hook 'my-shell-mode-hook-func)
