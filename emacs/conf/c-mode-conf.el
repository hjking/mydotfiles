
(message ">>>>> Loading [ C Mode ] Customizations ....")
(defun my-c-startup ()
  "C mode with adjusted defaults for use with the Linux kernal formatting."
  (interactive)
  (c-mode)
  (c-set-style "K&R")
  (setq tab-width 8)
  (setq indent-tabs-mode nil)
  (setq c-basic-offset 8)
  (setq comment-column 40)
  (setq compile-command "make")
)

(defun linux-c-mode ()
  "C mode with adjusted defaults for use with the Linux kernel."
   (interactive)
   (c-mode)
   (c-set-style "K&amp;R")
   (setq tab-width 4)
   (setq indent-tabs-mode t)
   (setq c-basic-offset 4))

(add-hook 'c-mode-hook 'my-c-startup)
