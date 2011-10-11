
;; Filename: verilog-conf.el
;; Description: Setting for verilog.el
;; Author: Hong Jin
;; Created: 2010-12-09 10:00
;; Last Updated: 2010-12-22 13:35:28

(message ">>>>> Loading [ Verilog Mode ] Customizations ....")
;; load verilog mode only when needed
(autoload 'verilog-mode "verilog-mode" "Verilog mode" t)
(add-to-list 'auto-mode-alist '("\\.[ds]?\\(v\\|vp\\)\\'" . verilog-mode))
;; (setq auto-mode-alist (cons  '("\\.v\\'" . verilog-mode) auto-mode-alist))
;; any file in verilog mode should have their keywords colorized
(add-hook 'verilog-mode-hook '(lambda () (font-lock-mode 1)))
(setq verilog-indent-level             4
      verilog-indent-level-module      4
      verilog-indent-level-declaration 4
      verilog-indent-level-behavioral  4
      verilog-indent-level-directive   2
      verilog-case-indent              4
      verilog-auto-newline             t
      verilog-auto-indent-on-newline   t
      verilog-tab-always-indent        nil
      verilog-auto-endcomments         t
      verilog-minimum-comment-distance 40
      verilog-indent-begin-after-if    t
      verilog-auto-lineup              'declarations
      verilog-highlight-p1800-keywords nil
      ;; Personal
      verilog-company                 "Founder International Software(Wuhan) Co.,Ltd"
      verilog-linter             "vcs +v2k -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+."
      verilog-compiler           "vcs +v2k -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+."
      verilog-simulator          "vcs +v2k  -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+."
      verilog-tool               "vcs +v2k  -R -PP -Mupdate -P /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/vcsd.tab /cadtools/novas/Novas-201001/share/PLI/vcsd_latest/LINUX/pli.a +vcsd +vcsd +incdir+."
)

;; Convert all tabs in region to multiple spaces
(add-hook 'verilog-mode-hook '(lambda () (add-hook 'local-write-file-hooks (lambda() (untabify (point-min) (point-max))))))

