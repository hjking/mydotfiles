
;; Filename: vlog-conf.el
;; Description: Setting for vlog-mode.el
;; Author: Hong Jin
;; Created: 2010-12-22 10:00
;; Last Updated: 2011-01-21 10:20:06

(message ">>>>> Loading [ Vlog Mode ] Customizations ....")

(require 'vlog-mode)

;; (add-to-list 'auto-mode-alist '("\\.sv\\'" . vlog-mode))
;; (add-to-list 'auto-mode-alist '("\\.v\\'" . vlog-mode))
;; (add-to-list 'auto-mode-alist '("\\.vp\\'" . vlog-mode))
;; (add-to-list 'auto-mode-alist '("\\.vh\\'" . vlog-mode))
;; (add-to-list 'auto-mode-alist '("\\.vl\\'" . vlog-mode))

(vlog-mode-enable-v2k)

(setq vlog-mode-highlight-all-uppercase-words t)

(setq vlog-align-mod-inst-stop-list '(28 52))

(setq vlog-indent-level-beh                2
      vlog-indent-level-block              0
      vlog-indent-level-block-beh          0
      vlog-indent-level-block-inside       2
      vlog-indent-level-case-inside        4
      vlog-indent-level-case-branch-inside 2
      vlog-indent-level-cond               2
      vlog-indent-level-default            2
      vlog-indent-level-port-list          4)

(setq vlog-mode-keywordset-docs
      (append vlog-mode-keywordset-docs
              (list "Note:" "NOTE:" "note:")))
(vlog-mode-make-keywords)

(setq vlog-skel-header-string "\
/*******************************************************************************
 * Copyright (C) %<time %Y> by %<company>
 *
 * -----------------------------------------------------------------------------
 * File Name      : %<filename>
 * Module Name    : %<modulename>
 * Author         : %<author>
 * Created        : %<time %Y-%m-%d %02H:%02M:%02S>
 * Last Modified  : %<time %Y-%m-%d %02H:%02M:%02S>
 * -----------------------------------------------------------------------------
 * ID             : $Id$
 * -----------------------------------------------------------------------------
 * [Reference]    : %<_>
 * [Description]
 * %<_>
 * -----------------------------------------------------------------------------
 * [Revision History]
 * Version    Date        Author      Description
 * 1.0%<align>%<time %Y-%m-%d>%<align>%<author>%<align>1. Initial revision
 * -----------------------------------------------------------------------------
 *
 ******************************************************************************/\n")

(setq vlog-skel-user-name    "Hong Jin"
      vlog-skel-company-name "Founder International Software(Wuhan) Co.,Ltd")

