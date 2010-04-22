;;; anything-config.el --- Predefined configurations for `anything.el'

;; Filename: anything-config.el

;; Description: Predefined configurations for `anything.el'
;; Author: Tassilo Horn <tassilo@member.fsf.org>
;; Maintainer: Tassilo Horn <tassilo@member.fsf.org>
;;             rubikitch    <rubikitch@ruby-lang.org>
;;             Thierry Volpiatto <thierry.volpiatto@gmail.com>
;; Copyright (C) 2007 ~ 2010, Tassilo Horn, all rights reserved.
;; Copyright (C) 2009, Andy Stewart, all rights reserved.
;; Copyright (C) 2009 ~ 2010, rubikitch, all rights reserved.
;; Copyright (C) 2009 ~ 2010, Thierry Volpiatto, all rights reserved.
;; Created: 2009-02-16 21:38:23
;; Version: 0.4.1
;; URL: http://www.emacswiki.org/emacs/download/anything-config.el
;; Keywords: anything, anything-config
;; Compatibility: GNU Emacs 22 ~ 23
;;
;; Features that might be required by this library:
;;
;; `anything'
;;

;;; This file is NOT part of GNU Emacs

;;; License
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.

;;; !NOTICE!
;;
;; If this file does not work, upgrade anything.el!
;; http://www.emacswiki.org/cgi-bin/wiki/download/anything.el

;;; Commentary:
;;
;; Predefined configurations for `anything.el'
;;
;; For quick start, try `anything-for-files' to open files.
;; 
;; To configure anything you should define anything command
;; with your favorite sources, like below:
;;
;; (defun my-anything ()
;;   (interactive)
;;   (anything-other-buffer
;;    '(anything-c-source-buffers
;;      anything-c-source-file-name-history
;;      anything-c-source-info-pages
;;      anything-c-source-info-elisp
;;      anything-c-source-man-pages
;;      anything-c-source-locate
;;      anything-c-source-emacs-commands)
;;    " *my-anything*"))
;;
;; Then type M-x my-anything to use sources.
;;
;; Defining own command is better than setup `anything-sources'
;; directly, because you can define multiple anything commands with
;; different sources. Each anything command should have own anything
;; buffer, because M-x anything-resume revives anything command.

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-c-describe-anything-bindings'
;;    [OBSOLETE] Describe `anything' bindings.
;;  `anything-for-files'
;;    Preconfigured `anything' for opening files.
;;  `anything-info-at-point'
;;    Preconfigured `anything' for searching info at point.
;;  `anything-show-kill-ring'
;;    Preconfigured `anything' for `kill-ring'. It is drop-in replacement of `yank-pop'.
;;  `anything-minibuffer-history'
;;    Preconfigured `anything' for `minibuffer-history'.
;;  `anything-gentoo'
;;    Preconfigured `anything' for gentoo linux.
;;  `anything-surfraw-only'
;;    Preconfigured `anything' for surfraw.
;;  `anything-imenu'
;;    Preconfigured `anything' for `imenu'.
;;  `anything-google-suggest'
;;    Preconfigured `anything' for google search with google suggest.
;;  `anything-yahoo-suggest'
;;    Preconfigured `anything' for Yahoo searching with Yahoo suggest.
;;  `anything-for-buffers'
;;    Preconfigured `anything' for buffer.
;;  `anything-bbdb'
;;    Preconfigured `anything' for BBDB.
;;  `anything-locate'
;;    Preconfigured `anything' for Locate.
;;  `anything-w3m-bookmarks'
;;    Preconfigured `anything' for w3m bookmark.
;;  `anything-colors'
;;    Preconfigured `anything' for color.
;;  `anything-bm-list'
;;    Preconfigured `anything' for visible bookmarks.
;;  `anything-timers'
;;    Preconfigured `anything' for timers.
;;  `anything-kill-buffers'
;;    Preconfigured `anything' to kill buffer you selected.
;;  `anything-query-replace-regexp'
;;    Preconfigured `anything' : Drop-in replacement of `query-replace-regexp' with building regexp visually.
;;  `anything-regexp'
;;    Preconfigured `anything' : It is like `re-builder'. It helps buliding regexp and replacement.
;;  `anything-insert-buffer-name'
;;    Insert buffer name.
;;  `anything-insert-symbol'
;;    Insert current symbol.
;;  `anything-insert-selection'
;;    Insert current selection.
;;  `anything-show-buffer-only'
;;    [OBSOLETE] Only show sources about buffer.
;;  `anything-show-bbdb-only'
;;    [OBSOLETE] Only show sources about BBDB.
;;  `anything-show-locate-only'
;;    [OBSOLETE] Only show sources about Locate.
;;  `anything-show-info-only'
;;    [OBSOLETE] Only show sources about Info.
;;  `anything-show-imenu-only'
;;    [OBSOLETE] Only show sources about Imenu.
;;  `anything-show-files-only'
;;    [OBSOLETE] Only show sources about File.
;;  `anything-show-w3m-bookmarks-only'
;;    [OBSOLETE] Only show source about w3m bookmark.
;;  `anything-show-colors-only'
;;    [OBSOLETE] Only show source about color.
;;  `anything-show-kill-ring-only'
;;    [OBSOLETE] Only show source about kill ring.
;;  `anything-show-this-source-only'
;;    Only show this source.
;;  `anything-test-sources'
;;    List all anything sources for test.
;;  `anything-select-source'
;;    Select source.
;;  `anything-find-files-down-one-level'
;;    Go down one level like unix command `cd ..'.
;;  `anything-find-files'
;;    Preconfigured `anything' for anything implementation of `find-file'.
;;  `anything-write-file'
;;    Preconfigured `anything' providing completion for `write-file'.
;;  `anything-insert-file'
;;    Preconfigured `anything' providing completion for `insert-file'.
;;  `anything-dired-rename-file'
;;    Preconfigured `anything' to rename files from dired.
;;  `anything-dired-copy-file'
;;    Preconfigured `anything' to copy files from dired.
;;  `anything-dired-symlink-file'
;;    Preconfigured `anything' to symlink files from dired.
;;  `anything-dired-hardlink-file'
;;    Preconfigured `anything' to hardlink files from dired.
;;  `anything-dired-bindings'
;;    Replace usual dired commands `C' and `R' by anything ones.
;;  `anything-bookmark-ext'
;;    Preconfigured `anything' for bookmark-extensions sources.
;;  `anything-simple-call-tree'
;;    Preconfigured `anything' for simple-call-tree. List function relationships.
;;  `anything-mark-ring'
;;    Preconfigured `anything' for `anything-c-source-mark-ring'.
;;  `anything-global-mark-ring'
;;    Preconfigured `anything' for `anything-c-source-global-mark-ring'.
;;  `anything-yaoddmuse-cache-pages'
;;    Fetch the list of files on emacswiki and create cache file.
;;  `anything-yaoddmuse-emacswiki-edit-or-view'
;;    Preconfigured `anything' to edit or view EmacsWiki page.
;;  `anything-yaoddmuse-emacswiki-post-library'
;;    Preconfigured `anything' to post library to EmacsWiki.
;;  `anything-emms-stream-edit-bookmark'
;;    Change the information of current emms-stream bookmark from anything.
;;  `anything-emms-stream-delete-bookmark'
;;    Delete an emms-stream bookmark from anything.
;;  `anything-call-source'
;;    Preconfigured `anything' to call anything source.
;;  `anything-call-source-from-anything'
;;    Call anything source within `anything' session.
;;  `anything-occur'
;;    Preconfigured Anything for Occur source.
;;  `anything-create-from-anything'
;;    Run `anything-create' from `anything' as a fallback.
;;  `anything-create'
;;    Preconfigured `anything' to do many create actions from STRING.
;;  `anything-top'
;;    Preconfigured `anything' for top command.
;;  `anything-select-xfont'
;;    Preconfigured `anything' to select Xfont.
;;  `anything-apt'
;;    Preconfigured `anything' : frontend of APT package manager.
;;  `anything-c-set-variable'
;;    Set value to VAR interactively.
;;  `anything-c-adaptive-save-history'
;;    Save history information to file given by `anything-c-adaptive-history-file'.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-c-use-standard-keys'
;;    Whether use standard keybindings. (no effect)
;;    default = nil
;;  `anything-c-adaptive-history-file'
;;    Path of file where history information is stored.
;;    default = "~/.emacs.d/anything-c-adaptive-history"
;;  `anything-c-adaptive-history-length'
;;    Maximum number of candidates stored for a source.
;;    default = 50
;;  `anything-c-google-suggest-url'
;;    URL used for looking up Google suggestions.
;;    default = "http://google.com/complete/search?output=toolbar&q="
;;  `anything-c-google-suggest-search-url'
;;    URL used for Google searching.
;;    default = "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
;;  `anything-google-suggest-use-curl-p'
;;    *When non--nil use CURL to get info from `anything-c-google-suggest-url'.
;;    default = nil
;;  `anything-c-yahoo-suggest-url'
;;    Url used for looking up Yahoo suggestions.
;;    default = "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
;;  `anything-c-yahoo-suggest-search-url'
;;    Url used for Yahoo searching.
;;    default = "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
;;  `anything-c-boring-buffer-regexp'
;;    The regexp that match boring buffers.
;;    default = (rx (or (group bos " ") "*anything" " *Echo Area" " *Minibuf"))
;;  `anything-c-boring-file-regexp'
;;    The regexp that match boring files.
;;    default = (rx (or (and "/" ... ...) (and line-start ".#") (and ... eol)))
;;  `anything-kill-ring-threshold'
;;    *Minimum length to be listed by `anything-c-source-kill-ring'.
;;    default = 10
;;  `anything-su-or-sudo'
;;    What command to use for root access.
;;    default = "su"
;;  `anything-for-files-prefered-list'
;;    Your prefered sources to find files.
;;    default = (quote (anything-c-source-ffap-line anything-c-source-ffap-guesser anything-c-source-buffers+ anything-c-source-recentf anything-c-source-bookmarks ...))
;;  `anything-create--actions-private'
;;    User defined actions for `anything-create' / `anything-c-source-create'.
;;    default = nil
;;  `anything-allow-skipping-current-buffer'
;;    Show current buffer or not in anything buffer
;;    default = t
;;  `anything-c-enable-eval-defun-hack'
;;    *If non-nil, execute `anything' using the source at point when C-M-x is pressed.
;;    default = t
;;  `anything-tramp-verbose'
;;    *Just like `tramp-verbose' but specific to anything.
;;    default = 0

;;
;; Anything sources can be tested by M-x `anything-call-source'.
;; Below are complete source list you can setup in the first argument
;;
;;  Buffer:
;;     `anything-c-source-buffers'		(Buffers)
;;     `anything-c-source-buffer-not-found'	(Create buffer)
;;     `anything-c-source-buffers+'		(Buffers)
;;  File:
;;     `anything-c-source-file-name-history'		(File Name History)
;;     `anything-c-source-files-in-current-dir'		(Files from Current Directory)
;;     `anything-c-source-files-in-current-dir+'	(Files from Current Directory)
;;     `anything-c-source-find-files'			(Find Files (`C-.':Go to precedent level))
;;     `anything-c-source-file-cache'			(File Cache)
;;     `anything-c-source-locate'			(Locate)
;;     `anything-c-source-recentf'			(Recentf)
;;     `anything-c-source-ffap-guesser'			(File at point)
;;     `anything-c-source-ffap-line'			(File/Lineno at point)
;;     `anything-c-source-files-in-all-dired'		(Files in all dired buffer.)
;;  info:
;;     `anything-c-source-info-pages'			(Info Pages)
;;     `anything-c-source-info-elisp'			(Info index: elisp)
;;     `anything-c-source-info-cl'			(Info index: cl)
;;     `anything-c-source-info-org'			(Info index: org)
;;     `anything-c-source-info-ratpoison'		(Info index: ratpoison)
;;     `anything-c-source-info-zsh'			(Info index: zsh)
;;     `anything-c-source-info-bash'			(Info index: bash)
;;     `anything-c-source-info-coreutils'		(Info index: coreutils)
;;     `anything-c-source-info-fileutils'		(Info index: fileutils)
;;     `anything-c-source-info-find'			(Info index: find)
;;     `anything-c-source-info-sh-utils'		(Info index: sh-utils)
;;     `anything-c-source-info-textutils'		(Info index: textutils)
;;     `anything-c-source-info-libc'			(Info index: libc)
;;     `anything-c-source-info-make'			(Info index: make)
;;     `anything-c-source-info-automake'		(Info index: automake)
;;     `anything-c-source-info-autoconf'		(Info index: autoconf)
;;     `anything-c-source-info-emacs-lisp-intro'	(Info index: emacs-lisp-intro)
;;     `anything-c-source-info-emacs'			(Info index: emacs)
;;     `anything-c-source-info-elib'			(Info index: elib)
;;     `anything-c-source-info-eieio'			(Info index: eieio)
;;     `anything-c-source-info-gauche-refe'		(Info index: gauche)
;;     `anything-c-source-info-guile'			(Info index: guile)
;;     `anything-c-source-info-guile-tut'		(Info index: guile-tut)
;;     `anything-c-source-info-goops'			(Info index: goops)
;;     `anything-c-source-info-screen'			(Info index: screen)
;;     `anything-c-source-info-latex'			(Info index: latex)
;;     `anything-c-source-info-gawk'			(Info index: gawk)
;;     `anything-c-source-info-sed'			(Info index: sed)
;;     `anything-c-source-info-m4'			(Info index: m4)
;;     `anything-c-source-info-wget'			(Info index: wget)
;;     `anything-c-source-info-binutils'		(Info index: binutils)
;;     `anything-c-source-info-as'			(Info index: as)
;;     `anything-c-source-info-bfd'			(Info index: bfd)
;;     `anything-c-source-info-gprof'			(Info index: gprof)
;;     `anything-c-source-info-ld'			(Info index: ld)
;;     `anything-c-source-info-diff'			(Info index: diff)
;;     `anything-c-source-info-flex'			(Info index: flex)
;;     `anything-c-source-info-grep'			(Info index: grep)
;;     `anything-c-source-info-gzip'			(Info index: gzip)
;;     `anything-c-source-info-libtool'			(Info index: libtool)
;;     `anything-c-source-info-texinfo'			(Info index: texinfo)
;;     `anything-c-source-info-info'			(Info index: info)
;;     `anything-c-source-info-gdb'			(Info index: gdb)
;;     `anything-c-source-info-stabs'			(Info index: stabs)
;;     `anything-c-source-info-cvsbook'			(Info index: cvsbook)
;;     `anything-c-source-info-cvs'			(Info index: cvs)
;;     `anything-c-source-info-bison'			(Info index: bison)
;;     `anything-c-source-info-id-utils'		(Info index: id-utils)
;;     `anything-c-source-info-global'			(Info index: global)
;;  Help:
;;     `anything-c-source-man-pages'    (Manual Pages)
;;  Command:
;;     `anything-c-source-complex-command-history'	(Complex Command History)
;;     `anything-c-source-extended-command-history'	(Emacs Commands History)
;;     `anything-c-source-emacs-commands'		(Emacs Commands)
;;     `anything-c-source-lacarte'			(Lacarte)
;;  Function:
;;     `anything-c-source-emacs-functions'		(Emacs Functions)
;;     `anything-c-source-emacs-functions-with-abbrevs' (Emacs Functions)
;;  Variable:
;;     `anything-c-source-emacs-variables'	(Emacs Variables)
;;  Bookmark:
;;     `anything-c-source-bookmarks'			(Bookmarks)
;;     `anything-c-source-bookmark-set'			(Set Bookmark)
;;     `anything-c-source-bookmarks-ssh'		(Bookmarks-ssh)
;;     `anything-c-source-bookmarks-su'			(Bookmarks-root)
;;     `anything-c-source-bookmarks-local'		(Bookmarks-Local)
;;     `anything-c-source-bookmark-regions'		(Bookmark Regions)
;;     `anything-c-source-bookmark-w3m'			(Bookmark W3m)
;;     `anything-c-source-bookmark-man'			(Bookmark Woman&Man)
;;     `anything-c-source-bookmark-gnus'		(Bookmark Gnus)
;;     `anything-c-source-bookmark-info'		(Bookmark Info)
;;     `anything-c-source-bookmark-files&dirs'		(Bookmark Files&Directories)
;;     `anything-c-source-bookmark-su-files&dirs'	(Bookmark Root-Files&Directories)
;;     `anything-c-source-bookmark-ssh-files&dirs'	(Bookmark Ssh-Files&Directories)
;;     `anything-c-source-firefox-bookmarks'		(Firefox Bookmarks)
;;     `anything-c-source-w3m-bookmarks'		(W3m Bookmarks)
;;  Library:
;;     `anything-c-source-elisp-library-scan'   (Elisp libraries (Scan))
;;  Programming:
;;     `anything-c-source-imenu'				(Imenu)
;;     `anything-c-source-ctags'				(Exuberant ctags)
;;     `anything-c-source-semantic'				(Semantic Tags)
;;     `anything-c-source-simple-call-tree-functions-callers'   (Function is called by)
;;     `anything-c-source-simple-call-tree-callers-functions'   (Function calls)
;;     `anything-c-source-commands-and-options-in-file'		(Commands/Options in file)
;;  Color and Face:
;;     `anything-c-source-customize-face'	(Customize Face)
;;     `anything-c-source-colors'		(Colors)
;;  Search Engine:
;;     `anything-c-source-tracker-search'	(Tracker Search)
;;     `anything-c-source-mac-spotlight'	(mdfind)
;;  Kill ring:
;;     `anything-c-source-kill-ring'    (Kill Ring)
;;  Mark ring:
;;     `anything-c-source-mark-ring'		(mark-ring)
;;     `anything-c-source-global-mark-ring'	(global-mark-ring)
;;  Register:
;;     `anything-c-source-register'	(Registers)
;;  Headline Extraction:
;;     `anything-c-source-fixme'				(TODO/FIXME/DRY comments)
;;     `anything-c-source-rd-headline'				(RD HeadLine)
;;     `anything-c-source-oddmuse-headline'			(Oddmuse HeadLine)
;;     `anything-c-source-emacs-source-defun'			(Emacs Source DEFUN)
;;     `anything-c-source-emacs-lisp-expectations'		(Emacs Lisp Expectations)
;;     `anything-c-source-emacs-lisp-toplevels'			(Emacs Lisp Toplevel / Level 4 Comment / Linkd Star)
;;     `anything-c-source-org-headline'				(Org HeadLine)
;;     `anything-c-source-yaoddmuse-emacswiki-edit-or-view'	(Yaoddmuse Edit or View (EmacsWiki))
;;     `anything-c-source-yaoddmuse-emacswiki-post-library'	(Yaoddmuse Post library (EmacsWiki))
;;     `anything-c-source-eev-anchor'				(Anchors)
;;  Misc:
;;     `anything-c-source-org-keywords'		(Org Keywords)
;;     `anything-c-source-picklist'		(Picklist)
;;     `anything-c-source-bbdb'			(BBDB)
;;     `anything-c-source-evaluation-result'    (Evaluation Result)
;;     `anything-c-source-calculation-result'   (Calculation Result)
;;     `anything-c-source-google-suggest'	(Google Suggest)
;;     `anything-c-source-yahoo-suggest'	(Yahoo Suggest)
;;     `anything-c-source-surfraw'		(Surfraw)
;;     `anything-c-source-emms-streams'		(Emms Streams)
;;     `anything-c-source-emms-dired'		(Music Directory)
;;     `anything-c-source-jabber-contacts'	(Jabber Contacts)
;;     `anything-c-source-call-source'		(Call anything source)
;;     `anything-c-source-occur'		(Occur)
;;     `anything-c-source-create'		(Create)
;;     `anything-c-source-minibuffer-history'   (Minibuffer History)
;;     `anything-c-source-elscreen'		(Elscreen)
;;  System:
;;     `anything-c-source-top'				(Top (Press C-c C-u to refresh))
;;     `anything-c-source-absolute-time-timers'		(Absolute Time Timers)
;;     `anything-c-source-idle-time-timers'		(Idle Time Timers)
;;     `anything-c-source-xrandr-change-resolution'	(Change Resolution)
;;     `anything-c-source-xfonts'			(X Fonts)
;;     `anything-c-source-apt'				(APT)
;;     `anything-c-source-gentoo'			(Portage sources)
;;     `anything-c-source-use-flags'			(Use Flags)
;;     `anything-c-source-emacs-process'		(Emacs Process)

;;; Change log:
;;
;;  Change log of this file is found at
;;  http://repo.or.cz/w/anything-config.git?a=shortlog

;;; Contributors:
;;
;;     Tamas Patrovics
;;     Tassilo Horn <tassilo@member.fsf.org>
;;     Vagn Johansen <gonz808@hotmail.com>
;;     Mathias Dahl <mathias.dahl@gmail.com>
;;     Bill Clementson <billclem@gmail.com>
;;     Stefan Kamphausen (see http://www.skamphausen.de for more informations)
;;     Drew Adams <drew.adams@oracle.com>
;;     Jason McBrayer <jmcbray@carcosa.net>
;;     Andy Stewart <lazycat.manatee@gmail.com>
;;     Thierry Volpiatto <thierry.volpiatto@gmail.com>
;;     rubikitch <rubikitch@ruby-lang.org>
;;     Scott Vokes <vokes.s@gmail.com>
;;

;;; For Maintainers:
;;
;; Evaluate (anything-c-insert-summary) before commit. This function
;; generates anything-c-source-* list.
;;
;; Install also http://www.emacswiki.org/emacs/auto-document.el
;; And eval it or run interactively.
;;
;; [EVAL IT] (anything-c-insert-summary)
;; [EVAL IT] (auto-document)
;;
;; Please write details documentation about function, then others will
;; read code more easier.   -- Andy Stewart
;;


;;; TODO
;;
;; - Fix documentation, now many functions haven't documentations.
;;

;;; Require
(require 'anything)
(require 'thingatpt)
(require 'ffap)
(require 'cl)

;;; Code:

;; version check
(let ((version "1.263"))
  (when (and (string= "1." (substring version 0 2))
             (string-match "1\.\\([0-9]+\\)" anything-version)
             (< (string-to-number (match-string 1 anything-version))
                (string-to-number (substring version 2))))
    (error "Please update anything.el!!

http://www.emacswiki.org/cgi-bin/wiki/download/anything.el

or  M-x install-elisp-from-emacswiki anything.el")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Customize ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defgroup anything-config nil
  "Predefined configurations for `anything.el'."
  :group 'anything)

(defcustom anything-c-use-standard-keys nil
  "Whether use standard keybindings. (no effect)

Key definitions in anything-config.el are removed because
anything.el uses Emacs-standard keys by default. e.g. M-p/M-n for
minibuffer history, C-s for isearch, etc.

If you use `iswitchb' with `anything',
evaluate (anything-iswitchb-setup) .  Then some bindings that
conflict with `iswitchb', e.g. C-p/C-n for the minibuffer
history, are removed from `anything-map'. "
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-adaptive-history-file "~/.emacs.d/anything-c-adaptive-history"
  "Path of file where history information is stored."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-adaptive-history-length 50
  "Maximum number of candidates stored for a source."
  :type 'number
  :group 'anything-config)

(defcustom anything-c-google-suggest-url
  "http://google.com/complete/search?output=toolbar&q="
  "URL used for looking up Google suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-google-suggest-search-url
  "http://www.google.com/search?ie=utf-8&oe=utf-8&q="
  "URL used for Google searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-google-suggest-use-curl-p nil
  "*When non--nil use CURL to get info from `anything-c-google-suggest-url'.
Otherwise `url-retrieve-synchronously' is used."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-yahoo-suggest-url
  "http://search.yahooapis.com/WebSearchService/V1/relatedSuggestion?appid=Generic&query="
  "Url used for looking up Yahoo suggestions."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-yahoo-suggest-search-url
  "http://search.yahoo.com/search?&ei=UTF-8&fr&h=c&p="
  "Url used for Yahoo searching."
  :type 'string
  :group 'anything-config)

(defcustom anything-c-boring-buffer-regexp
  (rx (or
       (group bos  " ")
       ;; anything-buffer
       "*anything"
       ;; echo area
       " *Echo Area" " *Minibuf"))
  "The regexp that match boring buffers.
Buffer candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-buffers' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-buffers' is used."
  :type 'string
  :group 'anything-config)
;; (string-match anything-c-boring-buffer-regexp "buf")
;; (string-match anything-c-boring-buffer-regexp " hidden")
;; (string-match anything-c-boring-buffer-regexp " *Minibuf-1*")

(defcustom anything-c-boring-file-regexp
  (rx (or
       ;; Boring directories
       (and "/" (or ".svn" "CVS" "_darcs" ".git" ".hg") (or "/" eol))
       ;; Boring files
       (and line-start  ".#")
       (and (or ".class" ".la" ".o" "~") eol)))
  "The regexp that match boring files.
File candidates matching this regular expression will be
filtered from the list of candidates if the
`anything-c-skip-boring-files' candidate transformer is used, or
they will be displayed with face `file-name-shadow' if
`anything-c-shadow-boring-files' is used."
  :type 'string
  :group 'anything-config)

(defcustom anything-kill-ring-threshold 10
  "*Minimum length to be listed by `anything-c-source-kill-ring'."
  :type 'integer
  :group 'anything-config)

(defcustom anything-su-or-sudo "su"
  "What command to use for root access."
  :type 'string
  :group 'anything-config)

(defcustom anything-for-files-prefered-list '(anything-c-source-ffap-line
                                              anything-c-source-ffap-guesser
                                              anything-c-source-buffers+
                                              anything-c-source-recentf
                                              anything-c-source-bookmarks
                                              anything-c-source-file-cache
                                              anything-c-source-files-in-current-dir+
                                              anything-c-source-locate)
  "Your prefered sources to find files."
  :type 'list
  :group 'anything-config)

(defcustom anything-create--actions-private nil
  "User defined actions for `anything-create' / `anything-c-source-create'.
It is a list of (DISPLAY . FUNCTION) pairs like `action'
attribute of `anything-sources'.

It is prepended to predefined pairs."
  :type 'list
  :group 'anything-config)

(defcustom anything-allow-skipping-current-buffer t
  "Show current buffer or not in anything buffer"
  :type 'boolean
  :group 'anything-config)

(defcustom anything-c-enable-eval-defun-hack t
  "*If non-nil, execute `anything' using the source at point when C-M-x is pressed.
This hack is invoked when pressing C-M-x in the form (defvar anything-c-source-XXX ...) or (setq anything-c-source-XXX ...)."
  :type 'boolean
  :group 'anything-config)

(defcustom anything-tramp-verbose 0
  "*Just like `tramp-verbose' but specific to anything.
When set to 0 don't show tramp messages in anything.
If you want to have the default tramp messages set it to 3."
  :type 'integer
  :group 'anything-config)

;;; Documentation
;; It is replaced by `anything-help'
(defun anything-c-describe-anything-bindings ()
  "[OBSOLETE] Describe `anything' bindings."
  (interactive)
  (anything-run-after-quit
   #'(lambda ()
       (with-current-buffer (get-buffer-create "*Anything Help*")
         (erase-buffer)
         (insert
          (substitute-command-keys
           "The keys that are defined for `anything' are:
       \\{anything-map}")))
       (pop-to-buffer "*Anything Help*")
       (goto-char (point-min)))))

;; Use `describe-mode' key in `global-map'
;; (dolist (k (where-is-internal 'describe-mode global-map))
;;   (define-key anything-map k 'anything-c-describe-anything-bindings))

;;; Help message
(defun anything-c-list-preconfigured-anything ()
  (loop with doc
        with sym
        for entry in (cdr (assoc (locate-library "anything-config") load-history))
        if (and (consp entry)
                (eq (car entry) 'defun)
                (string-match "^Preconfigured.+$"
                              (setq doc (or (documentation (setq sym (cdr entry)))
                                            ""))))
        collect (format "\\[%s] : %s\n" sym (match-string 0 doc))))

(setq anything-help-message
      (concat
       "\\<anything-map>"
       "`anything' is QuickSilver-like candidate-selection framework.

Narrow the list by typing some pattern,
Multiple patterns are allowed by splitting by space.
Select with natural Emacs operations, choose with RET.

== Basic Operations ==
C-p, Up: Previous Line
C-n, Down : Next Line
M-v, PageUp : Previous Page
C-v, PageDown : Next Page
Enter : Execute first (default) action / Select 
M-< : First Line
M-> : Last Line
M-PageUp, C-M-S-v, C-M-y : Previous Page (other-window)
M-PageDown, C-M-v : Next Page (other-window)

Tab, C-i : Show action list
Left : Previous Source
Right, C-o : Next Source
C-k : Delete pattern
C-z : Persistent Action (Execute action with anything session kept)

== Shortcuts For 2nd/3rd Action ==
\\[anything-select-2nd-action-or-end-of-line] : Execute 2nd Action (if the minibuffer cursor is at end of line)
\\[anything-select-3rd-action] : Execute 3rd Action

== Visible Marks ==
Visible marks store candidate. Some actions uses marked candidates.

\\[anything-toggle-visible-mark] : Toggle Visible Mark
\\[anything-prev-visible-mark] : Previous Mark
\\[anything-next-visible-mark] : Next Mark

== Miscellaneous Commands ==
\\[anything-toggle-resplit-window] : Toggle vertical/horizontal split anything window
\\[anything-quit-and-find-file] : Drop into `find-file'
\\[anything-delete-current-selection] : Delete Selected Item (visually)
\\[anything-kill-selection-and-quit] : Set Item Into the kill-ring And Quit
\\[anything-yank-selection] : Yank Selected Item Into Pattern
\\[anything-follow-mode] : Toggle Automatical Execution Of Persistent Action
\\[anything-force-update] : Recalculate And Redisplay Candidates

== Global Commands ==
\\<global-map>\\[anything-resume] revives last `anything' session.
It is very useful, so you should bind any key.

Simgle source is executed by \\[anything-call-source].

== Preconfigured `anything' ==
Preconfigured `anything' is commands that uses `anything' interface.
You can use them without configuration.

"
       (apply 'concat (anything-c-list-preconfigured-anything))
       "
Enjoy!"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Preconfigured Anything ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;###autoload
(defun anything-for-files ()
  "Preconfigured `anything' for opening files.
ffap -> recentf -> buffer -> bookmark -> file-cache -> files-in-current-dir -> locate"
  (interactive)
  (anything-other-buffer anything-for-files-prefered-list "*anything for files*"))

;;;###autoload
(defun anything-info-at-point ()
  "Preconfigured `anything' for searching info at point."
  (interactive)
  (anything '(anything-c-source-info-elisp
              anything-c-source-info-cl
              anything-c-source-info-pages)
            (thing-at-point 'symbol) nil nil nil "*anything info*"))

;;;###autoload
(defun anything-show-kill-ring ()
  "Preconfigured `anything' for `kill-ring'. It is drop-in replacement of `yank-pop'.
You may bind this command to M-y."
  (interactive)
  (anything-other-buffer 'anything-c-source-kill-ring "*anything kill-ring*"))

;;;###autoload
(defun anything-minibuffer-history ()
  "Preconfigured `anything' for `minibuffer-history'."
  (interactive)
  (let ((enable-recursive-minibuffers t))
    (anything-other-buffer 'anything-c-source-minibuffer-history
                           "*anything minibuffer-history*")))

;; In Emacs 23.1.50, minibuffer-local-must-match-filename-map was renamed to
;; minibuffer-local-filename-must-match-map.
(defvar minibuffer-local-filename-must-match-map (make-sparse-keymap)) ;; Emacs 23.1.+
(defvar minibuffer-local-must-match-filename-map (make-sparse-keymap)) ;; Older Emacsen
(dolist (map (list minibuffer-local-filename-completion-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-filename-map
                   minibuffer-local-filename-must-match-map
                   minibuffer-local-map
                   minibuffer-local-isearch-map
                   minibuffer-local-must-match-map
                   minibuffer-local-ns-map))
  (define-key map "\C-r" 'anything-minibuffer-history))

;;;###autoload
(defun anything-gentoo ()
  "Preconfigured `anything' for gentoo linux."
  (interactive)
  (anything-other-buffer '(anything-c-source-gentoo
                           anything-c-source-use-flags)
                         "*anything gentoo*"))

;;;###autoload
(defun anything-surfraw-only ()
  "Preconfigured `anything' for surfraw.
If region is marked set anything-pattern to region.
With one prefix arg search symbol at point.
With two prefix args allow choosing in which symbol to search."
  (interactive)
  (let (search pattern)
    (cond ((region-active-p)
           (setq pattern (buffer-substring (region-beginning) (region-end))))
          ((equal current-prefix-arg '(4))
           (setq pattern (thing-at-point 'symbol)))
          ((equal current-prefix-arg '(16))
           (setq search
                 (intern
                  (completing-read "Search in: "
                                   (list "symbol" "sentence" "sexp" "line" "word"))))
           (setq pattern (thing-at-point search))))
    (anything 'anything-c-source-surfraw
              (and pattern (replace-regexp-in-string "\n" "" pattern))
              nil nil nil "*anything surfraw*")))

;;;###autoload
(defun anything-imenu ()
  "Preconfigured `anything' for `imenu'."
  (interactive)
  (anything 'anything-c-source-imenu nil nil nil nil "*anything imenu*"))

;;;###autoload
(defun anything-google-suggest ()
  "Preconfigured `anything' for google search with google suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-google-suggest "*anything google*"))

;;;###autoload
(defun anything-yahoo-suggest ()
  "Preconfigured `anything' for Yahoo searching with Yahoo suggest."
  (interactive)
  (anything-other-buffer 'anything-c-source-yahoo-suggest "*anything yahoo*"))

;;; Converted from anything-show-*-only
;;;###autoload
(defun anything-for-buffers ()
  "Preconfigured `anything' for buffer."
  (interactive)
  (anything-other-buffer 'anything-c-source-buffers "*anything for buffers*"))

;;;###autoload
(defun anything-bbdb ()
  "Preconfigured `anything' for BBDB."
  (interactive)
  (anything-other-buffer 'anything-c-source-bbdb "*anything bbdb*"))

;;;###autoload
(defun anything-locate ()
  "Preconfigured `anything' for Locate."
  (interactive)
  (anything-other-buffer 'anything-c-source-locate "*anything locate*"))

;;;###autoload
(defun anything-w3m-bookmarks ()
  "Preconfigured `anything' for w3m bookmark."
  (interactive)
  (anything-other-buffer 'anything-c-source-w3m-bookmarks "*anything w3m bookmarks*"))

;;;###autoload
(defun anything-colors ()
  "Preconfigured `anything' for color."
  (interactive)
  (anything-other-buffer '(anything-c-source-colors anything-c-source-customize-face)
                         "*anything colors*"))

;;;###autoload
(defun anything-bm-list ()
  "Preconfigured `anything' for visible bookmarks."
  (interactive)
  (anything-other-buffer 'anything-c-source-bm "*anything bm list*"))

;;;###autoload
(defun anything-timers ()
  "Preconfigured `anything' for timers."
  (interactive)
  (anything-other-buffer '(anything-c-source-absolute-time-timers
                           anything-c-source-idle-time-timers)
                         "*anything timers*"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything Applications ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; kill buffers
;;;###autoload
(defun anything-kill-buffers ()
  "Preconfigured `anything' to kill buffer you selected."
  (interactive)
  (anything
   '(((name . "Kill Buffers")
      (candidates . anything-c-buffer-list)
      (action
       ("Kill Buffer" . (lambda (candidate)
                          (kill-buffer candidate)
                          (anything-kill-buffers)
                          )))))
   nil nil))

;;; Regexp
;;;###autoload
(defun anything-query-replace-regexp (&rest args)
  "Preconfigured `anything' : Drop-in replacement of `query-replace-regexp' with building regexp visually."
  (interactive
   (let ((common
          (anything-c-regexp-base "Query Replace Regexp: "
                                  '((name . "Lines matching Regexp")
                                    (mode-line . "Set replace start line and type RET.")
                                    (action . anything-c-query-replace-args)))))
     (if (not common)
         (keyboard-quit))
     (list (car common) (cadr common) (caddr common)
	   ;; These are done separately here
	   ;; so that command-history will record these expressions
	   ;; rather than the values they had this time.
           ;;
           ;; This idea is borrowed from original `query-replace-regexp'.
           (if (and transient-mark-mode mark-active)
               (region-beginning))
           (if (and transient-mark-mode mark-active)
               (region-end)))))
  (apply 'query-replace-regexp args))

;;;###autoload
(defun anything-regexp ()
  "Preconfigured `anything' : It is like `re-builder'. It helps buliding regexp and replacement."
  (interactive)
  (anything-c-regexp-base
   "Regexp: "
   '((name . "Regexp Builder")
     (mode-line . "Press TAB to select action.")
     (action
      ("Kill Regexp as sexp" .
       (lambda (x) (anything-c-regexp-kill-new (prin1-to-string (funcall (anything-attr 'regexp))))))
      ("Query Replace Regexp" .
       (lambda (x) (apply 'query-replace-regexp (anything-c-query-replace-args (point)))))
      ("Kill Regexp" .
       (lambda (x) (anything-c-regexp-kill-new (funcall (anything-attr 'regexp)))))))))

(defun anything-c-query-replace-args (start-point)
  ;; create arguments of `query-replace-regexp'.
  (let ((region-only (and transient-mark-mode mark-active))
        (regexp (funcall (anything-attr 'regexp))))
    (list
     regexp
     (query-replace-read-to regexp
                            (format "Query replace regexp %s%s%s with: "
                                    (if region-only "in region " "")
                                    regexp
                                    (if current-prefix-arg "(word) " ""))
                            t)
     current-prefix-arg)))

(defun anything-c-regexp-get-line (s e)
  (propertize
   (apply 'concat
          ;; Line contents
          (format "%5d: %s" (line-number-at-pos s) (buffer-substring s e))
          ;; subexps
          (loop for i from 0 to (1- (/ (length (match-data)) 2))
                unless (zerop i)
                collect (format "\n         $%d = %s"
                                i (match-string i))))
   ;; match beginning
   ;; KLUDGE: point of anything-candidate-buffer is +1 than that of anything-current-buffer.
   ;; It is implementation problem of candidates-in-buffer.
   'anything-realvalue
   (1- s)))

;; Shut up byte compiler
(defun anything-goto-line (numline)
  "Replacement of `goto-line'."
  (goto-char (point-min))
  (forward-line (1- numline)))

(defun anything-c-regexp-persistent-action (pt)
  (goto-char pt)
  (anything-persistent-highlight-point))

(defun anything-c-regexp-base (prompt attributes)
  (save-restriction
    (let ((anything-compile-source-functions
           ;; rule out anything-match-plugin because the input is one regexp.
           (delq 'anything-compile-source--match-plugin
                 (copy-sequence anything-compile-source-functions)))
          (base-attributes
           '((init . (lambda () (anything-candidate-buffer anything-current-buffer)))
             (candidates-in-buffer)
             (get-line . anything-c-regexp-get-line)
             (persistent-action . anything-c-regexp-persistent-action)
             (persistent-help . "Show this line")
             (multiline)
             (delayed))))
      (if (and transient-mark-mode mark-active)
          (narrow-to-region (region-beginning) (region-end)))
      (anything
       (list
        (append
         attributes
         '((regexp . (lambda () anything-pattern)))
         base-attributes)
        ;; sexp form regexp
        (append
         `((name . ,(concat (assoc-default 'name attributes) " (sexp)")))
         attributes
         '((candidates-in-buffer
            . (lambda () (let ((anything-pattern (eval (read anything-pattern))))
                           (anything-candidates-in-buffer))))
           (regexp . (lambda () (eval (read anything-pattern)))))
         base-attributes))
       nil prompt nil nil "*anything regexp*"))))

;; (anything-c-regexp-base "Regexp: " '((name . "test")))
;; (anything-c-regexp-base "Regexp: " '((name . "test") (candidates-in-buffer . (lambda () (let ((anything-pattern (eval (read anything-pattern)))) (anything-candidates-in-buffer))))))

(defun anything-c-regexp-kill-new (input)
  (kill-new input)
  (message "Killed: %s" input))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Interactive Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anything-insert-buffer-name ()
  "Insert buffer name."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (if buffer-file-name (file-name-nondirectory buffer-file-name)
       (buffer-name)))))

(defun anything-insert-symbol ()
  "Insert current symbol."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (save-excursion
       (buffer-substring (beginning-of-thing 'symbol)
                         (end-of-thing 'symbol))))))

(defun anything-insert-selection ()
  "Insert current selection."
  (interactive)
  (anything-insert-string
   (with-current-buffer anything-current-buffer
     (anything-get-selection))))

(defun anything-show-buffer-only ()
  "[OBSOLETE] Only show sources about buffer.
Use `anything-for-buffers' instead."
  (interactive)
  (anything-set-source-filter '("Buffers")))

(defun anything-show-bbdb-only ()
  "[OBSOLETE] Only show sources about BBDB.
Use `anything-bbdb' instead."
  (interactive)
  (anything-set-source-filter '("BBDB")))

(defun anything-show-locate-only ()
  "[OBSOLETE] Only show sources about Locate.
Use `anything-locate' instead."
  (interactive)
  (anything-set-source-filter '("Locate")))

(defun anything-show-info-only ()
  "[OBSOLETE] Only show sources about Info.
Use `anything-info-at-point' instead."
  (interactive)
  (anything-set-source-filter '("Info Pages"
                                "Info Elisp"
                                "Info Common-Lisp")))

(defun anything-show-imenu-only ()
  "[OBSOLETE] Only show sources about Imenu.
Use `anything-imenu' instead."
  (interactive)
  (anything-set-source-filter '("Imenu")))

(defun anything-show-files-only ()
  "[OBSOLETE] Only show sources about File.
Use `anything-for-files' instead."
  (interactive)
  (anything-set-source-filter '("File Name History"
                                "Files from Current Directory"
                                "Recentf")))

(defun anything-show-w3m-bookmarks-only ()
  "[OBSOLETE] Only show source about w3m bookmark.
Use `anything-w3m-bookmarks' instead."
  (interactive)
  (anything-set-source-filter '("W3m Bookmarks")))

(defun anything-show-colors-only ()
  "[OBSOLETE] Only show source about color.
Use `anything-colors' instead."
  (interactive)
  (anything-set-source-filter '("Colors"
                                "Customize Faces")))

(defun anything-show-kill-ring-only ()
  "[OBSOLETE] Only show source about kill ring.
Use `anything-show-kill-ring' instead."
  (interactive)
  (anything-set-source-filter '("Kill Ring")))

(defun anything-show-this-source-only ()
  "Only show this source."
  (interactive)
  (setq anything-candidate-number-limit 9999)
  (anything-set-source-filter
   (list (assoc-default 'name (anything-get-current-source)))))

(defun anything-test-sources ()
  "List all anything sources for test.
The output is sexps which are evaluated by \\[eval-last-sexp]."
  (interactive)
  (with-output-to-temp-buffer "*Anything Test Sources*"
    (mapc (lambda (s) (princ (format ";; (anything '%s)\n" s)))
          (apropos-internal "^anything-c-source" #'boundp))
    (pop-to-buffer standard-output)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Utilities Functions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; For compatibility
(unless (fboundp 'region-active-p)
  (defun region-active-p ()
    "Return t if Transient Mark mode is enabled and the mark is active.

Most commands that act on the region if it is active and
Transient Mark mode is enabled, and on the text near point
otherwise, should use `use-region-p' instead.  That function
checks the value of `use-empty-active-region' as well."
    (and transient-mark-mode mark-active)))

(defun anything-nest (&rest same-as-anything)
  "Nested `anything'. If you use `anything' within `anything', use it."
  (with-selected-window (anything-window)
    (let (anything-current-position
          anything-current-buffer
          (orig-anything-buffer anything-buffer)
          anything-pattern
          anything-buffer
          anything-sources
          anything-compiled-sources
          anything-buffer-chars-modified-tick
          (anything-samewindow t)
          (enable-recursive-minibuffers t))
      (unwind-protect
          (apply #'anything same-as-anything)
        (anything-initialize-overlays orig-anything-buffer)
        (add-hook 'post-command-hook 'anything-check-minibuffer-input)))))

(defun anything-displaying-source-names ()
  "Display sources name."
  (with-current-buffer anything-buffer
    (goto-char (point-min))
    (loop with pos
          while (setq pos (next-single-property-change (point) 'anything-header))
          do (goto-char pos)
          collect (buffer-substring-no-properties (point-at-bol)(point-at-eol))
          do (forward-line 1))))

(defun anything-select-source ()
  "Select source."
  (interactive)
  (let ((default (assoc-default 'name (anything-get-current-source)))
        (source-names (anything-displaying-source-names))
        (all-source-names (mapcar (lambda (s) (assoc-default 'name s))
                                  (anything-get-sources))))
    (setq anything-candidate-number-limit 9999)
    (anything-aif
        (let (anything-source-filter)
          (anything-nest '(((name . "Anything Source")
                            (candidates . source-names)
                            (action . identity))
                           ((name . "Anything Source (ALL)")
                            (candidates . all-source-names)
                            (action . identity)))
                         nil "Source: " nil
                         default "*anything select source*"))
        (anything-set-source-filter (list it))
      (anything-set-source-filter nil))))

(defun anything-insert-string (str)
  "Insert STR."
  (delete-minibuffer-contents)
  (insert str))

(defun anything-c-match-on-file-name (candidate)
  "Return non-nil if `anything-pattern' match the filename (without directory part) of CANDIDATE."
  (string-match anything-pattern (file-name-nondirectory candidate)))

(defun anything-c-match-on-directory-name (candidate)
  "Return non-nil if `anything-pattern' match the directory part of CANDIDATE (a file)."
  (anything-aif (file-name-directory candidate)
      (string-match anything-pattern it)))

(defun anything-c-string-match (candidate)
  "Return non-nil if `anything-pattern' match CANDIDATE.
The match is done with `string-match'."
  (string-match anything-pattern candidate))

;; `anything-c-compose' is no more needed, it is for compatibility.
(defalias 'anything-c-compose 'anything-compose)

(defun anything-c-skip-entries (list regexp)
  "Remove entries which matches REGEXP from LIST."
  (remove-if (lambda (x) (and (stringp x) (string-match regexp x)))
             list))

(defun anything-c-shadow-entries (list regexp)
  "Elements of LIST matching REGEXP will be displayed with the `file-name-shadow' face if available."
  (mapcar (lambda (file)
            ;; Add shadow face property to boring files.
            (let ((face (if (facep 'file-name-shadow)
                            'file-name-shadow
                          ;; fall back to default on XEmacs
                          'default)))
              (if (string-match regexp file)
                  (setq file (propertize file 'face face))))
            file)
          list))

(defsubst anything-c-stringify (str-or-sym)
  "Get string of STR-OR-SYM."
  (if (stringp str-or-sym)
      str-or-sym
    (symbol-name str-or-sym)))

(defsubst anything-c-symbolify (str-or-sym)
  "Get symbol of STR-OR-SYM."
  (if (symbolp str-or-sym)
      str-or-sym
    (intern str-or-sym)))

(defun anything-c-describe-function (func)
  "FUNC is symbol or string."
  (describe-function (anything-c-symbolify func)))

(defun anything-c-describe-variable (var)
  "VAR is symbol or string."
  (describe-variable (anything-c-symbolify var)))

(defun anything-c-find-function (func)
  "FUNC is symbol or string."
  (find-function (anything-c-symbolify func)))

(defun anything-c-find-variable (var)
  "VAR is symbol or string."
  (find-variable (anything-c-symbolify var)))

(defun anything-c-kill-new (string &optional replace yank-handler)
  "STRING is symbol or string."
  (kill-new (anything-c-stringify string) replace yank-handler))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Prefix argument in action ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TODO
(defvar anything-current-prefix-arg nil
  "`current-prefix-arg' when selecting action.
It is cleared after executing action.")

(defadvice anything-exit-minibuffer (before anything-current-prefix-arg activate)
  (unless anything-current-prefix-arg
    (setq anything-current-prefix-arg current-prefix-arg)))

(add-hook 'anything-after-action-hook
          (lambda () (setq anything-current-prefix-arg nil)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Hacks ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defadvice eval-defun (after anything-source-hack activate)
  "See `anything-c-enable-eval-defun-hack'."
  (when anything-c-enable-eval-defun-hack
    (let ((varsym (save-excursion
                    (beginning-of-defun)
                    (forward-char 1)
                    (when (memq (read (current-buffer)) '(defvar setq))
                      (read (current-buffer))))))
      (when (string-match "^anything-c-source-" (symbol-name varsym))
        (anything varsym)))))
;; (progn (ad-disable-advice 'eval-defun 'after 'anything-source-hack) (ad-update 'eval-defun))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Document Generator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun anything-c-create-summary ()
  "Create `anything' summary."
  (save-excursion
    (goto-char (point-min))
    (loop while (re-search-forward "^;;;; <\\(.+?\\)>$\\|^;; (anything '\\(.+?\\))$\\|^ *;; (anything '\\(.+?\\))$"  nil t)
          collect (cond ((match-beginning 1)
                         (cons 'section (match-string-no-properties 1)))
                        ((match-beginning 2)
                         (cons 'source
                               (cons (match-string-no-properties 2)
                                     (assoc-default 'name (symbol-value (intern (match-string-no-properties 2)))))))
                        ((match-beginning 3)
                         (cons 'source
                               (cons (match-string-no-properties 3)
                                     (assoc-default 'name (symbol-value (intern (match-string-no-properties 3)))))))))))
                         
;; (find-epp (anything-c-create-summary))

(defun anything-c-insert-summary ()
  "Insert `anything' summary."
  (save-excursion
    (goto-char (point-min))
    (search-forward ";; Below are complete source list you can setup in")
    (forward-line 1)
    (delete-region (point)
                   (progn (search-forward ";;; Change log:" nil t)
                          (forward-line -1) (point)))
    (insert ";;\n")
    (loop with beg
          for (kind . value) in (anything-c-create-summary)
          for i from 0
          do (cond ((eq kind 'section)
                    (unless (zerop i)
                      (align-regexp beg (point) "\\(\\s-*\\)(" 1 1 nil))
                    (insert ";;  " value ":\n")
                    (setq beg (point)))
                   (t
                    (insert ";;     `" (car value) "'    (" (cdr value) ")\n")))
          finally (align-regexp beg (point) "\\(\\s-*\\)(" 1 1 nil))))
;; (anything-c-insert-summary)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Anything Sources ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; <Buffer>
(defun anything-c-buffer-list ()
  "Return the list of names of buffers with boring buffers filtered out.
Boring buffers is specified by `anything-c-boring-buffer-regexp'.
The first buffer in the list will be the last recently used
buffer that is not the current buffer."
  (let ((buffers (mapcar 'buffer-name (buffer-list))))
    (append (cdr buffers) (list (car buffers)))))

(defvar anything-c-source-buffers
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (type . buffer)))
;; (anything 'anything-c-source-buffers)

(defvar anything-c-source-buffer-not-found
  '((name . "Create buffer")
    (dummy)
    (type . buffer)))
;; (anything 'anything-c-source-buffer-not-found)

;;; Buffers+
(defface anything-dir-heading '((t (:foreground "Blue" :background "Pink")))
  "*Face used for directory headings in dired buffers."
  :group 'anything)

(defface anything-file-name
  '((t (:foreground "Blue")))
  "*Face used for file names (without suffixes) in dired buffers."
  :group 'anything)

(defface anything-dir-priv
  '((t (:foreground "DarkRed" :background "LightGray")))
  "*Face used for directory privilege indicator (d) in dired buffers."
  :group 'anything)

(defvar anything-c-buffers-face1 'anything-dir-priv)
(defvar anything-c-buffers-face2 'font-lock-type-face)
(defvar anything-c-buffers-face3 'italic)
(eval-when-compile (require 'dired))
(defun anything-c-highlight-buffers (buffers)
  (require 'dired)
  (loop for i in buffers
        if (rassoc (get-buffer i) dired-buffers)
        collect (propertize i
                            'face anything-c-buffers-face1
                            'help-echo (car (rassoc (get-buffer i) dired-buffers)))
        if (buffer-file-name (get-buffer i))
        collect (propertize i
                            'face anything-c-buffers-face2
                            'help-echo (buffer-file-name (get-buffer i)))
        if (and (not (rassoc (get-buffer i) dired-buffers))
                (not (buffer-file-name (get-buffer i))))
        collect (propertize i
                            'face anything-c-buffers-face3)))

(defvar anything-c-source-buffers+
  '((name . "Buffers")
    (candidates . anything-c-buffer-list)
    (type . buffer)
    (candidate-transformer anything-c-skip-current-buffer
                           anything-c-highlight-buffers
                           anything-c-skip-boring-buffers)
    (persistent-action . anything-c-buffers+-persistent-action)
    (persistent-help . "Show this buffer / C-u \\[anything-execute-persistent-action]: Kill this buffer")))

(defun anything-c-buffers+-persistent-action (name)
  (flet ((kill (item)
               (with-current-buffer item
                 (if (and (buffer-modified-p)
                          (buffer-file-name (current-buffer)))
                     (progn
                       (save-buffer)
                       (kill-buffer item))
                   (kill-buffer item))))
         (goto (item)
               (switch-to-buffer item)))
    (if current-prefix-arg
        (progn
          (kill name)
          (anything-delete-current-selection))
      (goto name))))

;; (anything 'anything-c-source-buffers+)


;;;; <File>
;;; File name history
(defvar anything-c-source-file-name-history
  '((name . "File Name History")
    (candidates . file-name-history)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file)))
;; (anything 'anything-c-source-file-name-history)

;;; Files in current dir
(defvar anything-c-source-files-in-current-dir
  '((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-current-buffer anything-current-buffer
                      (directory-files default-directory))))
    ;; volatile is not needed, I think.
    (type . file)))
;; (anything 'anything-c-source-files-in-current-dir)

(defvar anything-c-files-face1 'anything-dir-priv)
(defvar anything-c-files-face2 'anything-file-name)
(defun anything-c-highlight-files (files)
  (loop for i in files
        if (file-directory-p i)
        collect (propertize (file-name-nondirectory i)
                            'face anything-c-files-face1
                            'help-echo (expand-file-name i))
        else
        collect (propertize (file-name-nondirectory i)
                            'face anything-c-files-face2
                            'help-echo (expand-file-name i))))


(defvar anything-c-source-files-in-current-dir+
  '((name . "Files from Current Directory")
    (candidates . (lambda ()
                    (with-current-buffer anything-current-buffer
                      (directory-files default-directory t))))
    (candidate-transformer anything-c-highlight-files)
    ;; volatile is not needed, I think.
    (type . file)))

;; (anything 'anything-c-source-files-in-current-dir+)

;;; Anything replacement of file name completion for `find-file' and friends.

(defvar anything-c-find-files-doc-header (format " (`%s':Go to precedent level)"
                                                 (if window-system "C-." "C-l")) 
  "*The doc that is inserted in the Name header of a find-files or dired source.")

(defvar anything-c-source-find-files
  `((name . ,(concat "Find Files" anything-c-find-files-doc-header))
    ;; It is needed for filenames with capital letters
    (disable-shortcuts)
    (init . (lambda ()
              (setq ffap-newfile-prompt t)))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action . ,(delq nil `(("Find File" . find-file-at-point)
                           ("Find file in Dired" . anything-c-point-file-in-dired)
                           ,(and (locate-library "elscreen") '("Find file in Elscreen"  . anything-elscreen-find-file))
                           ("Complete at point" . anything-c-insert-file-name-completion-at-point)
                           ("Delete File(s)" . anything-delete-marked-files)
                           ("Find file as root" . anything-find-file-as-root)
                           ("Find file other window" . find-file-other-window)
                           ("Find file other frame" . find-file-other-frame))))))

;; (anything 'anything-c-source-find-files)

(defun* anything-reduce-file-name (fname level &key unix-close expand)
    "Reduce FNAME by LEVEL from end or beginning depending LEVEL value.
If LEVEL is positive reduce from end else from beginning.
If UNIX-CLOSE is non--nil close filename with /.
If EXPAND is non--nil expand-file-name."
  (let* ((exp-fname  (expand-file-name fname))
         (fname-list (split-string (if (or (string= fname "~/") expand)
                                       exp-fname fname) "/" t))
         (len        (length fname-list))
         (pop-list   (if (< level 0)
                         (subseq fname-list (* level -1))
                         (subseq fname-list 0 (- len level))))
         (result     (mapconcat 'identity pop-list "/"))
         (empty      (string= result "")))
    (when unix-close (setq result (concat result "/")))
    (if (string-match "^~" result)
        (if (string= result "~/") "~/" result)
        (if (< level 0)
            (if empty "../" (concat "../" result))
            (cond ((and (eq system-type 'windows-nt) empty)
                   "c:/")
                  ((and (not empty) (eq system-type 'windows-nt))
                   result)
                  (empty "/")
                  (t
                   (concat "/" result)))))))

(defun anything-find-files-or-dired-p ()
  "Test if current source is a dired or find-files source."
  (let ((ff-sources '("Find Files" "Copy Files"
                      "Rename Files" "Symlink Files"
                      "Hardlink Files" "Write File" "Insert File"))
        (cur-source (cdr (assoc 'name (anything-get-current-source)))))
    (catch 'break
      (dolist (i ff-sources)
        (when (equal cur-source (concat i anything-c-find-files-doc-header))
          (throw 'break t))))))

(defun anything-find-files-down-one-level (arg)
  "Go down one level like unix command `cd ..'.
If prefix numeric arg is given go ARG level down."
  (interactive "p")
  (when (anything-find-files-or-dired-p)
    (let ((new-pattern (anything-reduce-file-name anything-pattern arg :unix-close t :expand t)))
      (with-selected-window (minibuffer-window)
        (delete-minibuffer-contents)
        (insert new-pattern)))))

;; `C-.' doesn't work in terms use `C-l' instead. 
(if window-system
    (define-key anything-map (kbd "C-.") 'anything-find-files-down-one-level)
  (define-key anything-map (kbd "C-l") 'anything-find-files-down-one-level))

(defun anything-c-point-file-in-dired (file)
  "Put point on filename FILE in dired buffer."
  (dired (file-name-directory file))
  (dired-goto-file file))

(defun anything-create-tramp-name (fname)
  "Build filename for `anything-pattern' like /su:: or /sudo::."
  (apply #'tramp-make-tramp-file-name
         (loop
            with v = (tramp-dissect-file-name fname)
            for i across v collect i)))
  
(defun anything-find-files-get-candidates ()
  "Create candidate list for `anything-c-source-find-files'."
  (let* ( ; Don't try to tramp connect before entering the second ":".
         (tramp-file-name-regexp "\\`/\\([^[/:]+\\|[^/]+]\\):.*:?")
         (path (cond ((string-match "^~" anything-pattern)
                      (replace-match (getenv "HOME") nil t anything-pattern))
                     ((string-match tramp-file-name-regexp anything-pattern)
                      (let ((tramp-name (anything-create-tramp-name
                                         (match-string 0 anything-pattern))))
                        (replace-match tramp-name nil t anything-pattern)))
                     (t anything-pattern)))
         (tramp-verbose anything-tramp-verbose)) ; No tramp message when 0.
    ;; Inlined version (<2010-02-18 Jeu.>.) of `tramp-handle-directory-files'
    ;; to fix bug in tramp that doesn't show the dot file names(i.e "." "..")
    ;; and sorting.
    (flet ((tramp-handle-directory-files
               (directory &optional full match nosort files-only)
             "Like `directory-files' for Tramp files."
             ;; FILES-ONLY is valid for XEmacs only.
             (when (file-directory-p directory)
               (setq directory (file-name-as-directory (expand-file-name directory)))
               (let ((temp (nreverse (file-name-all-completions "" directory)))
                     result item)

                 (while temp
                   (setq item (directory-file-name (pop temp)))
                   (when (and (or (null match) (string-match match item))
                              (or (null files-only)
                                  ;; Files only.
                                  (and (equal files-only t) (file-regular-p item))
                                  ;; Directories only.
                                  (file-directory-p item)))
                     (push (if full (concat directory item) item)
                           result)))
                 (if nosort result (sort result 'string<))))))

      (set-text-properties 0 (length path) nil path)
      (setq anything-pattern path)
      (cond ((or (file-regular-p path)
                 (and ffap-url-regexp (string-match ffap-url-regexp path)))
             (list path))
            ((string= anything-pattern "") (directory-files "/" t))
            ((file-directory-p path) (directory-files path t))
            (t
             (append
              (list path)
              (directory-files (file-name-directory path) t)))))))

(defface anything-dired-symlink-face
  '((t (:foreground "DarkOrange")))
  "*Face used for symlinks in `anything-find-files'."
  :group 'anything)

(defun anything-c-highlight-ffiles (files)
  "Candidate transformer for `anything-c-source-find-files'."
  (loop for i in files
     if (file-symlink-p i)
     collect (propertize i 'face 'anything-dired-symlink-face
                         'help-echo (file-truename i)) into a
     if (file-directory-p i)
     collect (propertize i 'face anything-c-files-face1) into a
     else
     collect (propertize i 'face anything-c-files-face2) into a
     finally return a))


(defun anything-find-files-persistent-action (candidate)
  "Open subtree CANDIDATE without quitting anything.
If CANDIDATE is not a directory expand CANDIDATE filename.
If CANDIDATE is alone, open file CANDIDATE filename."
  (flet ((insert-in-minibuffer (elm)
           (with-selected-window (minibuffer-window)
             (delete-minibuffer-contents)
             (set-text-properties 0 (length elm) nil elm)
             (insert elm))))
    (cond ((and (file-directory-p candidate) (file-symlink-p candidate))
           (insert-in-minibuffer (file-name-as-directory
                                  (file-truename
                                   (expand-file-name candidate)))))
           ((file-directory-p candidate)
            (insert-in-minibuffer (file-name-as-directory
                                  (expand-file-name candidate))))
          ((file-symlink-p candidate)
           (insert-in-minibuffer (file-truename candidate)))
          (t ; First hit on C-z expand CANDIDATE second hit open file.
           (let ((new-pattern   (anything-get-selection anything-last-buffer))
                 (num-lines-buf (with-current-buffer anything-last-buffer
                                  (count-lines (point-min) (point-max)))))
             (if (> num-lines-buf 3)
                 (insert-in-minibuffer new-pattern) (find-file candidate)))))))

(defun anything-c-insert-file-name-completion-at-point (candidate)
  "Insert file name completion at point."
  (let* ((end         (point))
         (guess       (thing-at-point 'filename))
         (full-path-p (string-match (concat "^" (getenv "HOME")) guess)))
    (set-text-properties 0 (length candidate) nil candidate)
    (when (and guess (not (string= guess "")))
      (search-backward guess (- (point) (length guess)))
      (delete-region (point) end)
      (if full-path-p
          (insert (expand-file-name candidate))
          (insert (abbreviate-file-name candidate))))))

;;; REGRESSION:
;;; FIXME completing on regexps don't erase initial text.
;;
;; (defun anything-c-insert-file-name-completion-at-point (candidate)
;;   "Insert filename completion at point."
;;   (let* ((end         (point))
;;          (guess       (anything-get-regexp-filename-at-point))
;;          (full-path-p (string-match (concat "^" (getenv "HOME")) (cdr guess))))
;;     (set-text-properties 0 (length candidate) nil candidate)
;;     (delete-region (- (point) (car guess)) end)
;;     (if full-path-p
;;         (insert (expand-file-name candidate))
;;         (insert (abbreviate-file-name candidate)))))


;; (defun anything-get-regexp-filename-at-point ()
;;   "Return filename at point even if filename contain regexps."
;;   (let ((thing-at-point-file-name-chars "\\(^\\|~/\\|/\\)*[:alnum:]_.${}#%,:"))
;;     (save-excursion
;;       (loop with count = 0
;;          for tap = (thing-at-point 'filename)
;;          while (and (string= tap "") (not (looking-back "\"\\|\'\\|\n")))
;;          do (progn (forward-char -1) (incf count))
;;          finally return (cons (+ count (length tap)) tap)))))

;;;###autoload
(defun anything-find-files ()
  "Preconfigured `anything' for anything implementation of `find-file'."
  (interactive)
  (anything 'anything-c-source-find-files
            (anything-find-files-input (ffap-guesser) (thing-at-point 'filename))
            "Find Files or Url: " nil nil "*Anything Find Files*"))

(defun anything-find-files-input (fap tap)
  "Default input of `anything-find-files'."
  (let* ((file-p (and fap (file-exists-p fap)
                      (file-exists-p
                       (file-name-directory (expand-file-name tap)))))
         (input  (if file-p (expand-file-name tap) fap)))
    (or input (expand-file-name default-directory))))

;;; Anything completion for `write-file'.==> C-x C-w
(defvar anything-c-source-write-file
  `((name . ,(concat "Write File" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Write File" . (lambda (candidate)
                        (write-file candidate 'confirm)))))))

;;;###autoload
(defun anything-write-file ()
  "Preconfigured `anything' providing completion for `write-file'."
  (interactive)
  (anything 'anything-c-source-write-file
            (expand-file-name default-directory)
            "Write buffer to file: " nil nil "*Anything write file*"))

;;; Anything completion for `insert-file'.==> C-x i
(defvar anything-c-source-insert-file
  `((name . ,(concat "Insert File" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Insert File" . (lambda (candidate)
                        (when (y-or-n-p (format "Really insert %s in %s "
                                                candidate anything-current-buffer))
                          (insert-file candidate))))))))

;;;###autoload
(defun anything-insert-file ()
  "Preconfigured `anything' providing completion for `insert-file'."
  (interactive)
  (anything 'anything-c-source-insert-file
            (expand-file-name default-directory)
            "Insert file: " nil nil "*Anything insert file*"))

;;; Anything completion for copy, rename and (rel)sym/hard/link files from dired.
(defvar anything-c-source-copy-files
  `((name . ,(concat "Copy Files" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Copy File"
       . (lambda (candidate)
           (anything-dired-action candidate :action 'copy)))
      ("Copy and Follow"
       . (lambda (candidate)
           (anything-dired-action candidate :action 'copy :follow t)))))))


(defvar  anything-c-source-rename-files
  `((name . ,(concat "Rename Files" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Rename File"
       . (lambda (candidate)
           (anything-dired-action candidate :action 'rename)))
      ("Rename and Follow"
       . (lambda (candidate)
           (anything-dired-action candidate :action 'rename :follow t)))))))

(defvar anything-c-source-symlink-files
  `((name . ,(concat "Symlink Files" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Symlink File" . (lambda (candidate)
                          (anything-dired-action candidate :action 'symlink)))
      ("RelSymlink File" . (lambda (candidate)
                             (anything-dired-action candidate :action 'relsymlink)))))))


(defvar anything-c-source-hardlink-files
  `((name . ,(concat "Hardlink Files" anything-c-find-files-doc-header))
    (candidates . anything-find-files-get-candidates)
    (candidate-transformer anything-c-highlight-ffiles)
    (persistent-action . anything-find-files-persistent-action)
    (persistent-help . "Expand Candidate")
    (volatile)
    (action .
     (("Hardlink File" . (lambda (candidate)
                           (anything-dired-action candidate :action 'hardlink)))))))

(defun* anything-dired-action (candidate &key action follow)
  "Copy, rename or symlink file at point or marked files in dired to CANDIDATE.
ACTION is a key that can be one of 'copy, 'rename, 'symlink, 'relsymlink."
  (let ((files  (dired-get-marked-files))
        (fn     (case action
                  ('copy       'dired-copy-file)
                  ('rename     'dired-rename-file)
                  ('symlink    'make-symbolic-link)
                  ('relsymlink 'dired-make-relative-symlink)
                  ('hardlink   'dired-hardlink)))
        (marker (case action
                  ((copy rename)   dired-keep-marker-copy)
                  ('symlink        dired-keep-marker-symlink)
                  ('relsymlink     dired-keep-marker-relsymlink)
                  ('hardlink       dired-keep-marker-hardlink))))
    (dired-create-files
     fn (symbol-name action) files
     (if (file-directory-p candidate)
         ;; When CANDIDATE is a directory, build file-name in this directory.
         ;; Else we use CANDIDATE.
         #'(lambda (from)
             (expand-file-name (file-name-nondirectory from) candidate))
         #'(lambda (from) candidate))
     marker)
    (when follow
      (let* ((src          (car files))
             (dest         (expand-file-name candidate))
             (basename-src (if (file-directory-p src)
                               (file-relative-name
                                (directory-file-name src)
                                (file-name-directory src))
                               (file-name-nondirectory src)))
             (fname        (if (file-directory-p dest)
                               (concat (file-name-as-directory dest)
                                       basename-src)
                               dest)))
        (anything-c-point-file-in-dired fname)))))


(defun* anything-dired-do-action-on-file (&key action)
  (let* ((files     (dired-get-marked-files))
         (len       (length files))
         (fname     (if (> len 1)
                        (format "* %d Files" len)
                        (car files)))
         (source    (case action
                      ('copy     'anything-c-source-copy-files)
                      ('rename   'anything-c-source-rename-files)
                      ('symlink  'anything-c-source-symlink-files)
                      ('hardlink 'anything-c-source-hardlink-files)))
         (prompt-fm (case action
                      ('copy     "Copy %s to: ")
                      ('rename   "Rename %s to: ")
                      ('symlink  "Symlink %s to: ")
                      ('hardlink "Hardlink %s to: ")))
         (buffer    (case action
                      ('copy     "*Anything Copy Files*")
                      ('rename   "*Anything Rename Files*")
                      ('symlink  "*Anything Symlink Files*")
                      ('hardlink "*Anything Hardlink Files*"))))
    (anything source
              (or (dired-dwim-target-directory)
                  (expand-file-name default-directory))
              (format prompt-fm fname) nil nil buffer)))


;;;###autoload
(defun anything-dired-rename-file ()
  "Preconfigured `anything' to rename files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'rename))

;;;###autoload
(defun anything-dired-copy-file ()
  "Preconfigured `anything' to copy files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'copy))

;;;###autoload
(defun anything-dired-symlink-file ()
  "Preconfigured `anything' to symlink files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'symlink))

;;;###autoload
(defun anything-dired-hardlink-file ()
  "Preconfigured `anything' to hardlink files from dired."
  (interactive)
  (anything-dired-do-action-on-file :action 'hardlink))

(defvar anything-dired-bindings nil)
;;;###autoload
(defun anything-dired-bindings (&optional arg)
  "Replace usual dired commands `C' and `R' by anything ones.
When call interactively toggle dired bindings and anything bindings.
When call non--interactively with arg > 0, enable anything bindings.
You can put (anything-dired-binding 1) in init file to enable anything bindings."
  (interactive)
  (if (or (when arg (> arg 0)) (not anything-dired-bindings))
      (progn
        (define-key dired-mode-map (kbd "C") 'anything-dired-copy-file)
        (define-key dired-mode-map (kbd "R") 'anything-dired-rename-file)
        (define-key dired-mode-map (kbd "S") 'anything-dired-symlink-file)
        (define-key dired-mode-map (kbd "H") 'anything-dired-hardlink-file)
        (setq anything-dired-bindings t))
      (define-key dired-mode-map (kbd "C") 'dired-do-copy)
      (define-key dired-mode-map (kbd "R") 'dired-do-rename)
      (define-key dired-mode-map (kbd "S") 'dired-do-symlink)
      (define-key dired-mode-map (kbd "H") 'dired-do-hardlink)
      (setq anything-dired-bindings nil)))

;;; File Cache
(defvar anything-c-source-file-cache-initialized nil)

(defvar anything-c-file-cache-files nil)

(defvar anything-c-source-file-cache
  '((name . "File Cache")
    (init . (lambda ()
              (require 'filecache nil t)
              (unless anything-c-source-file-cache-initialized
                (setq anything-c-file-cache-files
                      (loop for item in file-cache-alist append
                            (destructuring-bind (base &rest dirs) item
                              (loop for dir in dirs collect
                                    (concat dir base)))))
                (defadvice file-cache-add-file (after file-cache-list activate)
                  (add-to-list 'anything-c-file-cache-files (expand-file-name file)))
                (setq anything-c-source-file-cache-initialized t))))
    (candidates . anything-c-file-cache-files)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file)))
;; (anything 'anything-c-source-file-cache)

;;; Locate
(defvar anything-c-locate-options
  (cond
   ((eq system-type 'darwin) '("locate"))
   ((eq system-type 'berkeley-unix) '("locate" "-i"))
   (t '("locate" "-i" "-r")))
  "A list where the `car' is the name of the locat program followed by options.
The search pattern will be appended, so the
\"-r\" option should be the last option.")

(defvar anything-c-source-locate
  '((name . "Locate")
    (candidates . (lambda ()
                    (apply 'start-process "locate-process" nil
                           (append anything-c-locate-options
                                   (list anything-pattern)))))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern with locate.")
;; (anything 'anything-c-source-locate)

;;; Recentf files
(defvar anything-c-source-recentf
  '((name . "Recentf")
    (init . (lambda ()
              (require 'recentf)
              (or recentf-mode (recentf-mode 1))
              ;; Big value empowers anything/recentf
              (when (and (numberp recentf-max-saved-items)
                         (<= recentf-max-saved-items 20))
                (setq recentf-max-saved-items 500))))
    (candidates . recentf-list)
    (match anything-c-match-on-file-name
           anything-c-match-on-directory-name)
    (type . file))
  "See (info \"(emacs)File Conveniences\").
if `recentf-max-saved-items' is too small, set it to 500.")
;; (anything 'anything-c-source-recentf)

;;; ffap
(eval-when-compile (require 'ffap))
(defvar anything-c-source-ffap-guesser
  '((name . "File at point")
    (init . (lambda () (require 'ffap)))
    (candidates . (lambda ()
                    (anything-aif
                        (with-current-buffer anything-current-buffer
                          (ffap-guesser))
                        (list it))))
    (type . file)))
;; (anything 'anything-c-source-ffap-guesser)

;;; ffap with line number
(defun anything-c-ffap-file-line-at-point ()
  "Get (FILENAME . LINENO) at point."
  (anything-aif (let (ffap-alist) (ffap-file-at-point))
      (save-excursion
        (beginning-of-line)
        (when (and (search-forward it nil t)
                   (looking-at ":\\([0-9]+\\)"))
          (cons it (string-to-number (match-string 1)))))))

(defvar anything-c-ffap-line-location nil
  "(FILENAME . LINENO) used by `anything-c-source-ffap-line'.
It is cleared after jumping line.")

(defun anything-c-ffap-line-candidates ()
  (with-current-buffer anything-current-buffer
    (setq anything-c-ffap-line-location (anything-c-ffap-file-line-at-point)))
  (when anything-c-ffap-line-location
    (destructuring-bind (file . line) anything-c-ffap-line-location
      (list (cons (format "%s (line %d)" file line) file)))))

;;; Goto line after opening file by `anything-c-source-ffap-line'.
(defun anything-c-ffap-line-goto-line ()
  (when (car anything-c-ffap-line-location)
    (unwind-protect
        (ignore-errors
          (with-selected-window (get-buffer-window
                                 (get-file-buffer (car anything-c-ffap-line-location)))
            (anything-goto-line (cdr anything-c-ffap-line-location)))))))
(add-hook 'anything-after-action-hook 'anything-c-ffap-line-goto-line)
(add-hook 'anything-after-persistent-action-hook 'anything-c-ffap-line-goto-line)

(defvar anything-c-source-ffap-line
  '((name . "File/Lineno at point")
    (init . (lambda () (require 'ffap)))
    (candidates . anything-c-ffap-line-candidates)
    (type . file)))
;; (anything 'anything-c-source-ffap-line)

;;; list of files gleaned from every dired buffer
(defun anything-c-files-in-all-dired-candidates ()
  (save-excursion
    (mapcan
     (lambda (dir)
       (cond ((listp dir)               ;filelist
              dir)
             ((equal "" (file-name-nondirectory dir)) ;dir
              (directory-files dir t))
             (t                         ;wildcard
              (file-expand-wildcards dir t))))
     (delq nil
           (mapcar (lambda (buf)
                     (set-buffer buf)
                     (when (eq major-mode 'dired-mode)
                       (if (consp dired-directory)
                           (cdr dired-directory) ;filelist
                         dired-directory))) ;dir or wildcard
                   (buffer-list))))))
;; (dired '("~/" "~/.emacs-custom.el" "~/.emacs.bmk"))

(defvar anything-c-source-files-in-all-dired
  '((name . "Files in all dired buffer.")
    (candidates . anything-c-files-in-all-dired-candidates)
    (type . file)))
;; (anything 'anything-c-source-files-in-all-dired)

;;;; <info>
;;; Info pages
(defvar anything-c-info-pages nil
  "All info pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-info-pages
  `((name . "Info Pages")
    (candidates . (lambda ()
                    (if anything-c-info-pages
                        anything-c-info-pages
                      (setq anything-c-info-pages
                            (save-window-excursion
                              (save-excursion
                                (require 'info)
                                (Info-find-node "dir" "top")
                                (goto-char (point-min))
                                (let ((info-topic-regexp "\\* +\\([^:]+: ([^)]+)[^.]*\\)\\.")
                                      topics)
                                  (while (re-search-forward info-topic-regexp nil t)
                                    (add-to-list 'topics (match-string-no-properties 1)))
                                  (goto-char (point-min))
                                  (Info-exit)
                                  topics)))))))
    (action . (("Show with Info" .(lambda (node-str)
                                    (info (replace-regexp-in-string "^[^:]+: "
                                                                    ""
                                                                    node-str))))))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-info-pages)


;;; Use info-index plug-in. Note that `name' attribute is
;;; not needed but `anything-c-insert-summary' uses it.
;; Info Elisp
(defvar anything-c-source-info-elisp
  '((name . "Info index: elisp")
    (info-index . "elisp")))
;; (anything 'anything-c-source-info-elisp)

;; Info-Common-Lisp
(defvar anything-c-source-info-cl
  '((name . "Info index: cl")
    (info-index . "cl")))
;; (anything 'anything-c-source-info-cl)

;; Info Index org
(defvar anything-c-source-info-org
  '((name . "Info index: org")
    (info-index . "org")))
;; (anything 'anything-c-source-info-org)

;; Info Index ratpoison
(defvar anything-c-source-info-ratpoison
  '((name . "Info index: ratpoison")
    (info-index . "ratpoison")))
;; (anything 'anything-c-source-info-ratpoison)

;; Info Index zsh
(defvar anything-c-source-info-zsh
  '((name . "Info index: zsh")
    (info-index . "zsh")))
;; (anything 'anything-c-source-info-zsh)

;; Info Index bash
(defvar anything-c-source-info-bash
  '((name . "Info index: bash")
    (info-index . "bash")))
;; (anything 'anything-c-source-info-bash)

;; Info Index coreutils
(defvar anything-c-source-info-coreutils
  '((name . "Info index: coreutils")
    (info-index . "coreutils")))
;; (anything 'anything-c-source-info-coreutils)

;; Info Index fileutils
(defvar anything-c-source-info-fileutils
  '((name . "Info index: fileutils")
    (info-index . "fileutils")))
;; (anything 'anything-c-source-info-fileutils)

;; Info Index find
(defvar anything-c-source-info-find
  '((name . "Info index: find")
    (info-index . "find")))
;; (anything 'anything-c-source-info-find)

;; Info Index sh-utils
(defvar anything-c-source-info-sh-utils
  '((name . "Info index: sh-utils")
    (info-index . "sh-utils")))
;; (anything 'anything-c-source-info-sh-utils)

;; Info Index textutils
(defvar anything-c-source-info-textutils
  '((name . "Info index: textutils")
    (info-index . "textutils")))
;; (anything 'anything-c-source-info-textutils)

;; Info Index libc
(defvar anything-c-source-info-libc
  '((name . "Info index: libc")
    (info-index . "libc")))
;; (anything 'anything-c-source-info-libc)

;; Info Index make
(defvar anything-c-source-info-make
  '((name . "Info index: make")
    (info-index . "make")))
;; (anything 'anything-c-source-info-make)

;; Info Index automake
(defvar anything-c-source-info-automake
  '((name . "Info index: automake")
    (info-index . "automake")))
;; (anything 'anything-c-source-info-automake)

;; Info Index autoconf
(defvar anything-c-source-info-autoconf
  '((name . "Info index: autoconf")
    (info-index . "autoconf")))
;; (anything 'anything-c-source-info-autoconf)

;; Info Index emacs-lisp-intro
(defvar anything-c-source-info-emacs-lisp-intro
  '((name . "Info index: emacs-lisp-intro")
    (info-index . "emacs-lisp-intro")))
;; (anything 'anything-c-source-info-emacs-lisp-intro)

;; Info Index emacs
(defvar anything-c-source-info-emacs
  '((name . "Info index: emacs")
    (info-index . "emacs")))
;; (anything 'anything-c-source-info-emacs)

;; Info Index elib
(defvar anything-c-source-info-elib
  '((name . "Info index: elib")
    (info-index . "elib")))
;; (anything 'anything-c-source-info-elib)

;; Info Index eieio
(defvar anything-c-source-info-eieio
  '((name . "Info index: eieio")
    (info-index . "eieio")))
;; (anything 'anything-c-source-info-eieio)

;; Info Index gauche-refe
(defvar anything-c-source-info-gauche-refe
  '((name . "Info index: gauche")
    (info-index . "gauche-refe")))
;; (anything 'anything-c-source-info-gauche-refe)

;; Info Index guile
(defvar anything-c-source-info-guile
  '((name . "Info index: guile")
    (info-index . "guile")))
;; (anything 'anything-c-source-info-guile)

;; Info Index guile-tut
(defvar anything-c-source-info-guile-tut
  '((name . "Info index: guile-tut")
    (info-index . "guile-tut")))
;; (anything 'anything-c-source-info-guile-tut)

;; Info Index goops
(defvar anything-c-source-info-goops
  '((name . "Info index: goops")
    (info-index . "goops")))
;; (anything 'anything-c-source-info-goops)

;; Info Index screen
(defvar anything-c-source-info-screen
  '((name . "Info index: screen")
    (info-index . "screen")
    (index-nodes "Concept Index" "Command Index" "Keystroke Index")))
;; (anything 'anything-c-source-info-screen)

;; Info Index latex
(defvar anything-c-source-info-latex
  '((name . "Info index: latex")
    (info-index . "latex")))
;; (anything 'anything-c-source-info-latex)

;; Info Index gawk
(defvar anything-c-source-info-gawk
  '((name . "Info index: gawk")
    (info-index . "gawk")))
;; (anything 'anything-c-source-info-gawk)

;; Info Index sed
(defvar anything-c-source-info-sed
  '((name . "Info index: sed")
    (info-index . "sed")))
;; (anything 'anything-c-source-info-sed)

;; Info Index m4
(defvar anything-c-source-info-m4
  '((name . "Info index: m4")
    (info-index . "m4")))
;; (anything 'anything-c-source-info-m4)

;; Info Index wget
(defvar anything-c-source-info-wget
  '((name . "Info index: wget")
    (info-index . "wget")))
;; (anything 'anything-c-source-info-wget)

;; Info Index binutils
(defvar anything-c-source-info-binutils
  '((name . "Info index: binutils")
    (info-index . "binutils")))
;; (anything 'anything-c-source-info-binutils)

;; Info Index as
(defvar anything-c-source-info-as
  '((name . "Info index: as")
    (info-index . "as")))
;; (anything 'anything-c-source-info-as)

;; Info Index bfd
(defvar anything-c-source-info-bfd
  '((name . "Info index: bfd")
    (info-index . "bfd")))
;; (anything 'anything-c-source-info-bfd)

;; Info Index gprof
(defvar anything-c-source-info-gprof
  '((name . "Info index: gprof")
    (info-index . "gprof")))
;; (anything 'anything-c-source-info-gprof)

;; Info Index ld
(defvar anything-c-source-info-ld
  '((name . "Info index: ld")
    (info-index . "ld")))
;; (anything 'anything-c-source-info-ld)

;; Info Index diff
(defvar anything-c-source-info-diff
  '((name . "Info index: diff")
    (info-index . "diff")))
;; (anything 'anything-c-source-info-diff)

;; Info Index flex
(defvar anything-c-source-info-flex
  '((name . "Info index: flex")
    (info-index . "flex")))
;; (anything 'anything-c-source-info-flex)

;; Info Index grep
(defvar anything-c-source-info-grep
  '((name . "Info index: grep")
    (info-index . "grep")))
;; (anything 'anything-c-source-info-grep)

;; Info Index gzip
(defvar anything-c-source-info-gzip
  '((name . "Info index: gzip")
    (info-index . "gzip")))
;; (anything 'anything-c-source-info-gzip)

;; Info Index libtool
(defvar anything-c-source-info-libtool
  '((name . "Info index: libtool")
    (info-index . "libtool")))
;; (anything 'anything-c-source-info-libtool)

;; Info Index texinfo
(defvar anything-c-source-info-texinfo
  '((name . "Info index: texinfo")
    (info-index . "texinfo")))
;; (anything 'anything-c-source-info-texinfo)

;; Info Index info
(defvar anything-c-source-info-info
  '((name . "Info index: info")
    (info-index . "info")))
;; (anything 'anything-c-source-info-info)

;; Info Index gdb
(defvar anything-c-source-info-gdb
  '((name . "Info index: gdb")
    (info-index . "gdb")))
;; (anything 'anything-c-source-info-gdb)

;; Info Index stabs
(defvar anything-c-source-info-stabs
  '((name . "Info index: stabs")
    (info-index . "stabs")))
;; (anything 'anything-c-source-info-stabs)

;; Info Index cvsbook
(defvar anything-c-source-info-cvsbook
  '((name . "Info index: cvsbook")
    (info-index . "cvsbook")))
;; (anything 'anything-c-source-info-cvsbook)

;; Info Index cvs
(defvar anything-c-source-info-cvs
  '((name . "Info index: cvs")
    (info-index . "cvs")))
;; (anything 'anything-c-source-info-cvs)

;; Info Index bison
(defvar anything-c-source-info-bison
  '((name . "Info index: bison")
    (info-index . "bison")))
;; (anything 'anything-c-source-info-bison)

;; Info Index id-utils
(defvar anything-c-source-info-id-utils
  '((name . "Info index: id-utils")
    (info-index . "id-utils")))
;; (anything 'anything-c-source-info-id-utils)

;; Info Index global
(defvar anything-c-source-info-global
  '((name . "Info index: global")
    (info-index . "global")))
;; (anything 'anything-c-source-info-global)

;;;; <Help>
;;; Man Pages
(defvar anything-c-man-pages nil
  "All man pages on system.
Will be calculated the first time you invoke anything with this
source.")

(defvar anything-c-source-man-pages
  `((name . "Manual Pages")
    (candidates . (lambda ()
                    (if anything-c-man-pages
                        anything-c-man-pages
                      ;; XEmacs doesn't have a woman :)
                      (setq anything-c-man-pages
                            (ignore-errors
                              (require 'woman)
                              (woman-file-name "")
                              (sort (mapcar 'car woman-topic-all-completions)
                                    'string-lessp))))))
    (action  ("Show with Woman" . woman))
    ;; Woman does not work OS X
    ;; http://xahlee.org/emacs/modernization_man_page.html
    (action-transformer . (lambda (actions candidate)
                            (if (eq system-type 'darwin)
                                '(("Show with Man" . man))
                              actions)))
    (requires-pattern . 2)))
;; (anything 'anything-c-source-man-pages)

;;;; <Command>
;;; Complex command history
(defvar anything-c-source-complex-command-history
  '((name . "Complex Command History")
    (candidates . (lambda () (mapcar 'prin1-to-string command-history)))
    (type . sexp)))
;; (anything 'anything-c-source-complex-command-history)

;;; M-x history
(defvar anything-c-source-extended-command-history
  '((name . "Emacs Commands History")
    (candidates . extended-command-history)
    (type . command)))
;; (anything 'anything-c-source-extended-command-history)

;;; Emacs commands
(defvar anything-c-source-emacs-commands
  '((name . "Emacs Commands")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a)
                                  (if (commandp a)
                                      (push (symbol-name a)
                                            commands))))
                      (sort commands 'string-lessp))))
    (type . command)
    (requires-pattern . 2))
  "Source for completing and invoking Emacs commands.
A command is a function with interactive spec that can
be invoked with `M-x'.

To get non-interactive functions listed, use
`anything-c-source-emacs-functions'.")
;; (anything 'anything-c-source-emacs-commands)

;;; LaCarte
(defvar anything-c-source-lacarte
  '((name . "Lacarte")
    (init . (lambda () (require 'lacarte )))
    (candidates . (lambda () (delete '(nil) (lacarte-get-overall-menu-item-alist))))
    (candidate-number-limit . 9999)
    (action . anything-c-call-interactively))
  "Needs lacarte.el.

http://www.emacswiki.org/cgi-bin/wiki/download/lacarte.el")
;; (anything 'anything-c-source-lacarte)

;;;; <Function>
;;; Emacs functions
(defvar anything-c-source-emacs-functions
  '((name . "Emacs Functions")
    (candidates . (lambda ()
                    (let (commands)
                      (mapatoms (lambda (a) (if (functionp a)
                                                (push (symbol-name a) commands))))
                      (sort commands 'string-lessp))))
    (type . function)
    (requires-pattern . 2))
  "Source for completing Emacs functions.")
;; (anything 'anything-c-source-emacs-functions)

;;; With abbrev expansion
;;; Similar to my exec-abbrev-cmd.el
;;; See http://www.tsdh.de/cgi-bin/wiki.pl/exec-abbrev-cmd.el
(defvar anything-c-function-abbrev-regexp nil
  "The regexp for `anything-c-source-emacs-functions-with-abbrevs'.
Regexp built from the current `anything-pattern' interpreting it
as abbreviation.
Only for internal use.")

(defun anything-c-match-function-by-abbrev (candidate)
  "Return non-nil if `anything-pattern' is an abbreviation of the function CANDIDATE.

Abbreviations are made by taking the first character from each
word in the function's name, e.g. \"bb\" is an abbrev for
`bury-buffer', \"stb\" is an abbrev for `switch-to-buffer'."
  (string-match anything-c-function-abbrev-regexp candidate))

(defvar anything-c-source-emacs-functions-with-abbrevs
  (append anything-c-source-emacs-functions
          '((match anything-c-match-function-by-abbrev
                   anything-c-string-match))
          '((init . (lambda ()
                      (defadvice anything-update
                        (before anything-c-update-function-abbrev-regexp activate)
                        (let ((char-list (append anything-pattern nil))
                              (str "^"))
                          (dolist (c char-list)
                            (setq str (concat str (list c) "[^-]*-")))
                          (setq str (concat (substring str 0 (1- (length str))) "$"))
                          (setq anything-c-function-abbrev-regexp str))))))))
;; (anything 'anything-c-source-emacs-functions-with-abbrevs)

;;;; <Variable>
;;; Emacs variables
(defvar anything-c-source-emacs-variables
  '((name . "Emacs Variables")
    (candidates . (lambda ()
                    (sort (all-completions "" obarray 'boundp) 'string-lessp)))
    (type . variable)
    (requires-pattern . 2))
  "Source for completing Emacs variables.")
;; (anything 'anything-c-source-emacs-variables)

;;;; <Bookmark>
;;; Bookmarks
(eval-when-compile (require 'bookmark))
(defvar anything-c-source-bookmarks
  '((name . "Bookmarks")
    (init . (lambda ()
              (require 'bookmark)))
    (candidates . bookmark-all-names)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks)

;;; bookmark-set
(defvar anything-c-source-bookmark-set
  '((name . "Set Bookmark")
    (dummy)
    (action . bookmark-set))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmark-set)

;;; Visible Bookmarks
;; (install-elisp "http://cvs.savannah.gnu.org/viewvc/*checkout*/bm/bm/bm.el")


;; http://d.hatena.ne.jp/grandVin/20080911/1221114327
(defvar anything-c-source-bm
  '((name . "Visible Bookmarks")
    (init . anything-c-bm-init)
    (candidates-in-buffer)
    (type . line))
  "Needs bm.el.

http://www.nongnu.org/bm/")

(defun anything-c-bm-init ()
  "Init function for `anything-c-source-bm'."
  (when (require 'bm nil t)
    (with-no-warnings
      (let ((bookmarks (bm-lists))
            (buf (anything-candidate-buffer 'global)))
        (dolist (bm (sort* (append (car bookmarks) (cdr bookmarks))
                           '< :key 'overlay-start))
          (let ((start (overlay-start bm))
                (end (overlay-end bm))
                (annotation (or (overlay-get bm 'annotation) "")))
            (unless (< (- end start) 1) ; org => (if (< (- end start) 2)
              (let ((str (format "%7d: [%s]: %s\n"
                                 (line-number-at-pos start)
                                 annotation
                                 (buffer-substring start (1- end)))))
                (with-current-buffer buf (insert str))))))))))

;;; Special bookmarks
(defvar anything-c-source-bookmarks-ssh
  '((name . "Bookmarks-ssh")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-ssh)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-ssh (loop for i in lis-all
                                          if (string-match "^(ssh)" i)
                                          collect i))
                      (sort lis-ssh 'string-lessp))))
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-ssh)

(defvar anything-c-source-bookmarks-su
  '((name . "Bookmarks-root")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-su)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-su (loop for i in lis-all
                                         if (string-match (format "^(%s)" anything-su-or-sudo) i)
                                         collect i))
                      (sort lis-su 'string-lessp))))
    (candidate-transformer anything-c-highlight-bookmark-su)

    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-su)


(defun tv-root-logged-p ()
  (catch 'break
    (dolist (i (mapcar #'buffer-name (buffer-list)))
      (when (string-match (format "*tramp/%s ." anything-su-or-sudo) i)
        (throw 'break t)))))


(defun anything-c-highlight-bookmark-su (files)
  (if (tv-root-logged-p)
      (anything-c-highlight-bookmark files)
    (anything-c-highlight-not-logged files)))

(defun anything-c-highlight-not-logged (files)
  (loop for i in files
        collect (propertize i 'face anything-c-bookmarks-face3)))

(defun anything-c-highlight-bookmark (bookmarks)
  "Used as `candidate-transformer' to colorize bookmarks.
Work both with standard Emacs bookmarks and bookmark-extensions.el."
  (loop for i in bookmarks
     for pred          = (bookmark-get-filename i)
     for bufp          = (and (fboundp 'bmkext-get-buffer-name)
                              (bmkext-get-buffer-name i))
     for handlerp      = (and (fboundp 'bookmark-get-handler)
                              (bookmark-get-handler i))
     for isw3m         = (and (fboundp 'bmkext-w3m-bookmark-p)
                              (bmkext-w3m-bookmark-p i))
     for isgnus        = (and (fboundp 'bmkext-gnus-bookmark-p)
                              (bmkext-gnus-bookmark-p i)) 
     for isman         = (and (fboundp 'bmkext-man-bookmark-p) ; Man
                              (bmkext-man-bookmark-p i))
     for iswoman       = (and (fboundp 'bmkext-woman-bookmark-p) ; Woman
                              (bmkext-woman-bookmark-p i))
     for handlerp      = (bookmark-get-handler i)
     for isannotation  = (bookmark-get-annotation i)
     ;; Add a * if bookmark have annotation
     if (and isannotation (not (string-equal isannotation "")))
     do (setq i (concat "*" i))
     ;; info buffers
     if (eq handlerp 'Info-bookmark-jump)
     collect (propertize i 'face 'anything-bmkext-info 'help-echo pred)
     ;; w3m buffers
     if isw3m
     collect (propertize i 'face 'anything-bmkext-w3m 'help-echo pred)
     ;; gnus buffers
     if isgnus
     collect (propertize i 'face 'anything-bmkext-gnus 'help-echo pred)
     ;; Man Woman
     if (or iswoman isman) 
     collect (propertize i 'face 'anything-bmkext-man 'help-echo pred)
     ;; directories
     if (and pred (file-directory-p pred))
     collect (propertize i 'face anything-c-bookmarks-face1 'help-echo pred)
     ;; regular files
     if (and pred (not (file-directory-p pred)) (file-exists-p pred)
             (not (or iswoman isman)))
     collect (propertize i 'face 'anything-bmkext-file 'help-echo pred)))
       
;;; Faces for bookmarks
(defface anything-bmkext-info
  '((t (:foreground "green")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'anything)

(defface anything-bmkext-w3m
  '((t (:foreground "yellow")))
  "*Face used for W3m Emacs bookmarks (not w3m bookmarks)."
  :group 'anything)

(defface anything-bmkext-gnus
  '((t (:foreground "magenta")))
  "*Face used for Gnus bookmarks."
  :group 'anything)

(defface anything-bmkext-man
  '((t (:foreground "Orange4")))
  "*Face used for Woman/man bookmarks."
  :group 'anything)

(defface anything-bmkext-no--file
  '((t (:foreground "grey")))
  "*Face used for non--file bookmarks."
  :group 'anything)

(defface anything-bmkext-file
  '((t (:foreground "Deepskyblue2")))
  "*Face used for non--file bookmarks."
  :group 'anything)

(defface anything-bookmarks-su-face '((t (:foreground "red")))
  "Face for su/sudo bookmarks."
  :group 'anything)

(defvar anything-c-bookmarks-face1 'anything-dir-heading)
(defvar anything-c-bookmarks-face2 'anything-file-name)
(defvar anything-c-bookmarks-face3 'anything-bookmarks-su-face)

(defvar anything-c-source-bookmarks-local
  '((name . "Bookmarks-Local")
    (init . (lambda ()
              (require 'bookmark)))
    ;; DRY
    (candidates . (lambda ()
                    (let (lis-all lis-loc)
                      (setq lis-all (bookmark-all-names))
                      (setq lis-loc (loop for i in lis-all
                                          if (and (not (string-match "^(ssh)" i))
                                                  (not (string-match "^(su)" i)))
                                          collect i))
                      (sort lis-loc 'string-lessp))))
    (candidate-transformer anything-c-highlight-bookmark)
    (type . bookmark))
  "See (info \"(emacs)Bookmarks\").")
;; (anything 'anything-c-source-bookmarks-local)

;;; Sources to filter bookmark-extensions bookmarks.
;; Dependency: http://mercurial.intuxication.org/hg/emacs-bookmark-extension


(defun anything-c-bmkext-filter-setup-alist (fn &rest args)
  "Return a filtered `bookmark-alist' sorted alphabetically."
  (loop
     with alist = (if args
                      (apply #'(lambda (x) (funcall fn x)) args)
                      (funcall fn))
     for i in alist
     for b = (car i)
     collect b into sa
     finally return (sort sa 'string-lessp)))

;; W3m
(defvar anything-c-source-bookmark-w3m
  '((name . "Bookmark W3m")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-w3m-setup-alist)
    (candidate-transformer anything-c-highlight-bookmark)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-w3m)

(defun anything-c-bookmark-w3m-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (anything-c-bmkext-filter-setup-alist 'bmkext-w3m-alist-only))

;; Woman Man
(defvar anything-c-source-bookmark-man
  '((name . "Bookmark Woman&Man")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-man-setup-alist)
    (candidate-transformer anything-c-highlight-bookmark)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-man)

(defun anything-c-bookmark-man-setup-alist ()
  "Specialized filter function for bookmarks w3m."
  (append (anything-c-bmkext-filter-setup-alist 'bmkext-man-alist-only)
          (anything-c-bmkext-filter-setup-alist 'bmkext-woman-alist-only)))

;; Gnus
(defvar anything-c-source-bookmark-gnus
  '((name . "Bookmark Gnus")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-gnus-setup-alist)
    (candidate-transformer anything-c-highlight-bookmark)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-gnus)

(defun anything-c-bookmark-gnus-setup-alist ()
  "Specialized filter function for bookmarks gnus."
  (anything-c-bmkext-filter-setup-alist 'bmkext-gnus-alist-only))

;; Info
(defvar anything-c-source-bookmark-info
  '((name . "Bookmark Info")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-info-setup-alist)
    (candidate-transformer anything-c-highlight-bookmark)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-info)

(defun anything-c-bookmark-info-setup-alist ()
  "Specialized filter function for bookmarks info."
  (anything-c-bmkext-filter-setup-alist 'bmkext-info-alist-only))

;; Local Files&directories
(defvar anything-c-source-bookmark-files&dirs
  '((name . "Bookmark Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-local-files-setup-alist)
    (candidate-transformer anything-c-highlight-bookmark)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-files&dirs)

(defun anything-c-bookmark-local-files-setup-alist ()
  "Specialized filter function for bookmarks locals files."
  (anything-c-bmkext-filter-setup-alist 'bmkext-local-file-alist-only))

;; Su Files&directories
(defun anything-c-highlight-bmkext-su (bmk)
  (if (bmkext-root-or-sudo-logged-p)
      (anything-c-highlight-bookmark bmk)
      (anything-c-highlight-not-logged bmk)))

(defvar anything-c-source-bookmark-su-files&dirs
  '((name . "Bookmark Root-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-su-files-setup-alist)
    (candidate-transformer anything-c-highlight-bmkext-su)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-su-files&dirs)

(defun anything-c-bookmark-su-files-setup-alist ()
  "Specialized filter function for bookmarks su/sudo files."
  (loop
     with l = (anything-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
     for i in l
     for isfile = (bookmark-get-filename i)
     for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                        (save-match-data
                          (string-match tramp-file-name-regexp isfile)))
     for issu = (and istramp
                     (string-match bmkext-su-or-sudo-regexp isfile))
     if issu
     collect i))

;; Ssh Files&directories
(defvar anything-c-source-bookmark-ssh-files&dirs
  '((name . "Bookmark Ssh-Files&Directories")
    (init . (lambda ()
              (require 'bookmark-extensions)
              (bookmark-maybe-load-default-file)))
    (candidates . anything-c-bookmark-ssh-files-setup-alist)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (type . bookmark)))
;; (anything 'anything-c-source-bookmark-ssh-files&dirs)

(defun anything-c-bookmark-ssh-files-setup-alist ()
  "Specialized filter function for bookmarks ssh files."
  (loop
     with l = (anything-c-bmkext-filter-setup-alist 'bmkext-remote-file-alist-only)
     for i in l
     for isfile = (bookmark-get-filename i)
     for istramp = (and isfile (boundp 'tramp-file-name-regexp)
                        (save-match-data
                          (string-match tramp-file-name-regexp isfile)))
     for isssh = (and istramp
                      (string-match "/ssh:" isfile))
     if isssh
     collect i))


;; All bookmark-extensions sources.
;;;###autoload
(defun anything-bookmark-ext ()
  "Preconfigured `anything' for bookmark-extensions sources.
See: <http://mercurial.intuxication.org/hg/emacs-bookmark-extension>."
  (interactive)
  (anything '(anything-c-source-bookmark-files&dirs
              anything-c-source-bookmark-w3m
              anything-c-source-bookmark-gnus
              anything-c-source-bookmark-info
              anything-c-source-bookmark-man
              anything-c-source-bookmark-su-files&dirs
              anything-c-source-bookmark-ssh-files&dirs)))


;; Firefox bookmarks
;; You will have to set firefox to import bookmarks in his html file bookmarks.html.
;; (only for firefox versions >=3)
;; To achieve that, open about:config in firefox and double click on this line to enable value
;; to true:
;; user_pref("browser.bookmarks.autoExportHTML", false);
;; You should have now:
;; user_pref("browser.bookmarks.autoExportHTML", true);

(defvar anything-firefox-bookmark-url-regexp "\\(https\\|http\\|ftp\\|about\\|file\\)://[^ ]*")
(defvar anything-firefox-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")

(defun anything-get-firefox-user-init-dir ()
  "Guess the default Firefox user directory name."
  (let* ((moz-dir (concat (getenv "HOME") "/.mozilla/firefox/"))
         (moz-user-dir (with-current-buffer (find-file-noselect (concat moz-dir "profiles.ini"))
                         (goto-char (point-min))
                         (when (search-forward "Path=" nil t)
                           (buffer-substring-no-properties (point) (point-at-eol))))))
    (file-name-as-directory (concat moz-dir moz-user-dir))))

(defun anything-guess-firefox-bookmark-file ()
  "Return the path of the Firefox bookmarks file."
  (concat (anything-get-firefox-user-init-dir) "bookmarks.html"))

(defun anything-html-bookmarks-to-alist (file url-regexp bmk-regexp)
  "Parse html bookmark FILE and return an alist with (title . url) as elements."
  (let (bookmarks-alist url title)
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (while (not (eobp))
        (forward-line)
        (when (re-search-forward "href=\\|^ *<DT><A HREF=" nil t)
          (beginning-of-line)
          (when (re-search-forward url-regexp nil t)
            (setq url (concat "\"" (match-string 0))))
          (beginning-of-line)
          (when (re-search-forward bmk-regexp nil t)
            (setq title (match-string 1)))
          (push (cons title url) bookmarks-alist))))
    (nreverse bookmarks-alist)))


(defvar anything-c-firefox-bookmarks-alist nil)
(defvar anything-c-source-firefox-bookmarks
  '((name . "Firefox Bookmarks")
    (init . (lambda ()
              (setq anything-c-firefox-bookmarks-alist
                    (anything-html-bookmarks-to-alist
                     (anything-guess-firefox-bookmark-file)
                     anything-firefox-bookmark-url-regexp
                     anything-firefox-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car
                            anything-c-firefox-bookmarks-alist)))
    (candidate-transformer anything-c-highlight-firefox-bookmarks)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (action . (("Browse Url" . (lambda (candidate)
                                 (w3m-browse-url
                                  (anything-c-firefox-bookmarks-get-value candidate)))) 
               ("Browse Url Firefox" . (lambda (candidate)
                                         (browse-url-firefox
                                          (anything-c-firefox-bookmarks-get-value candidate)))) 
               ("Copy Url" . (lambda (elm)
                               (kill-new (anything-c-w3m-bookmarks-get-value elm))))))))

;; (anything 'anything-c-source-firefox-bookmarks)

(defun anything-c-firefox-bookmarks-get-value (elm)
  (replace-regexp-in-string "\"" ""
                            (cdr (assoc elm
                                        anything-c-firefox-bookmarks-alist))))


(defun anything-c-highlight-firefox-bookmarks (books)
  (loop for i in books
        collect (propertize i
                            'face '((:foreground "YellowGreen"))
                            'help-echo (anything-c-firefox-bookmarks-get-value i))))

;; W3m bookmark
(eval-when-compile (require 'w3m-bookmark nil t))
(unless (and (require 'w3m nil t)
             (require 'w3m-bookmark nil t))
  (defvar w3m-bookmark-file "~/.w3m/bookmark.html"))


(defface anything-w3m-bookmarks-face '((t (:foreground "cyan1" :underline t)))
  "Face for w3m bookmarks" :group 'anything)

(defvar anything-w3m-bookmarks-regexp ">\\([^><]+.[^</a>]\\)")
(defvar anything-w3m-bookmark-url-regexp "\\(https\\|http\\|ftp\\|file\\)://[^>]*")
(defvar anything-c-w3m-bookmarks-alist nil)
(defvar anything-c-source-w3m-bookmarks
  '((name . "W3m Bookmarks")
    (init . (lambda ()
              (setq anything-c-w3m-bookmarks-alist
                    (anything-html-bookmarks-to-alist
                     w3m-bookmark-file
                     anything-w3m-bookmark-url-regexp
                     anything-w3m-bookmarks-regexp))))
    (candidates . (lambda ()
                    (mapcar #'car anything-c-w3m-bookmarks-alist)))
    (candidate-transformer anything-c-highlight-w3m-bookmarks)
    (filtered-candidate-transformer . anything-c-adaptive-sort)
    (action . (("Browse Url" . (lambda (candidate)
                                 (anything-c-w3m-browse-bookmark candidate)))
               ("Copy Url" . (lambda (elm)
                               (kill-new (anything-c-w3m-bookmarks-get-value elm))))
               ("Browse Url Firefox" . (lambda (candidate)
                                         (anything-c-w3m-browse-bookmark candidate t)))
               ("Delete Bookmark" . (lambda (candidate)
                                      (anything-c-w3m-delete-bookmark candidate)))
               ("Rename Bookmark" . (lambda (candidate)
                                      (anything-c-w3m-rename-bookmark candidate)))))
    (persistent-action . (lambda (candidate)
                           (if current-prefix-arg
                               (anything-c-w3m-browse-bookmark candidate t)
                               (anything-c-w3m-browse-bookmark candidate nil t))))
    (persistent-help . "Open URL with emacs-w3m in new tab / \
C-u \\[anything-execute-persistent-action]: Open URL with Firefox")))

;; (anything 'anything-c-source-w3m-bookmarks)

(defun anything-c-w3m-bookmarks-get-value (elm)
  (replace-regexp-in-string
   "\"" "" (cdr (assoc elm anything-c-w3m-bookmarks-alist))))

(defun anything-c-w3m-browse-bookmark (elm &optional use-firefox new-tab)
  (let* ((fn (if use-firefox 'browse-url-firefox 'w3m-browse-url))
         (arg (and (eq fn 'w3m-browse-url) new-tab)))
    (funcall fn (anything-c-w3m-bookmarks-get-value elm) arg)))

(defun anything-c-highlight-w3m-bookmarks (books)
  (loop for i in books
        collect (propertize
                 i 'face 'anything-w3m-bookmarks-face
                 'help-echo (anything-c-w3m-bookmarks-get-value i))))


(defun anything-c-w3m-delete-bookmark (elm)
  (save-excursion
    (find-file-literally w3m-bookmark-file)
    (goto-char (point-min))
    (when (re-search-forward elm nil t)
      (beginning-of-line)
      (delete-region (point)
                     (line-end-position))
      (delete-blank-lines))
    (save-buffer (current-buffer))
    (kill-buffer (current-buffer))))

(defun anything-c-w3m-rename-bookmark (elm)
  (let* ((old-title (replace-regexp-in-string ">" "" elm))
         (new-title (read-string "NewTitle: " old-title)))
    (save-excursion
      (find-file-literally w3m-bookmark-file)
      (goto-char (point-min))
      (when (re-search-forward (concat elm "<") nil t)
        (goto-char (1- (point)))
        (delete-backward-char (length old-title))
        (insert new-title))
      (save-buffer (current-buffer))
      (kill-buffer (current-buffer)))))

;;;; <Library>
;;; Elisp library scan
(defvar anything-c-source-elisp-library-scan
  '((name . "Elisp libraries (Scan)")
    (init . (anything-c-elisp-library-scan-init))
    (candidates-in-buffer)
    (action ("Find library" . (lambda (candidate)
                                (find-file (find-library-name candidate))))
            ("Find library other window" . (lambda (candidate)
                                             (find-file-other-window (find-library-name candidate))))
            ("Load library" . (lambda (candidate)
                                (load-library candidate))))))
;; (anything 'anything-c-source-elisp-library-scan)

(defun anything-c-elisp-library-scan-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (anything-c-elisp-library-scan-list)))
    (with-current-buffer anything-buffer
      (dolist (library library-list)
        (insert (format "%s\n" library))))))

(defun anything-c-elisp-library-scan-list (&optional dirs string)
  "Do completion for file names passed to `locate-file'.
DIRS is directory to search path.
STRING is string to match."
  ;; Use `load-path' as path when ignore `dirs'.
  (or dirs (setq dirs load-path))
  ;; Init with blank when ignore `string'.
  (or string (setq string ""))
  ;; Get library list.
  (let ((string-dir (file-name-directory string))
        ;; File regexp that suffix match `load-file-rep-suffixes'.
        (match-regexp (format "^.*\\.el%s$" (regexp-opt load-file-rep-suffixes)))
        name
        names)
    (dolist (dir dirs)
      (unless dir
        (setq dir default-directory))
      (if string-dir
          (setq dir (expand-file-name string-dir dir)))
      (when (file-directory-p dir)
        (dolist (file (file-name-all-completions
                       (file-name-nondirectory string) dir))
          ;; Suffixes match `load-file-rep-suffixes'.
          (setq name (if string-dir (concat string-dir file) file))
          (if (string-match match-regexp name)
              (add-to-list 'names name)))))
    names))

;;;; <Programming>
;;; Imenu
(defvar anything-c-imenu-delimiter " / ")

(defvar anything-c-imenu-index-filter nil)
(make-variable-buffer-local 'anything-c-imenu-index-filter)

(defvar anything-c-cached-imenu-alist nil)
(make-variable-buffer-local 'anything-c-cached-imenu-alist)

(defvar anything-c-cached-imenu-candidates nil)
(make-variable-buffer-local 'anything-c-cached-imenu-candidates)

(defvar anything-c-cached-imenu-tick nil)
(make-variable-buffer-local 'anything-c-cached-imenu-tick)

(eval-when-compile (require 'imenu))
(setq imenu-auto-rescan t)

(defun anything-imenu-create-candidates (entry)
  "Create candidates with ENTRY."
  (if (listp (cdr entry))
      (mapcan (lambda (sub)
                (if (consp (cdr sub))
                    (mapcar
                     (lambda (subentry)
                       (concat (car entry) anything-c-imenu-delimiter subentry))
                     (anything-imenu-create-candidates sub))
                  (list (concat (car entry) anything-c-imenu-delimiter (car sub)))))
              (cdr entry))
    (list entry)))

(defvar anything-c-source-imenu
  '((name . "Imenu")
    (candidates . anything-c-imenu-candidates)
    (persistent-action . (lambda (elm)
                           (anything-c-imenu-default-action elm)
                           (unless (fboundp 'semantic-imenu-tag-overlay)
                             (anything-match-line-color-current-line))))
    (persistent-help . "Show this entry")
    (action . anything-c-imenu-default-action))
  "See (info \"(emacs)Imenu\")")

;; (anything 'anything-c-source-imenu)

(defun anything-c-imenu-candidates ()
  (with-current-buffer anything-current-buffer
    (let ((tick (buffer-modified-tick)))
      (if (eq anything-c-cached-imenu-tick tick)
          anything-c-cached-imenu-candidates
        (setq imenu--index-alist nil)
        (setq anything-c-cached-imenu-tick tick
              anything-c-cached-imenu-candidates
              (condition-case nil
                  (mapcan
                   'anything-imenu-create-candidates
                   (setq anything-c-cached-imenu-alist
                         (let ((index (imenu--make-index-alist)))
                           (if anything-c-imenu-index-filter
                               (funcall anything-c-imenu-index-filter index)
                             index))))
                (error nil)))
        (setq anything-c-cached-imenu-candidates
              (mapcar #'(lambda (x)
                          (if (stringp x)
                              x
                            (car x)))
                      anything-c-cached-imenu-candidates))))))

(setq imenu-default-goto-function 'imenu-default-goto-function)
(defun anything-c-imenu-default-action (elm)
  "The default action for `anything-c-source-imenu'."
  (let ((path (split-string elm anything-c-imenu-delimiter))
        (alist anything-c-cached-imenu-alist))
    (if (> (length path) 1)
        (progn
          (setq alist (assoc (car path) alist))
          (setq elm (cadr path))
          (imenu (assoc elm alist)))
      (imenu (assoc elm alist)))))

;;; Ctags
(defvar anything-c-ctags-modes
  '( c-mode c++-mode awk-mode csharp-mode java-mode javascript-mode lua-mode
            makefile-mode pascal-mode perl-mode cperl-mode php-mode python-mode
            scheme-mode sh-mode slang-mode sql-mode tcl-mode ))

(defun anything-c-source-ctags-init ()
  (when (and buffer-file-name
             (memq major-mode anything-c-ctags-modes)
             (anything-current-buffer-is-modified))
    (with-current-buffer (anything-candidate-buffer 'local)
      (call-process-shell-command
       (if (string-match "\\.el\\.gz$" anything-buffer-file-name)
           (format "ctags -e -u -f- --language-force=lisp --fields=n =(zcat %s) " anything-buffer-file-name)
         (format "ctags -e -u -f- --fields=n %s " anything-buffer-file-name))
       nil (current-buffer))
      (goto-char (point-min))
      (forward-line 2)
      (delete-region (point-min) (point))
      (loop while (and (not (eobp)) (search-forward "\001" (point-at-eol) t))
            for lineno-start = (point)
            for lineno = (buffer-substring lineno-start (1- (search-forward "," (point-at-eol) t)))
            do
            (beginning-of-line)
            (insert (format "%5s:" lineno))
            (search-forward "\177" (point-at-eol) t)
            (delete-region (1- (point)) (point-at-eol))
            (forward-line 1)))))

(defvar anything-c-source-ctags
  '((name . "Exuberant ctags")
    (init . anything-c-source-ctags-init)
    (candidates-in-buffer)
    (adjust)
    (type . line))
  "Needs Exuberant Ctags.

http://ctags.sourceforge.net/")
;; (anything 'anything-c-source-ctags)

;; Semantic
(defvar anything-semantic-candidates nil)
(eval-when-compile (require 'semantic nil t))
(defun anything-semantic-construct-candidates (tags depth)
  (when (require 'semantic nil t)
    (apply 'append
           (mapcar (lambda (tag)
                     (if (listp tag)
                         (let ((type (semantic-tag-type tag))
                               (class (semantic-tag-class tag)))
                           (if (or (and (stringp type)
                                        (or (string= type "class")
                                            (string= type "namespace")))
                                   (eq class 'function)
                                   (eq class 'variable))
                               (cons (cons (concat (make-string (* depth 2) ?\s)
                                                   (semantic-format-tag-summarize tag nil t)) tag)
                                     (anything-semantic-construct-candidates (semantic-tag-components tag)
                                                                             (1+ depth)))))))
                   tags))))

(defun anything-semantic-default-action (candidate)
  (let ((tag (cdr (assoc candidate anything-semantic-candidates))))
    (semantic-go-to-tag tag)))

(defvar anything-c-source-semantic
  '((name . "Semantic Tags")
    (init . (lambda ()
              (setq anything-semantic-candidates
                    (condition-case nil
                        (anything-semantic-construct-candidates (semantic-fetch-tags) 0)
                      (error nil)))))
    (candidates . (lambda ()
                    (if anything-semantic-candidates
                        (mapcar 'car anything-semantic-candidates))))
    (persistent-action . (lambda (elm)
                           (anything-semantic-default-action elm)
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . anything-semantic-default-action)
  "Needs semantic in CEDET.

http://cedet.sourceforge.net/semantic.shtml
http://cedet.sourceforge.net/"))

;; (anything 'anything-c-source-semantic)

;;; Function is called by
;;;###autoload
(defun anything-simple-call-tree ()
  "Preconfigured `anything' for simple-call-tree. List function relationships."
  (interactive)
  (anything-other-buffer
   '(anything-c-source-simple-call-tree-functions-callers
     anything-c-source-simple-call-tree-callers-functions)
   "*anything simple-call-tree*"))

(defvar anything-c-source-simple-call-tree-functions-callers
  '((name . "Function is called by")
    (init . anything-c-simple-call-tree-functions-callers-init)
    (multiline)
    (candidates . anything-c-simple-call-tree-candidates)
    (persistent-action . anything-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             anything-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defvar anything-c-simple-call-tree-tick nil)
(make-variable-buffer-local 'anything-c-simple-call-tree-tick)
(defun anything-c-simple-call-tree-analyze-maybe ()
  (unless (eq (buffer-chars-modified-tick) anything-c-simple-call-tree-tick)
    (simple-call-tree-analyze)
    (setq anything-c-simple-call-tree-tick (buffer-chars-modified-tick))))

(defun anything-c-simple-call-tree-init-base (function message)
  (require 'simple-call-tree)
  (with-no-warnings
    (when (anything-current-buffer-is-modified)
      (anything-c-simple-call-tree-analyze-maybe)
      (let ((list (funcall function simple-call-tree-alist)))
        (with-current-buffer (anything-candidate-buffer 'local)
          (dolist (entry list)
            (let ((funcs (concat "  " (mapconcat #'identity (cdr entry) "\n  "))))
              (insert (car entry) message
                      (if (string= funcs "  ")
                          "  no functions."
                        funcs)
                      "\n\n"))))))))

(defun anything-c-simple-call-tree-functions-callers-init ()
  (anything-c-simple-call-tree-init-base 'simple-call-tree-invert " is called by\n"))

(defun anything-c-simple-call-tree-candidates ()
  (with-current-buffer (anything-candidate-buffer)
    (split-string (buffer-string) "\n\n")))

(defvar anything-c-simple-call-tree-related-functions nil)
(defvar anything-c-simple-call-tree-function-index 0)
(defun anything-c-simple-call-tree-persistent-action (candidate)
  (unless (eq last-command 'anything-execute-persistent-action)
    (setq anything-c-simple-call-tree-related-functions
          (delete "no functions."
                  (split-string
                   (replace-regexp-in-string "  \\| is called by\\| calls " "" candidate)
                   "\n")))
    (setq anything-c-simple-call-tree-function-index -1))
  (incf anything-c-simple-call-tree-function-index)
  (anything-c-simple-call-tree-find-definition candidate))

(defun anything-c-simple-call-tree-find-definition (candidate)
  (find-function (intern
                  (nth (mod anything-c-simple-call-tree-function-index
                            (length anything-c-simple-call-tree-related-functions))
                       anything-c-simple-call-tree-related-functions))))

;; (anything 'anything-c-source-simple-call-tree-functions-callers)

;;; Function calls
(defvar anything-c-source-simple-call-tree-callers-functions
  '((name . "Function calls")
    (init . anything-c-simple-call-tree-callers-functions-init)
    (multiline)
    (candidates . anything-c-simple-call-tree-candidates)
    (persistent-action . anything-c-simple-call-tree-persistent-action)
    (persistent-help . "Show function definitions by rotation")
    (action ("Find definition selected by persistent-action" .
             anything-c-simple-call-tree-find-definition)))
  "Needs simple-call-tree.el.
http://www.emacswiki.org/cgi-bin/wiki/download/simple-call-tree.el")

(defun anything-c-simple-call-tree-callers-functions-init ()
  (anything-c-simple-call-tree-init-base 'identity " calls \n"))

;; (anything 'anything-c-source-simple-call-tree-callers-functions)

;;; Commands/Options with doc
(defvar anything-c-auto-document-data nil)
(make-variable-buffer-local 'anything-c-auto-document-data)
(defvar anything-c-source-commands-and-options-in-file
  '((name . "Commands/Options in file")
    (header-name
     . (lambda (x) (format "Commands/Options in %s"
                           (buffer-local-value 'buffer-file-name anything-current-buffer))))
    (candidates . anything-command-and-options-candidates)
    (multiline)
    (action . imenu))
  "List Commands and Options with doc. It needs auto-document.el .

http://www.emacswiki.org/cgi-bin/wiki/download/auto-document.el")

(eval-when-compile (require 'auto-document nil t))
(defun anything-command-and-options-candidates ()
  (with-current-buffer anything-current-buffer
    (when (and (require 'auto-document nil t)
               (eq major-mode 'emacs-lisp-mode)
               (or (anything-current-buffer-is-modified)
                   (not anything-c-auto-document-data)))
      (or imenu--index-alist (imenu--make-index-alist t))
      (setq anything-c-auto-document-data
            (destructuring-bind (commands options)
                (adoc-construct anything-current-buffer)
              (append
               (loop for (command . doc) in commands
                     for cmdname = (symbol-name command)
                     collect
                     (cons (format "Command: %s\n %s"
                                   (propertize cmdname 'face font-lock-function-name-face)
                                   (adoc-first-line doc))
                           (assoc cmdname imenu--index-alist)))
               (loop with var-alist = (cdr (assoc "Variables" imenu--index-alist))
                     for (option doc default) in options
                     for optname = (symbol-name option)
                     collect
                     (cons (format "Option: %s\n %s\n default = %s"
                                   (propertize optname 'face font-lock-variable-name-face)
                                   (adoc-first-line doc)
                                   (adoc-prin1-to-string default))
                           (assoc optname
                                  var-alist)))))))
    anything-c-auto-document-data))

;; (anything 'anything-c-source-commands-and-options-in-file)

;;;; <Color and Face>
;;; Customize Face
(defvar anything-c-source-customize-face
  '((name . "Customize Face")
    (init . (lambda ()
              (unless (anything-candidate-buffer)
                (save-window-excursion (list-faces-display))
                (anything-candidate-buffer (get-buffer "*Faces*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action . (lambda (line)
                (customize-face (intern (car (split-string line))))))
    (requires-pattern . 3))
  "See (info \"(emacs)Faces\")")
;; (anything 'anything-c-source-customize-face)

;; Color
(defvar anything-c-source-colors
  '((name . "Colors")
    (init . (lambda () (unless (anything-candidate-buffer)
                         (save-window-excursion (list-colors-display))
                         (anything-candidate-buffer (get-buffer "*Colors*")))))
    (candidates-in-buffer)
    (get-line . buffer-substring)
    (action ("Copy Name" . (lambda (candidate)
                             (kill-new (anything-c-colors-get-name candidate))))
            ("Copy RGB" . (lambda (candidate)
                            (kill-new (anything-c-colors-get-rgb candidate))))
            ("Insert Name" . (lambda (candidate)
                               (with-current-buffer anything-current-buffer
                                 (insert (anything-c-colors-get-name candidate)))))
            ("Insert RGB" . (lambda (candidate)
                              (with-current-buffer anything-current-buffer
                                (insert (anything-c-colors-get-rgb candidate))))))))
;; (anything 'anything-c-source-colors)

(defun anything-c-colors-get-name (candidate)
  "Get color name."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-min))
     (search-forward-regexp "\\s-\\{2,\\}")
     (kill-line)
     (buffer-string))))

(defun anything-c-colors-get-rgb (candidate)
  "Get color RGB."
  (replace-regexp-in-string
   " " ""
   (with-temp-buffer
     (insert (capitalize candidate))
     (goto-char (point-max))
     (search-backward-regexp "\\s-\\{2,\\}")
     (kill-region (point) (point-min))
     (buffer-string))))

;;;; <Search Engine>
;;; Tracker desktop search
(defvar anything-c-source-tracker-search
  '((name . "Tracker Search")
    (candidates . (lambda ()
                    (start-process "tracker-search-process" nil
                                   "tracker-search"
                                   anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files matching the current input pattern
with the tracker desktop search.")
;; (anything 'anything-c-source-tracker-search)

;;; Spotlight (MacOS X desktop search)
(defvar anything-c-source-mac-spotlight
  '((name . "mdfind")
    (candidates . (lambda ()
                    (start-process "mdfind-process" nil "mdfind" anything-pattern)))
    (type . file)
    (requires-pattern . 3)
    (delayed))
  "Source for retrieving files via Spotlight's command line
utility mdfind.")
;; (anything 'anything-c-source-mac-spotlight)


;;;; <Kill ring>
;;; Kill ring
(defvar anything-c-source-kill-ring
  '((name . "Kill Ring")
    (init . (lambda () (anything-attrset 'last-command last-command)))
    (candidates . (lambda ()
                    (loop for kill in kill-ring
                          unless (or (< (length kill) anything-kill-ring-threshold)
                                     (string-match "^[\\s\\t]+$" kill))
                          collect kill)))
    (action . anything-c-kill-ring-action)
    (last-command)
    (migemo)
    (multiline))
  "Source for browse and insert contents of kill-ring.")

(defun anything-c-kill-ring-action (str)
  "Insert STR in `kill-ring' and set STR to the head.
If this action is executed just after `yank', replace with STR as yanked string."
  (setq kill-ring (delete str kill-ring))
  (if (not (eq (anything-attr 'last-command) 'yank))
      (insert-for-yank str)
    ;; from `yank-pop'
    (let ((inhibit-read-only t)
          (before (< (point) (mark t))))
      (if before
          (funcall (or yank-undo-function 'delete-region) (point) (mark t))
        (funcall (or yank-undo-function 'delete-region) (mark t) (point)))
      (setq yank-undo-function nil)
      (set-marker (mark-marker) (point) (current-buffer))
      (insert-for-yank str)
      ;; Set the window start back where it was in the yank command,
      ;; if possible.
      (set-window-start (selected-window) yank-window-start t)
      (if before
          ;; This is like exchange-point-and-mark, but doesn't activate the mark.
          ;; It is cleaner to avoid activation, even though the command
          ;; loop would deactivate the mark because we inserted text.
          (goto-char (prog1 (mark t)
                       (set-marker (mark-marker) (point) (current-buffer)))))))
  (kill-new str))

;; (anything 'anything-c-source-kill-ring)

;;;; <Mark ring>
;; DO NOT include these sources in `anything-sources' use
;; the commands `anything-mark-ring' and `anything-global-mark-ring' instead.

(defun anything-c-source-mark-ring-candidates ()
  (flet ((get-marks (pos)
           (save-excursion
             (goto-char pos)
             (beginning-of-line)
             (let ((line  (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (when (string= "" line)
                 (setq line  "<EMPTY LINE>"))
               (format "%7d: %s" (line-number-at-pos) line)))))
    (with-current-buffer anything-current-buffer
      (loop
         with marks = (cons (mark-marker) mark-ring)
         with recip = nil
         for i in marks
         for f = (get-marks i) 
         if (not (member f recip))
         do
           (push f recip)
         finally (return (reverse recip))))))
           
(defvar anything-mark-ring-cache nil)
(defvar anything-c-source-mark-ring
  '((name . "mark-ring")
    (init . (lambda ()
              (setq anything-mark-ring-cache
                    (anything-c-source-mark-ring-candidates))))
    (candidates . (lambda ()
                    (anything-aif anything-mark-ring-cache
                        it)))
    (action . (("Goto line" . (lambda (candidate)
                                (anything-goto-line (string-to-number candidate))))))
    (persistent-action . (lambda (candidate)
                           (anything-goto-line (string-to-number candidate))
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this line")))

;; (anything 'anything-c-source-mark-ring)

;;;###autoload
(defun anything-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-mark-ring'."
  (interactive)
  (anything 'anything-c-source-mark-ring))

;;; Global-mark-ring
(defvar anything-c-source-global-mark-ring
  '((name . "global-mark-ring")
    (candidates . anything-c-source-global-mark-ring-candidates)
    (action . (("Goto line" . (lambda (candidate)
                                (let ((items (split-string candidate ":")))
                                  (switch-to-buffer (second items))
                                  (anything-goto-line (string-to-number (car items))))))))
    (persistent-action . (lambda (candidate)
                           (let ((items (split-string candidate ":")))
                             (switch-to-buffer (second items))
                             (anything-goto-line (string-to-number (car items)))
                             (anything-match-line-color-current-line))))
    (persistent-help . "Show this line")))
                             
(defun anything-c-source-global-mark-ring-candidates ()
  (flet ((buf-fn (m)
           (with-current-buffer (marker-buffer m)
             (goto-char m)
             (beginning-of-line)
             (let (line)
               (if (string= "" line)
                   (setq line  "<EMPTY LINE>")
                   (setq line (car (split-string (thing-at-point 'line) "[\n\r]"))))
               (format "%7d:%s:    %s" (line-number-at-pos) (marker-buffer m) line)))))
    (loop
       with marks = global-mark-ring
       with recip = nil  
       for i in marks
       if (not (or (string-match "^ " (format "%s" (marker-buffer i)))
                   (null (marker-buffer i))))
       for a = (buf-fn i)
       if (and a (not (member a recip)))
       do
         (push a recip)
       finally (return (reverse recip)))))

;; (anything 'anything-c-source-global-mark-ring)

;;;###autoload
(defun anything-global-mark-ring ()
  "Preconfigured `anything' for `anything-c-source-global-mark-ring'."
  (interactive)
  (anything 'anything-c-source-global-mark-ring))

;;;; <Register>
;;; Insert from register
(defvar anything-c-source-register
  '((name . "Registers")
    (candidates . anything-c-register-candidates)
    (action-transformer . anything-c-register-action-transformer)
    (multiline)
    (action))
  "See (info \"(emacs)Registers\")")

(defun anything-c-register-candidates ()
  "Collecting register contents and appropriate commands."
  (loop for (char . val) in register-alist
        for key    = (single-key-description char)
        for string-actions = (cond
                              ((numberp val)
                               (list (int-to-string val)
                                     'insert-register
                                     'increment-register))
                              ((markerp val)
                               (let ((buf (marker-buffer val)))
                                 (if (null buf)
                                     (list "a marker in no buffer")
                                   (list (concat
                                          "a buffer position:"
                                          (buffer-name buf)
                                          ", position "
                                          (int-to-string (marker-position val)))
                                         'jump-to-register
                                         'insert-register))))
                              ((and (consp val) (window-configuration-p (car val)))
                               (list "window configuration."
                                     'jump-to-register))
                              ((and (consp val) (frame-configuration-p (car val)))
                               (list "frame configuration."
                                     'jump-to-register))
                              ((and (consp val) (eq (car val) 'file))
                               (list (concat "file:"
                                             (prin1-to-string (cdr val))
                                             ".")
                                     'jump-to-register))
                              ((and (consp val) (eq (car val) 'file-query))
                               (list (concat "file:a file-query reference: file "
                                             (car (cdr val))
                                             ", position "
                                             (int-to-string (car (cdr (cdr val))))
                                             ".")
                                     'jump-to-register))
                              ((consp val)
                               (let ((lines (format "%4d" (length val))))
                                 (list (format "%s: %s\n" lines
                                               (truncate-string-to-width
                                                (mapconcat 'identity (list (car val))
                                                           ;; (mapconcat (lambda (y) y) val
                                                           "^J") (- (window-width) 15)))
                                       'insert-register)))
                              ((stringp val)
                               (list ;; without properties
                                (substring-no-properties val)
                                'insert-register
                                'append-to-register
                                'prepend-to-register))
                              (t
                               "GARBAGE!"))
        collect (cons (format "register %3s: %s" key (car string-actions))
                      (cons char (cdr string-actions)))))

(defun anything-c-register-action-transformer (actions register-and-functions)
  "Decide actions by the contents of register."
  (loop with func-actions =
        '((insert-register
           "Insert Register" .
           (lambda (c) (insert-register (car c))))
          (jump-to-register
           "Jump to Register" .
           (lambda (c) (jump-to-register (car c))))
          (append-to-register
           "Append Region to Register" .
           (lambda (c) (append-to-register (car c) (region-beginning) (region-end))))
          (prepend-to-register
           "Prepend Region to Register" .
           (lambda (c) (prepend-to-register (car c) (region-beginning) (region-end))))
          (increment-register
           "Increment Prefix Arg to Register" .
           (lambda (c) (increment-register anything-current-prefix-arg (car c)))))
        for func in (cdr register-and-functions)
        for cell = (assq func func-actions)
        when cell
        collect (cdr cell)))

;; (anything 'anything-c-source-register)

;;;; <Headline Extraction>
(defvar anything-c-source-fixme
  '((name . "TODO/FIXME/DRY comments")
    (headline . "^.*\\<\\(TODO\\|FIXME\\|DRY\\)\\>.*$")
    (adjust)
    (recenter))
  "Show TODO/FIXME/DRY comments in current file.")
;; (anything 'anything-c-source-fixme)

(defvar anything-c-source-rd-headline
  '((name . "RD HeadLine")
    (headline  "^= \\(.+\\)$" "^== \\(.+\\)$" "^=== \\(.+\\)$" "^==== \\(.+\\)$")
    (condition . (memq major-mode '(rdgrep-mode rd-mode)))
    (migemo)
    (subexp . 1))
  "Show RD headlines.

RD is Ruby's POD.
http://en.wikipedia.org/wiki/Ruby_Document_format")
;; (anything 'anything-c-source-rd-headline)

(defvar anything-c-source-oddmuse-headline
  '((name . "Oddmuse HeadLine")
    (headline  "^= \\(.+\\) =$" "^== \\(.+\\) ==$"
               "^=== \\(.+\\) ===$" "^==== \\(.+\\) ====$")
    (condition . (memq major-mode '(oddmuse-mode yaoddmuse-mode)))
    (migemo)
    (subexp . 1))
  "Show Oddmuse headlines, such as EmacsWiki.")
;; (anything 'anything-c-source-oddmuse-headline)

(defvar anything-c-source-emacs-source-defun
  '((name . "Emacs Source DEFUN")
    (headline . "DEFUN\\|DEFVAR")
    (condition . (string-match "/emacs2[0-9].+/src/.+c$" (or buffer-file-name ""))))
  "Show DEFUN/DEFVAR in Emacs C source file.")
;; (anything 'anything-c-source-emacs-source-defun)

(defvar anything-c-source-emacs-lisp-expectations
  '((name . "Emacs Lisp Expectations")
    (headline . "(desc[ ]\\|(expectations")
    (condition . (eq major-mode 'emacs-lisp-mode)))
  "Show descriptions (desc) in Emacs Lisp Expectations.

http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (anything 'anything-c-source-emacs-lisp-expectations)

(defvar anything-c-source-emacs-lisp-toplevels
  '((name . "Emacs Lisp Toplevel / Level 4 Comment / Linkd Star")
    (headline . "^(\\|(@\\*\\|^;;;;")
    (get-line . buffer-substring)
    (condition . (eq major-mode 'emacs-lisp-mode))
    (adjust))
  "Show top-level forms, level 4 comments and linkd stars (optional) in Emacs Lisp.
linkd.el is optional because linkd stars are extracted by regexp.
http://www.emacswiki.org/cgi-bin/wiki/download/linkd.el")
;; (anything 'anything-c-source-emacs-lisp-toplevels)

(defvar anything-c-source-org-headline
  '((name . "Org HeadLine")
    (headline
     "^\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$"
     "^\\*\\*\\*\\*\\*\\*\\*\\* \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$")
    (condition . (eq major-mode 'org-mode))
    (migemo)
    (subexp . 1)
    (persistent-action . (lambda (elm)
                           (anything-c-action-line-goto elm)
                           (org-cycle)))
    (action-transformer
     . (lambda (actions candidate)
         '(("Go to Line" . anything-c-action-line-goto)
           ("Insert Link to This Headline" . anything-c-org-headline-insert-link-to-headline)))))
  "Show Org headlines.
org-mode is very very much extended text-mode/outline-mode.

See (find-library \"org.el\")
See http://orgmode.org for the latest version.")

(defun anything-c-org-headline-insert-link-to-headline (lineno-and-content)
  (insert
   (save-excursion
     (anything-goto-line (car lineno-and-content))
     (and (looking-at "^\\*+ \\(.+?\\)\\([ \t]*:[a-zA-Z0-9_@:]+:\\)?[ \t]*$")
          (org-make-link-string (concat "*" (match-string 1)))))))

;; (anything 'anything-c-source-org-headline)

;;; Anything yaoddmuse
;; Be sure to have yaoddmuse.el installed
;; install-elisp may be required if you want to install elisp file from here.
(defvar anything-yaoddmuse-use-cache-file nil)
(defvar anything-c-yaoddmuse-cache-file "~/.emacs.d/yaoddmuse-cache.el")
(defvar anything-c-yaoddmuse-ew-cache nil)
(defvar anything-c-source-yaoddmuse-emacswiki-edit-or-view
  '((name . "Yaoddmuse Edit or View (EmacsWiki)")
    (candidates . (lambda ()
                    (if anything-yaoddmuse-use-cache-file
                        (condition-case nil
                            (progn
                              (unless anything-c-yaoddmuse-ew-cache
                                (load anything-c-yaoddmuse-cache-file)
                                (setq anything-c-yaoddmuse-ew-cache
                                      (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                              anything-c-yaoddmuse-ew-cache)
                          (error nil))
                        (yaoddmuse-update-pagename t)
                        (gethash "EmacsWiki" yaoddmuse-pages-hash))))
    (action . (("Edit page" . (lambda (candidate)
                                (yaoddmuse-edit "EmacsWiki" candidate)))
               ("Browse page" . (lambda (candidate)
                                  (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse page other window" . (lambda (candidate)
                                               (if (one-window-p)
                                                   (split-window-vertically))
                                               (yaoddmuse-browse-page "EmacsWiki" candidate)))
               ("Browse diff" . (lambda (candidate)
                                  (yaoddmuse-browse-page-diff "EmacsWiki" candidate)))
               ("Copy URL" . (lambda (candidate)
                               (kill-new (yaoddmuse-url "EmacsWiki" candidate))
                               (message "Have copy page %s's URL to yank." candidate)))
               ("Create page" . (lambda (candidate)
                                  (yaoddmuse-edit "EmacsWiki" anything-input)))
               ("Update cache" . (lambda (candidate)
                                   (if anything-yaoddmuse-use-cache-file
                                       (progn
                                         (anything-yaoddmuse-cache-pages t)
                                         (setq anything-c-yaoddmuse-ew-cache
                                               (gethash "EmacsWiki" yaoddmuse-pages-hash)))
                                       (yaoddmuse-update-pagename))))))
    (action-transformer anything-c-yaoddmuse-action-transformer))) 

;; (anything 'anything-c-source-yaoddmuse-emacswiki-edit-or-view)

(defvar anything-c-source-yaoddmuse-emacswiki-post-library
  '((name . "Yaoddmuse Post library (EmacsWiki)")
    (init . (anything-yaoddmuse-init))
    (candidates-in-buffer)
    (action . (("Post library and Browse" . (lambda (candidate)
                                              (yaoddmuse-post-file (find-library-name candidate)
                                                                   "EmacsWiki"
                                                                   (file-name-nondirectory (find-library-name candidate))
                                                                   nil t)))
               ("Post library" . (lambda (candidate)
                                   (yaoddmuse-post-file (find-library-name candidate)
                                                        "EmacsWiki"
                                                        (file-name-nondirectory (find-library-name candidate)))))))))

;; (anything 'anything-c-source-yaoddmuse-emacswiki-post-library)

(defun anything-c-yaoddmuse-action-transformer (actions candidate)
  "Allow the use of `install-elisp' only on elisp files."
  (if (string-match "\.el$" candidate)
      (append actions '(("Install Elisp" . (lambda (elm)
                                             (install-elisp-from-emacswiki elm)))))
      actions))

;;;###autoload
(defun anything-yaoddmuse-cache-pages (&optional load)
  "Fetch the list of files on emacswiki and create cache file.
If load is non--nil load the file and feed `yaoddmuse-pages-hash'."
  (interactive)
  (yaoddmuse-update-pagename)
  (save-excursion
    (find-file anything-c-yaoddmuse-cache-file)
    (erase-buffer)
    (insert "(puthash \"EmacsWiki\" '(")
    (loop for i in (gethash "EmacsWiki" yaoddmuse-pages-hash)
       do
          (insert (concat "(\"" (car i) "\") ")))
    (insert ") yaoddmuse-pages-hash)\n")
    (save-buffer)
    (kill-buffer (current-buffer))
    (when (or current-prefix-arg
              load)
      (load anything-c-yaoddmuse-cache-file))))

;;;###autoload
(defun anything-yaoddmuse-emacswiki-edit-or-view ()
  "Preconfigured `anything' to edit or view EmacsWiki page."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-edit-or-view))

;;;###autoload
(defun anything-yaoddmuse-emacswiki-post-library ()
  "Preconfigured `anything' to post library to EmacsWiki."
  (interactive)
  (anything 'anything-c-source-yaoddmuse-emacswiki-post-library))

(defun anything-yaoddmuse-init ()
  "Init anything buffer status."
  (let ((anything-buffer (anything-candidate-buffer 'global))
        (library-list (yaoddmuse-get-library-list)))
    (with-current-buffer anything-buffer
      ;; Insert library name.
      (dolist (library library-list)
        (insert (format "%s\n" library)))
      ;; Sort lines.
      (sort-lines nil (point-min) (point-max)))))

;;; Eev anchors
(defvar anything-c-source-eev-anchor
  '((name . "Anchors")
    (init . (lambda ()
              (setq anything-c-eev-anchor-buffer
                    (current-buffer))))
    (candidates . (lambda ()
                    (condition-case nil
                        (save-excursion
                          (with-current-buffer anything-c-eev-anchor-buffer
                            (goto-char (point-min))
                            (let (anchors)
                              (while (re-search-forward (format ee-anchor-format "\\([^\.].+\\)") nil t)
                                (push (match-string-no-properties 1) anchors))
                              (setq anchors (reverse anchors)))))
                      (error nil))))
    (persistent-action . (lambda (item)
                           (ee-to item)
                           (anything-match-line-color-current-line)))
    (persistent-help . "Show this entry")
    (action . (("Goto link" . (lambda (item)
                                (ee-to item)))))))

;; (anything 'anything-c-source-eev-anchor)

;;;; <Misc>
;;; Org keywords
(defvar anything-c-source-org-keywords
  '((name . "Org Keywords")
    (init . anything-c-org-keywords-init)
    (candidates . anything-c-org-keywords-candidates)
    (action . anything-c-org-keywords-insert)
    (persistent-action . anything-c-org-keywords-show-help)
    (persistent-help . "Show an example and info page to describe this keyword.")
    (keywords-examples)
    (keywords)))
;; (anything 'anything-c-source-org-keywords)
(defvar anything-c-org-keywords-info-location
  '(("#+TITLE:" . "(org)Export options")
    ("#+AUTHOR:" . "(org)Export options")
    ("#+DATE:" . "(org)Export options")
    ("#+EMAIL:" . "(org)Export options")
    ("#+DESCRIPTION:" . "(org)Export options")
    ("#+KEYWORDS:" . "(org)Export options")
    ("#+LANGUAGE:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+TEXT:" . "(org)Export options")
    ("#+OPTIONS:" . "(org)Export options")
    ("#+BIND:" . "(org)Export options")
    ("#+LINK_UP:" . "(org)Export options")
    ("#+LINK_HOME:" . "(org)Export options")
    ("#+LATEX_HEADER:" . "(org)Export options")
    ("#+EXPORT_SELECT_TAGS:" . "(org)Export options")
    ("#+EXPORT_EXCLUDE_TAGS:" . "(org)Export options")
    ("#+INFOJS_OPT" . "(org)Javascript support")
    ("#+BEGIN_HTML" . "(org)Quoting HTML tags")
    ("#+BEGIN_LaTeX" . "(org)Quoting LaTeX code")
    ("#+ORGTBL" . "(org)Radio tables")
    ("#+HTML:" . "(org)Quoting HTML tags")
    ("#+LaTeX:" . "(org)Quoting LaTeX code")
    ("#+BEGIN:" . "(org)Dynamic blocks") ;clocktable columnview
    ("#+BEGIN_EXAMPLE" . "(org)Literal examples")
    ("#+BEGIN_QUOTE" . "(org)Paragraphs")
    ("#+BEGIN_VERSE" . "(org)Paragraphs")
    ("#+BEGIN_SRC" . "(org)Literal examples")
    ("#+CAPTION" . "(org)Tables in HTML export")
    ("#+LABEL" . "(org)Tables in LaTeX export")
    ("#+ATTR_HTML" . "(org)Links")
    ("#+ATTR_LaTeX" . "(org)Images in LaTeX export")))

(defun anything-c-org-keywords-init ()
  (unless (anything-attr 'keywords-examples)
    (require 'org)
    (anything-attrset 'keywords-examples
                      (append
                       (mapcar
                        (lambda (x)
                          (string-match "^#\\+\\(\\([A-Z_]+:?\\).*\\)" x)
                          (cons (match-string 2 x) (match-string 1 x)))
                        (org-split-string (org-get-current-options) "\n"))
                       (mapcar 'list org-additional-option-like-keywords)))
    (anything-attrset 'keywords (mapcar 'car (anything-attr 'keywords-examples)))))

(defun anything-c-org-keywords-candidates ()
  (and (eq (buffer-local-value 'major-mode anything-current-buffer) 'org-mode)
       (anything-attr 'keywords)))

(defun anything-c-org-keywords-insert (keyword)
  (cond ((string-match "BEGIN" keyword)
         (insert "#+" keyword " ")
         (save-excursion
           (insert "\n" (replace-regexp-in-string "BEGIN" "END" keyword) "\n")))
        (t
         (insert "#+" keyword " "))))

(defun anything-c-org-keywords-show-help (keyword)
  (info (or (assoc-default (concat "#+" keyword) anything-c-org-keywords-info-location)
            "(org)In-buffer settings"))
  (search-forward (concat "#+" keyword) nil t)
  (anything-persistent-highlight-point)
  (message "%s" (or (cdr (assoc keyword (anything-attr 'keywords-examples))) "")))


;;; Picklist
(defvar anything-c-source-picklist
  '((name . "Picklist")
    (candidates . (lambda () (mapcar 'car picklist-list)))
    (type . file)))
;; (anything 'anything-c-source-picklist)

;;; BBDB
(defun anything-c-bbdb-candidates ()
  "Return a list of all names in the bbdb database.  The format
is \"Firstname Lastname\"."
  (mapcar (lambda (bbdb-record)
            (replace-regexp-in-string
             "\\s-+$" ""
             (concat (aref bbdb-record 0) " " (aref bbdb-record 1))))
          (bbdb-records)))

(defun anything-c-bbdb-create-contact (actions candidate)
  "Action transformer that returns only an entry to add the
current `anything-pattern' as new contact.  All other actions are
removed."
  (if (string= candidate "*Add to contacts*")
      '(("Add to contacts" . (lambda (actions)
                               (bbdb-create-internal
                                (read-from-minibuffer "Name: " anything-c-bbdb-name)
                                (read-from-minibuffer "Company: ")
                                (read-from-minibuffer "Email: ")
                                nil
                                nil
                                (read-from-minibuffer "Note: ")))))
    actions))

(defun anything-c-bbdb-get-record (candidate)
  "Return record that match CANDIDATE."
  (bbdb candidate nil)
  (set-buffer "*BBDB*")
  (bbdb-current-record))

(defvar anything-c-bbdb-name nil
  "Only for internal use.")

(defvar anything-c-source-bbdb
  '((name . "BBDB")
    (candidates . anything-c-bbdb-candidates)
    (action ("Send a mail" . anything-c-bbdb-compose-mail)
            ("View person's data" . anything-c-bbdb-view-person-action))
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (setq anything-c-bbdb-name anything-pattern)
                                        (if (not candidates)
                                            (list "*Add to contacts*")
                                          candidates)))
    (action-transformer . (lambda (actions candidate)
                            (anything-c-bbdb-create-contact actions candidate)))))
;; (anything 'anything-c-source-bbdb)

(defun anything-c-bbdb-view-person-action (candidate)
  "View BBDB data of single CANDIDATE or marked candidates."
  (anything-aif (anything-marked-candidates)
      (let ((bbdb-append-records (length it)))
        (dolist (i it)
          (bbdb-redisplay-one-record (anything-c-bbdb-get-record i))))
    (bbdb-redisplay-one-record (anything-c-bbdb-get-record candidate))))

(defun anything-c-bbdb-collect-mail-addresses ()
  "Return a list of all mail addresses of records in bbdb buffer."
  (with-current-buffer bbdb-buffer-name
    (loop for i in bbdb-records
       if (bbdb-record-net (car i))
       collect (bbdb-dwim-net-address (car i)))))

(defun anything-c-bbdb-compose-mail (candidate)
  "Compose a mail with all records of bbdb buffer."
  (anything-c-bbdb-view-person-action candidate)
  (let* ((address-list (anything-c-bbdb-collect-mail-addresses))
         (address-str  (mapconcat 'identity address-list ",\n    ")))
    (compose-mail address-str)))

;;; Evaluation Result
(defvar anything-c-source-evaluation-result
  '((name . "Evaluation Result")
    (dummy)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (pp-to-string
                                              (eval (read anything-pattern)))
                                           (error "Error")))))
    (action ("Do Nothing" . ignore))))
;; (anything 'anything-c-source-evaluation-result)

;;; Calculation Result
(defvar anything-c-source-calculation-result
  '((name . "Calculation Result")
    (dummy)
    (filtered-candidate-transformer . (lambda (candidates source)
                                        (list
                                         (condition-case nil
                                             (calc-eval anything-pattern)
                                           (error "error")))))
    (action ("Copy result to kill-ring" . kill-new))))
;; (anything 'anything-c-source-calculation-result)

;;; Google Suggestions
(defvar anything-gg-sug-lgh-flag 0)
(defun anything-c-google-suggest-fetch (input)
  "Fetch suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat anything-c-google-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                with result-alist = (xml-get-children
                                     (car (xml-parse-region (point-min) (point-max)))
                                     'CompleteSuggestion)
                for i in result-alist
                for data = (cdr (caadr (assoc 'suggestion i)))
                for nqueries = (cdr (caadr (assoc 'num_queries i)))
                for ldata = (length data) 
                do
                  (when (> ldata anything-gg-sug-lgh-flag)
                    (setq anything-gg-sug-lgh-flag ldata))
                collect (cons data nqueries) into cont
                finally return cont)))
      (if anything-google-suggest-use-curl-p
          (with-temp-buffer
            (call-process "curl" nil t nil request)
            (fetch))
          (with-current-buffer
              (url-retrieve-synchronously request)
            (fetch))))))


(defun anything-c-google-suggest-set-candidates ()
  "Set candidates with result and number of google results found."
  (let ((suggestions (anything-c-google-suggest-fetch anything-input)))
    (setq suggestions (loop for i in suggestions
                         for interval = (- anything-gg-sug-lgh-flag (length (car i)))
                         for elm = (concat (car i)
                                           (make-string (+ 2 interval) ? )
                                           "(" (cdr i) " results)")
                         collect (cons elm (car i))))
    (if (some (lambda (data) (equal (cdr data) anything-input)) suggestions)
        suggestions
        ;; if there is no suggestion exactly matching the input then
        ;; prepend a Search on Google item to the list
        (append
         suggestions
         (list (cons (concat "Search for " "'" anything-input "'" " on Google")
                     anything-input))))))
         
    
(defun anything-c-google-suggest-action (candidate)
  "Default action to jump to a google suggested candidate."
  (browse-url (concat anything-c-google-suggest-search-url
                      (url-hexify-string candidate))))


(defvar anything-c-source-google-suggest
  '((name . "Google Suggest")
    (candidates . anything-c-google-suggest-set-candidates)
    (action . (("Google Search" . anything-c-google-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

;; (anything 'anything-c-source-google-suggest)

;;; Yahoo suggestions

(defun anything-c-yahoo-suggest-fetch (input)
  "Fetch Yahoo suggestions for INPUT from XML buffer.
Return an alist with elements like (data . number_results)."
  (let ((request (concat anything-c-yahoo-suggest-url
                         (url-hexify-string input))))
    (flet ((fetch ()
             (loop
                with result-alist = (xml-get-children
                                     (car (xml-parse-region (point-min) (point-max)))
                                     'Result)
                for i in result-alist
                collect (caddr i))))
      (with-current-buffer
          (url-retrieve-synchronously request)
        (fetch)))))

(defun anything-c-yahoo-suggest-set-candidates ()
  "Set candidates with Yahoo results found."
  (let ((suggestions (anything-c-yahoo-suggest-fetch anything-input)))
    (or suggestions
        (append
         suggestions
         (list (cons (concat "Search for " "'" anything-input "'" " on Yahoo")
                     anything-input))))))
         
(defun anything-c-yahoo-suggest-action (candidate)
  "Default action to jump to a Yahoo suggested candidate."
  (browse-url (concat anything-c-yahoo-suggest-search-url
                      (url-hexify-string candidate))))

(defvar anything-c-source-yahoo-suggest
  '((name . "Yahoo Suggest")
    (candidates . anything-c-yahoo-suggest-set-candidates)
    (action . (("Yahoo Search" . anything-c-yahoo-suggest-action)))
    (volatile)
    (requires-pattern . 3)
    (delayed)))

;; (anything 'anything-c-source-yahoo-suggest)

;;; Surfraw
;;; Need external program surfraw.
;;; http://surfraw.alioth.debian.org/
;; user variables
(defvar anything-c-surfraw-favorites '("google" "wikipedia"
                                       "yahoo" "translate"
                                       "codesearch" "genpkg"
                                       "genportage" "fast" 
                                       "currency")
  "All elements of this list will appear first in results.")
(defvar anything-c-surfraw-use-only-favorites nil
  "If non-nil use only `anything-c-surfraw-favorites'.")


(defun anything-c-build-elvi-alist ()
  "Build elvi alist.
A list of search engines."
  (let* ((elvi-list
          (with-temp-buffer
            (call-process "surfraw" nil t nil
                          "-elvi")
            (split-string (buffer-string) "\n")))
         (elvi-alist
          (let (line)
            (loop for i in elvi-list
               do
               (setq line (split-string i))
               collect (cons (first line) (mapconcat #'(lambda (x) x) (cdr line) " "))))))
    elvi-alist))

(defun anything-c-surfraw-sort-elvi (&optional only-fav)
  "Sort elvi alist according to `anything-c-surfraw-favorites'."
  (let* ((elvi-alist (anything-c-build-elvi-alist))
         (fav-alist (loop for j in anything-c-surfraw-favorites
                      collect (assoc j elvi-alist)))
         (rest-elvi (loop for i in elvi-alist
                         if (not (member i fav-alist))
                         collect i)))
    (if only-fav
        fav-alist
        (append fav-alist rest-elvi))))

(defun anything-c-surfraw-get-url (engine pattern)
  "Get search url from `engine' for `anything-pattern'."
  (with-temp-buffer
    (apply #'call-process "surfraw" nil t nil
           `(,engine
             "-p"
             ,anything-pattern))
    (buffer-string)))


(defvar anything-c-surfraw-elvi nil)
(defvar anything-c-surfraw-cache nil)
(defvar anything-c-source-surfraw
  '((name . "Surfraw")
    (init . (lambda ()
              (unless anything-c-surfraw-cache
                (setq anything-c-surfraw-elvi (anything-c-surfraw-sort-elvi
                                               anything-c-surfraw-use-only-favorites))
                (setq anything-c-surfraw-cache
                      (loop for i in anything-c-surfraw-elvi 
                         if (car i)
                         collect (car i))))))
    (candidates . (lambda ()
                    (loop for i in anything-c-surfraw-cache
                       for s = (anything-c-surfraw-get-url i anything-pattern)
                       collect (concat (propertize i
                                                   'face '((:foreground "green"))
                                                   'help-echo (cdr (assoc i anything-c-surfraw-elvi)))
                                       ">>>" (replace-regexp-in-string "\n" "" s)))))
    (action . (("Browse" . (lambda (candidate)
                             (let ((url (second (split-string candidate ">>>"))))
                               (browse-url url))))
               ("Browse firefox" . (lambda (candidate)
                                     (let ((url (second (split-string candidate ">>>"))))
                                       (browse-url-firefox url t))))))
    (volatile)
    (requires-pattern . 3)
    (multiline)
    (delayed)))

;; (anything 'anything-c-source-surfraw)

;;; Emms

;;;###autoload
(defun anything-emms-stream-edit-bookmark (elm)
  "Change the information of current emms-stream bookmark from anything."
  (interactive)
  (let* ((cur-buf anything-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name     (read-from-minibuffer "Description: "
                                         (nth 0 bookmark)))
         (url      (read-from-minibuffer "URL: "
                                         (nth 1 bookmark)))
         (fd       (read-from-minibuffer "Feed Descriptor: "
                                         (int-to-string (nth 2 bookmark))))
         (type     (read-from-minibuffer "Type (url, streamlist, or lastfm): "
                                         (format "%s" (car (last bookmark))))))
    (save-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-add-bookmark name url (string-to-number fd) type)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (switch-to-buffer cur-buf)))))

(defun anything-emms-stream-delete-bookmark (elm)
  "Delete an emms-stream bookmark from anything."
  (interactive)
  (let* ((cur-buf anything-current-buffer)
         (bookmark (assoc elm emms-stream-list))
         (name (nth 0 bookmark)))
    (save-excursion
      (emms-streams)
      (when (re-search-forward (concat "^" name) nil t)
        (beginning-of-line)
        (emms-stream-delete-bookmark)
        (emms-stream-save-bookmarks-file)
        (emms-stream-quit)
        (switch-to-buffer cur-buf)))))

(defvar anything-c-source-emms-streams
  '((name . "Emms Streams")
    (init . (lambda ()
              (emms-stream-init)))
    (candidates . (lambda ()
                    (mapcar 'car emms-stream-list)))
    (action . (("Play" . (lambda (elm)
                           (let* ((stream (assoc elm emms-stream-list))
                                  (fn (intern (concat "emms-play-" (symbol-name (car (last stream))))))
                                  (url (second stream)))
                             (funcall fn url))))
               ("Delete" . anything-emms-stream-delete-bookmark)
               ("Edit" . anything-emms-stream-edit-bookmark)))
    (filtered-candidate-transformer . anything-c-adaptive-sort)))
;; (anything 'anything-c-source-emms-streams)

;; Don't forget to set `emms-source-file-default-directory'
(defvar anything-c-source-emms-dired
  '((name . "Music Directory")
    (candidates . (lambda ()
                    (cddr (directory-files emms-source-file-default-directory))))
    (action .
     (("Play Directory" . (lambda (item)
                            (emms-play-directory
                             (expand-file-name
                              item
                              emms-source-file-default-directory))))
      ("Open dired in file's directory" . (lambda (item)
                                            (anything-c-open-dired
                                             (expand-file-name
                                              item
                                              emms-source-file-default-directory))))))
    (filtered-candidate-transformer . anything-c-adaptive-sort)))
;; (anything 'anything-c-source-emms-dired)

;;; Jabber Contacts (jabber.el)
(defun anything-c-jabber-online-contacts ()
  "List online Jabber contacts."
  (with-no-warnings
    (let (jids)
      (dolist (item (jabber-concat-rosters) jids)
        (when (get item 'connected)
          (push (if (get item 'name)
                    (cons (get item 'name) item)
                  (cons (symbol-name item) item)) jids))))))

(defvar anything-c-source-jabber-contacts
  '((name . "Jabber Contacts")
    (init . (lambda () (require 'jabber)))
    (candidates . (lambda () (mapcar 'car (anything-c-jabber-online-contacts))))
    (action . (lambda (x)
                (jabber-chat-with
                 (jabber-read-account)
                 (symbol-name
                  (cdr (assoc x (anything-c-jabber-online-contacts)))))))))
;; (anything 'anything-c-source-jabber-contacts)


;;; Call source.
(defvar anything-source-select-buffer "*anything source select*")
(defvar anything-c-source-call-source
  `((name . "Call anything source")
    (candidate-number-limit)
    (candidates . (lambda ()
                    (loop for vname in (all-completions "anything-c-source-" obarray)
                          for var = (intern vname)
                          for name = (ignore-errors (assoc-default 'name (symbol-value var)))
                          if name collect (cons (format "%s `%s'"
                                                        name (propertize vname 'face 'font-lock-variable-name-face))
                                                var))))
    (action . (("Invoke anything with selected source" .
                (lambda (candidate)
                  (setq anything-candidate-number-limit 9999)
                  (anything candidate nil nil nil nil
                            anything-source-select-buffer)))
               ("Describe variable" . describe-variable)))
    (persistent-action . describe-variable)
    (persistent-help . "Show description of this source")))
;; (anything 'anything-c-source-call-source)

;;;###autoload
(defun anything-call-source ()
  "Preconfigured `anything' to call anything source."
  (interactive)
  (anything 'anything-c-source-call-source nil nil nil nil
            anything-source-select-buffer))

(defun anything-call-source-from-anything ()
  "Call anything source within `anything' session."
  (interactive)
  (setq anything-input-idle-delay 0)
  (anything-set-sources '(anything-c-source-call-source)))


;; Occur

(defun anything-c-occur-goto-line (candidate)
  "Goto line action for anything-c-source-occur."
  (anything-goto-line (string-to-number candidate)))

(defun anything-c-occur-persistent-action (candidate)
  "Persistent action for anything occur source."
  (anything-c-occur-goto-line candidate)
  (anything-match-line-color-current-line))

(defun anything-c-get-occur-candidates ()
  "Get `anything-c-source-occur' candidates."
  (with-temp-buffer
    (let ((count (occur-engine
                  anything-pattern
                  (list anything-current-buffer) (current-buffer)
                  list-matching-lines-default-context-lines nil
                  list-matching-lines-buffer-name-face
                  nil list-matching-lines-face
                  (not (eq occur-excluded-properties t)))))
      (when (> count 0)
        (cdr (split-string (buffer-string) "\n" t))))))

(defvar anything-c-source-occur
  '((name . "Occur")
    (candidates . anything-c-get-occur-candidates)
    (persistent-action . anything-c-occur-persistent-action)
    (persistent-help . "Goto Line")
    (action . anything-c-occur-goto-line)
    (requires-pattern . 1)
    (delayed)
    (volatile)))
;; (anything 'anything-c-source-occur)

;;;###autoload
(defun anything-occur ()
  "Preconfigured Anything for Occur source."
  (interactive)
  (anything-other-buffer 'anything-c-source-occur "*Anything Occur*"))

;; Do many actions for input
(defvar anything-c-source-create
  '((name . "Create")
    (dummy)
    (action)
    (candidate-number-limit . 9999)
    (action-transformer . anything-create--actions))
  "Do many create actions from `anything-pattern'.
See also `anything-create--actions'.")
;; (anything 'anything-c-source-create)

(defun anything-create-from-anything ()
  "Run `anything-create' from `anything' as a fallback."
  (interactive)
  (anything-run-after-quit 'anything-create nil anything-pattern))

;;;###autoload
(defun anything-create (&optional string initial-input)
  "Preconfigured `anything' to do many create actions from STRING.
See also `anything-create--actions'."
  (interactive)
  (setq string (or string (read-string "Create Anything: " initial-input)))
  (anything '(((name . "Anything Create")
               (header-name . (lambda (_) (format "Action for \"%s\"" string)))
               (candidates . anything-create--actions)
               (candidate-number-limit . 9999)
               (action . (lambda (func) (funcall func string)))))))

(defun anything-create--actions (&rest ignored)
  "Default actions for `anything-create' / `anything-c-source-create'."
  (remove-if-not
   (lambda (pair) (and (consp pair) (functionp (cdr pair))))
   (append anything-create--actions-private
           '(("find-file" . find-file)
             ("find-file other window" . find-file-other-window)
             ("New buffer" . switch-to-buffer)
             ("New buffer other window" . switch-to-buffer-other-window)
             ("Bookmark Set" . bookmark-set)
             ("Set Register" .
              (lambda (x) (set-register (read-char "Register: ") x)))
             ("Insert Linkd star" . linkd-insert-star)
             ("Insert Linkd Tag" . linkd-insert-tag)
             ("Insert Linkd Link" . linkd-insert-link)
             ("Insert Linkd Lisp" . linkd-insert-lisp)
             ("Insert Linkd Wiki" . linkd-insert-wiki)
             ("Google Search" . google)))))

;; Minibuffer History
(defvar anything-c-source-minibuffer-history
  '((name . "Minibuffer History")
    (header-name . (lambda (name) (format "%s (%s)" name minibuffer-history-variable)))
    (candidates . (lambda () (let ((history (symbol-value minibuffer-history-variable)))
                               (if (consp (car history))
                                   (mapcar 'prin1-to-string history)
                                 history))))
    (migemo)
    (action . insert)))
;; (anything 'anything-c-source-minibuffer-history)

;; elscreen
(defvar anything-c-source-elscreen
  '((name . "Elscreen")
    (candidates . (lambda ()
                    (if (cdr (elscreen-get-screen-to-name-alist))
                        (sort
                         (loop for sname in (elscreen-get-screen-to-name-alist)
                               append (list (format "[%d] %s" (car sname) (cdr sname))) into lst
                               finally (return lst))
                         #'(lambda (a b) (compare-strings a nil nil b nil nil))))))
    (action . (("Change Screen".
                (lambda (candidate)
                  (elscreen-goto (- (aref candidate 1) (aref "0" 0)))))
               ("Kill Screen(s)".
                (lambda (candidate)
                  (anything-aif (anything-marked-candidates)
                      (dolist (i it)
                        (elscreen-kill-internal (- (aref i 1) (aref "0" 0))))
                    (elscreen-kill-internal (- (aref candidate 1) (aref "0" 0))))))
               ("Only Screen".
                (lambda (candidate)
                  (elscreen-goto (- (aref candidate 1) (aref "0" 0)))
                  (elscreen-kill-others)))))))
;; (anything 'anything-c-source-elscreen)

;;;; <System>

;;; Top (process)
(defvar anything-c-top-command "COLUMNS=%s top -b -n 1"
  "Top command (batch mode). %s is replaced with `frame-width'.")
(defvar anything-c-source-top
  '((name . "Top (Press C-c C-u to refresh)")
    (init . anything-c-top-init)
    (candidates-in-buffer)
    (display-to-real . anything-c-top-display-to-real)
    (update . anything-c-top-update)
    (action
     ("kill (TERM)" . (lambda (pid) (anything-c-top-sh (format "kill -TERM %s" pid))))
     ("kill (KILL)" . (lambda (pid) (anything-c-top-sh (format "kill -KILL %s" pid))))
     ("Copy PID" . (lambda (pid) (kill-new pid))))))
;; (anything 'anything-c-source-top)

(defun anything-c-top-sh (cmd)
  (message "Executed %s\n%s" cmd (shell-command-to-string cmd)))

(defun anything-c-top-init ()
  (with-current-buffer (anything-candidate-buffer 'global)
    (call-process-shell-command
     (format anything-c-top-command
             (- (frame-width) (if anything-enable-digit-shortcuts 4 0)))
     nil (current-buffer))))

(defun anything-c-top-display-to-real (line)
  (car (split-string line)))

(defun anything-c-top-update ()
  (let ((anything-source-name (assoc-default 'name anything-c-source-top))) ;UGLY HACK
    (anything-c-top-init)))

;;;###autoload
(defun anything-top ()
  "Preconfigured `anything' for top command."
  (interactive)
  (let ((anything-samewindow t)
        (anything-enable-shortcuts)
        (anything-display-function 'anything-default-display-buffer)
        (anything-candidate-number-limit 9999))
    (save-window-excursion
      (delete-other-windows)
      (anything-other-buffer 'anything-c-source-top "*anything top*"))))

;;; Timers
(defvar anything-c-source-absolute-time-timers
  '((name . "Absolute Time Timers")
    (candidates . timer-list)
    (type . timer)))
;; (anything 'anything-c-source-absolute-time-timers)

(defvar anything-c-source-idle-time-timers
  '((name . "Idle Time Timers")
    (candidates . timer-idle-list)
    (type . timer)))
;; (anything 'anything-c-source-idle-time-timers)

(defun anything-c-timer-real-to-display (timer)
  (destructuring-bind (_ t1 t2 t3 _ func args &rest rest) (append timer nil)
    (format "%s %s(%s)"
            (format-time-string "%m/%d %T" (list t1 t2 t3))
            func
            (mapconcat 'prin1-to-string (aref timer 6) " "))))

;;; X RandR resolution change
;;; FIXME I do not care multi-display.
(defvar anything-c-xrandr-output "VGA")
(defvar anything-c-xrandr-screen "0")
(defvar anything-c-source-xrandr-change-resolution
  '((name . "Change Resolution")
    (candidates
     . (lambda ()
         (with-temp-buffer
           (call-process "xrandr" nil (current-buffer) nil
                         "--screen" anything-c-xrandr-screen "-q")
           (goto-char 1)
           (loop while (re-search-forward "   \\([0-9]+x[0-9]+\\)" nil t)
                 collect (match-string 1)))))
    (action
     ("Change Resolution" . (lambda (mode)
                              (call-process "xrandr" nil nil nil
                                            "--screen" anything-c-xrandr-screen
                                            "--output" anything-c-xrandr-output
                                            "--mode" mode))))))
;; (anything 'anything-c-source-xrandr-change-resolution)

;;; Xfont selection
(defun anything-c-persistent-xfont-action (elm)
  "Show current font temporarily"
  (let ((current-font (cdr (assoc 'font (frame-parameters))))
        (default-font elm))
    (unwind-protect
         (progn (set-frame-font default-font 'keep-size) (sit-for 2))
      (set-frame-font current-font))))

(defvar anything-c-xfonts-cache nil)
(defvar anything-c-source-xfonts
  '((name . "X Fonts")
    (init . (lambda ()
              (unless anything-c-xfonts-cache
                (setq anything-c-xfonts-cache
                      (x-list-fonts "*")))))  
    (candidates . anything-c-xfonts-cache)
    (action . (("Copy to kill ring" . (lambda (elm)
                                        (kill-new elm)))
               ("Set Font" . (lambda (elm)
                               (kill-new elm)
                               (set-frame-font elm 'keep-size)
                               (message "New font have been copied to kill ring")))))
    (persistent-action . anything-c-persistent-xfont-action)
    (persistent-help . "Switch to this font temporarily")))

;;;###autoload
(defun anything-select-xfont ()
  "Preconfigured `anything' to select Xfont."
  (interactive)
  (anything-other-buffer 'anything-c-source-xfonts "*anything select* xfont"))

;; (anything 'anything-c-source-xfonts)

;; Source for Debian/Ubuntu users
(defvar anything-c-source-apt
  '((name . "APT")
    (init . anything-c-apt-init)
    (candidates-in-buffer)
    (display-to-real . anything-c-apt-display-to-real)
    (candidate-number-limit . 9999)
    (action
     ("Show package description" . anything-c-apt-cache-show)
     ("Install package" . anything-c-apt-install))))
;; (anything 'anything-c-source-apt)

(defvar anything-c-apt-query "emacs")
(defvar anything-c-apt-search-command "apt-cache search '%s'")
(defvar anything-c-apt-show-command "apt-cache show '%s'")
(defvar anything-c-apt-install-command "xterm -e sudo apt-get install '%s' &")

;;;###autoload
(defun anything-apt (query)
  "Preconfigured `anything' : frontend of APT package manager."
  (interactive "sAPT search: ")
  (let ((anything-c-apt-query query))
    (anything 'anything-c-source-apt)))

(defun anything-c-apt-init ()
  (with-current-buffer
      (anything-candidate-buffer
       (get-buffer-create (format "*anything-apt:%s*" anything-c-apt-query)))
    (call-process-shell-command
     (format anything-c-apt-search-command anything-c-apt-query)
     nil (current-buffer))))
(defun anything-c-apt-display-to-real (line)
  (car (split-string line " - ")))

;;;###autoload
(defun anything-c-shell-command-if-needed (command)
  (interactive "sShell command: ")
  (if (get-buffer command)		; if the buffer already exists
      (switch-to-buffer command)	; then just switch to it
    (switch-to-buffer command)		; otherwise create it
    (insert (shell-command-to-string command))))

(defun anything-c-apt-cache-show (package)
  (anything-c-shell-command-if-needed (format anything-c-apt-show-command package)))
(defun anything-c-apt-install (package)
  (shell-command (format anything-c-apt-install-command package) "*apt install*"))

;; (anything-c-apt-install "jed")
;; Sources for gentoo users

(defvar anything-gentoo-prefered-shell 'eshell
  "Your favorite shell to run emerge command.")

(defvar anything-c-gentoo-use-flags nil)
(defvar anything-c-gentoo-buffer "*anything-gentoo-output*")
(defvar anything-c-cache-gentoo nil)
(defvar anything-c-cache-world nil)
(defvar anything-c-source-gentoo
  '((name . "Portage sources")
    (init . (lambda ()
              (get-buffer-create anything-c-gentoo-buffer)
              (unless anything-c-cache-gentoo
                (anything-c-gentoo-setup-cache))
              (unless anything-c-cache-world
                (setq anything-c-cache-world (anything-c-gentoo-get-world)))
              (anything-c-gentoo-init-list)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-world)
    (action . (("Show package" . (lambda (elm)
                                   (anything-c-gentoo-eshell-action elm "eix")))
               ("Show history" . (lambda (elm)
                                   (if (member elm anything-c-cache-world)
                                       (anything-c-gentoo-eshell-action elm "genlop -qe")
                                       (message "No infos on packages not yet installed"))))
               ("Copy in kill-ring" . kill-new)
               ("insert at point" . insert)
               ("Browse HomePage" . (lambda (elm)
                                      (browse-url (car (anything-c-gentoo-get-url elm)))))
               ("Show extra infos" . (lambda (elm)
                                       (if (member elm anything-c-cache-world)
                                           (anything-c-gentoo-eshell-action elm "genlop -qi")
                                           (message "No infos on packages not yet installed"))))
               ("Show use flags" . (lambda (elm)
                                     (anything-c-gentoo-default-action elm "equery" "-C" "u")
                                     (font-lock-add-keywords nil '(("^\+.*" . font-lock-variable-name-face)))
                                     (font-lock-mode 1)))
               ("Run emerge pretend" . (lambda (elm)
                                         (anything-c-gentoo-eshell-action elm "emerge -p")))
               ("Emerge" . (lambda (elm)
                             (anything-gentoo-install elm :action 'install)))
               ("Unmerge" . (lambda (elm)
                              (anything-gentoo-install elm :action 'uninstall)))
               ("Show dependencies" . (lambda (elm)
                                        (anything-c-gentoo-default-action elm "equery" "-C" "d")))
               ("Show related files" . (lambda (elm)
                                         (anything-c-gentoo-default-action elm "equery" "files")))
               ("Refresh" . (lambda (elm)
                              (anything-c-gentoo-setup-cache)
                              (setq anything-c-cache-world (anything-c-gentoo-get-world))))))))

;; (anything 'anything-c-source-gentoo)

(defun* anything-gentoo-install (candidate &key action)
  (funcall anything-gentoo-prefered-shell)
  (let ((command (case action
                   ('install "*sudo emerge -av ")
                   ('uninstall "*sudo emerge -avC ")
                   (t (error "Unknow action")))))
  (if (anything-marked-candidates)
      (let ((elms (mapconcat 'identity (anything-marked-candidates) " ")))
        (insert (concat command elms)))
      (insert (concat command candidate)))))


(defun anything-c-gentoo-default-action (elm command &rest args)
  "Gentoo default action that use `anything-c-gentoo-buffer'."
  (if (member elm anything-c-cache-world)
      (progn
        (switch-to-buffer anything-c-gentoo-buffer)
        (erase-buffer)
        (let ((com-list (append args (list elm))))
          (apply #'call-process command nil t nil
                 com-list)))
      (message "No infos on packages not yet installed")))

(defvar anything-c-source-use-flags
  '((name . "Use Flags")
    (init . (lambda ()
              (unless anything-c-gentoo-use-flags
                (anything-c-gentoo-setup-use-flags-cache))
              (anything-c-gentoo-get-use)))
    (candidates-in-buffer)
    (match . identity)
    (candidate-transformer anything-c-highlight-local-use)
    (action . (("Description"
                . (lambda (elm)
                    (switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "euse" nil t nil
                           `("-i"
                             ,elm))
                    (font-lock-add-keywords nil `((,elm . font-lock-variable-name-face)))
                    (font-lock-mode 1)))
               ("Enable"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo euse -E")))
               ("Disable"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo euse -D")))
               ("Remove"
                . (lambda (elm)
                    (anything-c-gentoo-eshell-action elm "*sudo euse -P")))
               ("Show which dep use this flag"
                . (lambda (elm)
                    (switch-to-buffer anything-c-gentoo-buffer)
                    (erase-buffer)
                    (apply #'call-process "equery" nil t nil
                           `("-C"
                             "h"
                             ,elm))))))))


;; (anything 'anything-c-source-use-flags)

(defun anything-c-gentoo-init-list ()
  "Initialize buffer with all packages in Portage."
  (let* ((portage-buf (get-buffer-create "*anything-gentoo*"))
         (buf (anything-candidate-buffer 'portage-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-cache-gentoo)
        (insert (concat i "\n"))))))

(defun anything-c-gentoo-setup-cache ()
  "Set up `anything-c-cache-gentoo'"
  (setq anything-c-cache-gentoo
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--only-names")
                        (buffer-string)))))

(defun anything-c-gentoo-eshell-action (elm command)
  (when (get-buffer "*EShell Command Output*")
    (kill-buffer "*EShell Command Output*"))
  (message "Wait searching...")
  (let ((buf-fname (buffer-file-name anything-current-buffer)))
    (if (and buf-fname (string-match tramp-file-name-regexp buf-fname))
        (progn
          (save-window-excursion
            (pop-to-buffer "*scratch*")
            (eshell-command (format "%s %s" command elm)))
          (pop-to-buffer "*EShell Command Output*"))
        (eshell-command (format "%s %s" command elm)))))

(defun anything-c-gentoo-get-use ()
  "Initialize buffer with all use flags."
  (let* ((use-buf (get-buffer-create "*anything-gentoo-use*"))
         (buf (anything-candidate-buffer 'use-buf)))
    (with-current-buffer buf
      (dolist (i anything-c-gentoo-use-flags)
        (insert (concat i "\n"))))))


(defun anything-c-gentoo-setup-use-flags-cache ()
  "Setup `anything-c-gentoo-use-flags'"
  (setq anything-c-gentoo-use-flags
        (split-string (with-temp-buffer
                        (call-process "eix" nil t nil
                                      "--print-all-useflags")
                        (buffer-string)))))

(defun anything-c-gentoo-get-url (elm)
  "Return a list of urls from eix output."
  (split-string (eshell-command-result
                 (format "eix %s | grep Homepage | awk '{print $2}'" elm))))

(defun anything-c-gentoo-get-world ()
  "Return list of all installed package on your system."
  (split-string (with-temp-buffer
                  (call-process "qlist" nil t nil
                                "-I")
                  (buffer-string))))

(defun anything-c-gentoo-get-local-use ()
  (split-string (with-temp-buffer
                  (call-process "portageq" nil t nil
                                "envvar"
                                "USE")
                  (buffer-string))))

(defface anything-gentoo-match-face '((t (:foreground "red")))
  "Face for anything-gentoo installed packages."
  :group 'traverse-faces)

(defun anything-c-highlight-world (eix)
  "Highlight all installed package."
  (loop for i in eix
        if (member i anything-c-cache-world)
        collect (propertize i 'face 'anything-gentoo-match-face)
        else
        collect i))

(defun anything-c-highlight-local-use (use-flags)
  (let ((local-uses (anything-c-gentoo-get-local-use)))
    (loop for i in use-flags
          if (member i local-uses)
          collect (propertize i 'face 'anything-gentoo-match-face)
          else
          collect i)))

(defvar anything-c-source-emacs-process
  '((name . "Emacs Process")
    (candidates . (lambda () (mapcar #'process-name (process-list))))
    (action ("Kill Process" . (lambda (elm) (delete-process (get-process elm)))))))

;; (anything 'anything-c-source-emacs-process)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Action Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
(defvar anything-c-external-commands-list nil
  "A list of all external commands the user can execute.  If this
variable is not set by the user, it will be calculated
automatically.")

(defun anything-c-external-commands-list-1 ()
  "Returns a list of all external commands the user can execute.

If `anything-c-external-commands-list' is non-nil it will
return its contents.  Else it calculates all external commands
and sets `anything-c-external-commands-list'.

The code is ripped out of `eshell-complete-commands-list'."
  (if anything-c-external-commands-list
      anything-c-external-commands-list
    (setq anything-c-external-commands-list
          (let* ((paths (split-string (getenv "PATH") path-separator))
                 (cwd (file-name-as-directory
                       (expand-file-name default-directory)))
                 (path "") (comps-in-path ())
                 (file "") (filepath "") (completions ()))
            ;; Go thru each path in the search path, finding completions.
            (while paths
              (setq path (file-name-as-directory
                          (expand-file-name (or (car paths) ".")))
                    comps-in-path
                    (and (file-accessible-directory-p path)
                         (file-name-all-completions "" path)))
              ;; Go thru each completion found, to see whether it should be
              ;; used, e.g. see if it's executable.
              (while comps-in-path
                (setq file (car comps-in-path)
                      filepath (concat path file))
                (if (and (not (member file completions))
                         (or (string-equal path cwd)
                             (not (file-directory-p filepath)))
                         (file-executable-p filepath))
                    (setq completions (cons file completions)))
                (setq comps-in-path (cdr comps-in-path)))
              (setq paths (cdr paths)))
            completions))))

(defun anything-c-file-buffers (filename)
  "Returns a list of buffer names corresponding to FILENAME."
  (let ((name     (expand-file-name filename))
        (buf-list ()))
    (dolist (buf (buffer-list) buf-list)
      (let ((bfn (buffer-file-name buf)))
        (when (and bfn (string= name bfn))
          (push (buffer-name buf) buf-list))))))

(defun anything-c-delete-file (file)
  "Delete the given file after querying the user.
Ask to kill buffers associated with that file, too."
  (let ((buffers (anything-c-file-buffers file)))
    (dired-delete-file file 'dired-recursive-deletes)
    (when buffers
      (dolist (buf buffers)
        (when (y-or-n-p (format "Kill buffer %s, too? " buf))
          (kill-buffer buf))))))

(defun anything-c-open-file-externally (file)
  "Open FILE with an external tool. Query the user which tool to use."
  (start-process "anything-c-open-file-externally"
                 nil
                 (completing-read "Program: "
                                  (anything-c-external-commands-list-1))
                 file))

;;;###autoload
(defun w32-shell-execute-open-file (file)
  (interactive "fOpen file:")
  (with-no-warnings
    (w32-shell-execute "open" (replace-regexp-in-string ;for UNC paths
                               "/" "\\"
                               (replace-regexp-in-string ; strip cygdrive paths
                                "/cygdrive/\\(.\\)" "\\1:" file nil nil) nil t))))
(defun anything-c-open-file-with-default-tool (file)
  "Open FILE with the default tool on this platform."
  (if (eq system-type 'windows-nt)
      (w32-shell-execute-open-file file)
    (start-process "anything-c-open-file-with-default-tool"
                   nil
                   (cond ((eq system-type 'gnu/linux)
                          "xdg-open")
                         ((or (eq system-type 'darwin) ;; Mac OS X
                              (eq system-type 'macos)) ;; Mac OS 9
                          "open"))
                   file)))

(defun anything-c-open-dired (file)
  "Opens a dired buffer in FILE's directory.  If FILE is a
directory, open this directory."
  (if (file-directory-p file)
      (dired file)
    (dired (file-name-directory file))
    (dired-goto-file file)))

(defun anything-c-display-to-real-line (candidate)
  (if (string-match "^ *\\([0-9]+\\):\\(.*\\)$" candidate)
      (list (string-to-number (match-string 1 candidate)) (match-string 2 candidate))
    (error "Line number not found")))

(defun anything-c-action-line-goto (lineno-and-content)
  (apply #'anything-goto-file-line (anything-attr 'target-file)
         (append lineno-and-content
                 (list (if (and (anything-attr-defined 'target-file)
                                (not anything-in-persistent-action))
                           'find-file-other-window
                         'find-file)))))

(defun* anything-c-action-file-line-goto (file-line-content &optional (find-file-function #'find-file))
  (apply #'anything-goto-file-line file-line-content))

(require 'compile)
(defun anything-c-filtered-candidate-transformer-file-line (candidates source)
  (mapcar
   (lambda (candidate)
     (if (not (string-match "^\\(.+?\\):\\([0-9]+\\):\\(.*\\)$" candidate))
         (error "Filename and line number not found")
       (let ((filename (match-string 1 candidate))
             (lineno (match-string 2 candidate))
             (content (match-string 3 candidate)))
         (cons (format "%s:%s\n %s"
                       (propertize filename 'face compilation-info-face)
                       (propertize lineno 'face compilation-line-face)
                       content)
               (list (expand-file-name
                      filename
                      (anything-aif (anything-attr 'default-directory)
                          (if (functionp it) (funcall it) it)
                        (and (anything-candidate-buffer)
                             (buffer-local-value
                              'default-directory
                              (anything-candidate-buffer)))))
                     (string-to-number lineno) content)))))
   candidates))

(defun* anything-goto-file-line (file lineno content &optional (find-file-function #'find-file))
  (anything-aif (anything-attr 'before-jump-hook)
      (funcall it))
  (when file (funcall find-file-function file))
  (if (anything-attr-defined 'adjust)
      (anything-c-goto-line-with-adjustment lineno content)
    (anything-goto-line lineno))
  (unless (anything-attr-defined 'recenter)
    (set-window-start (get-buffer-window anything-current-buffer) (point)))
  (anything-aif (anything-attr 'after-jump-hook)
      (funcall it))
  (when anything-in-persistent-action
    (anything-match-line-color-current-line)))

(defun anything-find-file-as-root (candidate)
  (find-file (concat "/" anything-su-or-sudo "::" (expand-file-name candidate))))

(defun anything-find-many-files (ignore)
  (mapc 'find-file (anything-marked-candidates)))

;; borrowed from etags.el
;; (anything-c-goto-line-with-adjustment (line-number-at-pos) ";; borrowed from etags.el")
(defun anything-c-goto-line-with-adjustment (line line-content)
  (let ((startpos)
        offset found pat)
    ;; This constant is 1/2 the initial search window.
    ;; There is no sense in making it too small,
    ;; since just going around the loop once probably
    ;; costs about as much as searching 2000 chars.
    (setq offset 1000
          found nil
          pat (concat (if (eq selective-display t)
                          "\\(^\\|\^m\\) *" "^ *") ;allow indent
                      (regexp-quote line-content)))
    ;; If no char pos was given, try the given line number.
    (setq startpos (progn (anything-goto-line line) (point)))
    (or startpos (setq startpos (point-min)))
    ;; First see if the tag is right at the specified location.
    (goto-char startpos)
    (setq found (looking-at pat))
    (while (and (not found)
                (progn
                  (goto-char (- startpos offset))
                  (not (bobp))))
      (setq found
            (re-search-forward pat (+ startpos offset) t)
            offset (* 3 offset)))       ; expand search window
    (or found
        (re-search-forward pat nil t)
        (error "not found")))
  ;; Position point at the right place
  ;; if the search string matched an extra Ctrl-m at the beginning.
  (and (eq selective-display t)
       (looking-at "\^m")
       (forward-char 1))
  (beginning-of-line))

(anything-document-attribute 'default-directory "type . file-line"
  "`default-directory' to interpret file.")
(anything-document-attribute 'before-jump-hook "type . file-line / line"
  "Function to call before jumping to the target location.")
(anything-document-attribute 'after-jump-hook "type . file-line / line"
  "Function to call after jumping to the target location.")
(anything-document-attribute 'adjust "type . file-line"
  "Search around line matching line contents.")
(anything-document-attribute 'recenter "type . file-line / line"
  "`recenter' after jumping.")
(anything-document-attribute 'target-file "type . line"
  "Goto line of target-file.")

;;;###autoload
(defun anything-c-call-interactively (cmd-or-name)
  "Execute CMD-OR-NAME as Emacs command.
It is added to `extended-command-history'.
`anything-current-prefix-arg' is used as the command's prefix argument."
  (setq extended-command-history
        (cons (anything-c-stringify cmd-or-name)
              (delete (anything-c-stringify cmd-or-name) extended-command-history)))
  (let ((current-prefix-arg anything-current-prefix-arg)
        (cmd (anything-c-symbolify cmd-or-name)))
    (if (stringp (symbol-function cmd))
        (execute-kbd-macro (symbol-function cmd))
      (call-interactively cmd))))

;;;###autoload
(defun anything-c-set-variable (var)
  "Set value to VAR interactively."
  (interactive)
  (let ((sym (anything-c-symbolify var)))
    (set sym (eval-minibuffer (format "Set %s: " var)
                              (prin1-to-string (symbol-value sym))))))
;; (setq hh 12)
;; (anything-c-set-variable 'hh)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Persistent Action Helpers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-match-line-overlay-face nil)
(defvar anything-match-line-overlay nil)

(defun anything-match-line-color-current-line (&optional start end buf face rec)
  "Highlight and underline current position"
  (let ((args (list (or start (line-beginning-position))
                    (or end (1+ (line-end-position)))
                    buf)))
    (if (not anything-match-line-overlay)
        (setq anything-match-line-overlay (apply 'make-overlay args))
      (apply 'move-overlay anything-match-line-overlay args)))
  (overlay-put anything-match-line-overlay
               'face (or face anything-match-line-overlay-face))
  (when rec
    (goto-char start)
    (recenter)))

(defalias 'anything-persistent-highlight-point 'anything-match-line-color-current-line)

(defface anything-overlay-line-face '((t (:background "IndianRed4" :underline t)))
  "Face for source header in the anything buffer." :group 'anything)

(setq anything-match-line-overlay-face 'anything-overlay-line-face)

(defun anything-match-line-cleanup ()
  (when anything-match-line-overlay
    (delete-overlay anything-match-line-overlay)
    (setq anything-match-line-overlay nil)))

(defun anything-match-line-update ()
  (when anything-match-line-overlay
    (delete-overlay anything-match-line-overlay)
    (anything-match-line-color-current-line)))

(add-hook 'anything-cleanup-hook 'anything-match-line-cleanup)
(add-hook 'anything-after-persistent-action-hook 'anything-match-line-update)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Actions Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Files
(defun anything-c-transform-file-load-el (actions candidate)
  "Add action to load the file CANDIDATE if it is an emacs lisp
file.  Else return ACTIONS unmodified."
  (if (member (file-name-extension candidate) '("el" "elc"))
      (append actions '(("Load Emacs Lisp File" . load-file)))
    actions))

(defun anything-c-transform-file-browse-url (actions candidate)
  "Add an action to browse the file CANDIDATE if it in a html
file or URL.  Else return ACTIONS unmodified."
  (let ((browse-action '("Browse with Browser" . browse-url)))
    (cond ((string-match "^http\\|^ftp" candidate)
           (cons browse-action actions))
          ((string-match "\\.html?$" candidate)
           (append actions (list browse-action)))
          (t actions))))

;;;; Function
(defun anything-c-transform-function-call-interactively (actions candidate)
  "Add an action to call the function CANDIDATE interactively if
it is a command.  Else return ACTIONS unmodified."
  (if (commandp (intern-soft candidate))
      (append actions '(("Call Interactively"
                         .
                         anything-c-call-interactively)))
    actions))

;;;; S-Expressions
(defun anything-c-transform-sexp-eval-command-sexp (actions candidate)
  "If CANDIDATE's `car' is a command, then add an action to
evaluate it and put it onto the `command-history'."
  (if (commandp (car (read candidate)))
      ;; Make it first entry
      (cons '("Eval and put onto command-history" .
              (lambda (sexp)
                (let ((sym (read sexp)))
                  (eval sym)
                  (setq command-history
                        (cons sym command-history)))))
            actions)
    actions))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Candidate Transformers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Buffers
(defun anything-c-skip-boring-buffers (buffers)
  (anything-c-skip-entries buffers anything-c-boring-buffer-regexp))

(defun anything-c-skip-current-buffer (buffers)
  (if anything-allow-skipping-current-buffer
      (remove (buffer-name anything-current-buffer) buffers)
      buffers))

(defun anything-c-shadow-boring-buffers (buffers)
  "Buffers matching `anything-c-boring-buffer-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries buffers anything-c-boring-buffer-regexp))

;;; Files
(defun anything-c-shadow-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be
displayed with the `file-name-shadow' face if available."
  (anything-c-shadow-entries files anything-c-boring-file-regexp))

(defun anything-c-skip-boring-files (files)
  "Files matching `anything-c-boring-file-regexp' will be skipped."
  (anything-c-skip-entries files anything-c-boring-file-regexp))
;; (anything-c-skip-boring-files '("README" "/src/.svn/hoge"))

(defun anything-c-skip-current-file (files)
  "Current file will be skipped."
  (remove (buffer-file-name anything-current-buffer) files))

(defun anything-c-w32-pathname-transformer (args)
  "Change undesirable features of windows pathnames to ones more acceptable to
other candidate transformers."
  (if (eq system-type 'windows-nt)
      (mapcar (lambda (x)
                (replace-regexp-in-string "/cygdrive/\\(.\\)" "\\1:" x))
              (mapcar (lambda (y)
                        (replace-regexp-in-string "\\\\" "/" y)) args))
    args))

(defun anything-c-shorten-home-path (files)
  "Replaces /home/user with ~."
  (let ((home (replace-regexp-in-string "\\\\" "/" ; stupid Windows...
                                        (getenv "HOME"))))
    (mapcar (lambda (file)
              (if (and (stringp file) (string-match home file))
                  (cons (replace-match "~" nil nil file) file)
                file))
            files)))

;;; Functions
(defun anything-c-mark-interactive-functions (functions)
  "Mark interactive functions (commands) with (i) after the function name."
  (let (list)
    (loop for function in functions
          do (push (cons (concat function
                                 (when (commandp (intern-soft function)) " (i)"))
                         function)
                   list)
          finally (return (nreverse list)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Adaptive Sorting of Candidates ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar anything-c-adaptive-done nil
  "nil if history information is not yet stored for the current
selection.")

(defvar anything-c-adaptive-history nil
  "Contains the stored history information.
Format: ((SOURCE-NAME (SELECTED-CANDIDATE (PATTERN . NUMBER-OF-USE) ...) ...) ...)")

(defadvice anything-initialize (before anything-c-adaptive-initialize activate)
  "Advise `anything-initialize' to reset `anything-c-adaptive-done'
when anything is started."
  (setq anything-c-adaptive-done nil))

(defadvice anything-exit-minibuffer (before anything-c-adaptive-exit-minibuffer activate)
  "Advise `anything-exit-minibuffer' to store history information
when a candidate is selected with RET."
  (anything-c-adaptive-store-selection))

(defadvice anything-select-action (before anything-c-adaptive-select-action activate)
  "Advise `anything-select-action' to store history information
when the user goes to the action list with TAB."
  (anything-c-adaptive-store-selection))

(defun anything-c-source-use-adaptative-p (&optional source-name)
  "Return current source only if it use adaptative history, nil otherwise."
  (let* ((source (or source-name (anything-get-current-source)))
         (adapt-source (or (assoc-default 'filtered-candidate-transformer
                                          (assoc (assoc-default 'type source)
                                                 anything-type-attributes))
                           (assoc-default 'candidate-transformer
                                          (assoc (assoc-default 'type source)
                                                 anything-type-attributes))
                           (assoc-default 'filtered-candidate-transformer source)
                           (assoc-default 'candidate-transformer source))))
    (if (listp adapt-source)
        (when (member 'anything-c-adaptive-sort adapt-source) source)
        (when (eq adapt-source 'anything-c-adaptive-sort) source))))

(defun anything-c-adaptive-store-selection ()
  "Store history information for the selected candidate."
  (unless anything-c-adaptive-done
    (setq anything-c-adaptive-done t)
    (let ((source (anything-c-source-use-adaptative-p)))
      (when source
        (let* ((source-name (or (assoc-default 'type source)
                                (assoc-default 'name source)))
               (source-info (or (assoc source-name anything-c-adaptive-history)
                                (progn
                                  (push (list source-name) anything-c-adaptive-history)
                                  (car anything-c-adaptive-history))))
               (selection (anything-get-selection))
               (selection-info (progn
                                 (setcdr source-info
                                         (cons
                                          (let ((found (assoc selection (cdr source-info))))
                                            (if (not found)
                                                ;; new entry
                                                (list selection)

                                                ;; move entry to the beginning of the
                                                ;; list, so that it doesn't get
                                                ;; trimmed when the history is
                                                ;; truncated
                                                (setcdr source-info
                                                        (delete found (cdr source-info)))
                                                found))
                                          (cdr source-info)))
                                 (cadr source-info)))
               (pattern-info (progn
                               (setcdr selection-info
                                       (cons
                                        (let ((found (assoc anything-pattern (cdr selection-info))))
                                          (if (not found)
                                              ;; new entry
                                              (cons anything-pattern 0)

                                              ;; move entry to the beginning of the
                                              ;; list, so if two patterns used the
                                              ;; same number of times then the one
                                              ;; used last appears first in the list
                                              (setcdr selection-info
                                                      (delete found (cdr selection-info)))
                                              found))
                                        (cdr selection-info)))
                               (cadr selection-info))))

          ;; increase usage count
          (setcdr pattern-info (1+ (cdr pattern-info)))

          ;; truncate history if needed
          (if (> (length (cdr selection-info)) anything-c-adaptive-history-length)
              (setcdr selection-info
                      (subseq (cdr selection-info) 0 anything-c-adaptive-history-length))))))))

(if (file-readable-p anything-c-adaptive-history-file)
    (load-file anything-c-adaptive-history-file))
(add-hook 'kill-emacs-hook 'anything-c-adaptive-save-history)

(defun anything-c-adaptive-save-history ()
  "Save history information to file given by `anything-c-adaptive-history-file'."
  (interactive)
  (with-temp-buffer
    (insert
     ";; -*- mode: emacs-lisp -*-\n"
     ";; History entries used for anything adaptive display.\n")
    (prin1 `(setq anything-c-adaptive-history ',anything-c-adaptive-history)
           (current-buffer))
    (insert ?\n)
    (write-region (point-min) (point-max) anything-c-adaptive-history-file nil
                  (unless (interactive-p) 'quiet))))

(defun anything-c-adaptive-sort (candidates source)
  "Sort the CANDIDATES for SOURCE by usage frequency.
This is a filtered candidate transformer you can use for the
attribute `filtered-candidate-transformer' of a source in
`anything-sources' or a type in `anything-type-attributes'."
  (let* ((source-name (or (assoc-default 'type source)
                          (assoc-default 'name source)))
         (source-info (assoc source-name anything-c-adaptive-history)))
    (if (not source-info)
        ;; if there is no information stored for this source then do nothing
        candidates
      ;; else...
      (let ((usage
             ;; ... assemble a list containing the (CANIDATE . USAGE-COUNT)
             ;; pairs
             (mapcar (lambda (candidate-info)
                       (let ((count 0))
                         (dolist (pattern-info (cdr candidate-info))
                           (if (not (equal (car pattern-info)
                                           anything-pattern))
                               (incf count (cdr pattern-info))

                             ;; if current pattern is equal to the previously
                             ;; used one then this candidate has priority
                             ;; (that's why its count is boosted by 10000) and
                             ;; it only has to compete with other candidates
                             ;; which were also selected with the same pattern
                             (setq count (+ 10000 (cdr pattern-info)))
                             (return)))
                         (cons (car candidate-info) count)))
                     (cdr source-info)))
            sorted)

        ;; sort the list in descending order, so candidates with highest
        ;; priorty come first
        (setq usage (sort usage (lambda (first second)
                                  (> (cdr first) (cdr second)))))

        ;; put those candidates first which have the highest usage count
        (dolist (info usage)
          (when (member* (car info) candidates
                         :test 'anything-c-adaptive-compare)
            (push (car info) sorted)
            (setq candidates (remove* (car info) candidates
                                      :test 'anything-c-adaptive-compare))))

        ;; and append the rest
        (append (reverse sorted) candidates nil)))))

(defun anything-c-adaptive-compare (x y)
  "Compare candidates X and Y taking into account that the
candidate can be in (DISPLAY . REAL) format."
  (equal (if (listp x)
             (cdr x)
           x)
         (if (listp y)
             (cdr y)
           y)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Plug-in ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Plug-in: info-index
(defun* anything-c-info-init (&optional (file (anything-attr 'info-file)))
  (let (result)
    (unless (anything-candidate-buffer)
      (save-window-excursion
        (info file)
        (let (Info-history
              (tobuf (anything-candidate-buffer 'global))
              (infobuf (current-buffer))
              s e)
          (dolist (node (or (anything-attr 'index-nodes) (Info-index-nodes)))
            (Info-goto-node node)
            (goto-char (point-min))
            (while (search-forward "\n* " nil t)
              (unless (search-forward "Menu:\n" (1+ (point-at-eol)) t)
                '(save-current-buffer (buffer-substring-no-properties (point-at-bol) (point-at-eol)) result)
                (setq s (point-at-bol)
                      e (point-at-eol))
                (with-current-buffer tobuf
                  (insert-buffer-substring infobuf s e)
                  (insert "\n"))))))))))

(defun anything-c-info-goto (node-line)
  (Info-goto-node (car node-line))
  (anything-goto-line (cdr node-line)))

(defun anything-c-info-display-to-real (line)
  (and (string-match
        "\\* +\\([^\n]*.+[^\n]*\\):[ \t]+\\([^\n]*\\)\\.\\(?:[ \t\n]*(line +\\([0-9]+\\))\\)?" line)
       (cons (format "(%s)%s" (anything-attr 'info-file) (match-string 2 line))
             (string-to-number (or (match-string 3 line) "1")))))

(defun anything-c-make-info-source (file)
  `((name . ,(concat "Info Index: " file))
    (info-file . ,file)
    (init . anything-c-info-init)
    (display-to-real . anything-c-info-display-to-real)
    (get-line . buffer-substring)
    (candidates-in-buffer)
    (action ("Goto node" . anything-c-info-goto))))

(defun anything-compile-source--info-index (source)
  (anything-aif (assoc-default 'info-index source)
      (anything-c-make-info-source it)
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--info-index)

(anything-document-attribute 'info-index "info-index plugin"
  "Create a source of info index very easily.

ex. (defvar anything-c-source-info-wget '((info-index . \"wget\"))")

(anything-document-attribute 'index-nodes "info-index plugin (optional)"
  "Index nodes of info file.

If it is omitted, `Info-index-nodes' is used to collect index nodes.
Some info files are missing index specification.

ex. See `anything-c-source-info-screen'.")

;; Plug-in: candidates-file
(defun anything-compile-source--candidates-file (source)
  (if (assoc-default 'candidates-file source)
      `((init anything-p-candidats-file-init
              ,@(let ((orig-init (assoc-default 'init source)))
                  (cond ((null orig-init) nil)
                        ((functionp orig-init) (list orig-init))
                        (t orig-init))))
        (candidates-in-buffer)
        ,@source)
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--candidates-file)

(defun anything-p-candidats-file-init ()
  (destructuring-bind (file &optional updating)
      (anything-mklist (anything-attr 'candidates-file))
    (setq file (anything-interpret-value file))
    (with-current-buffer (anything-candidate-buffer (find-file-noselect file))
      (when updating
        (buffer-disable-undo)
        (font-lock-mode -1)
        (auto-revert-mode 1)))))

(anything-document-attribute 'candidates-file "candidates-file plugin"
  "Use a file as the candidates buffer.

1st argument is a filename, string or function name or variable name.
If optional 2nd argument is non-nil, the file opened with `auto-revert-mode'.")

;; Plug-in: headline
(defun anything-compile-source--anything-headline (source)
  (if (assoc-default 'headline source)
      (append '((init . anything-headline-init)
                (get-line-fn . buffer-substring)
                (type . line))
              source
              '((candidates-in-buffer)
                (persistent-help . "Show this line")))
    source))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--anything-headline)

(defun anything-headline-init ()
  (when (and (anything-current-buffer-is-modified)
             (with-current-buffer anything-current-buffer
               (eval (or (anything-attr 'condition) t))))
    (anything-headline-make-candidate-buffer
     (anything-attr 'headline)
     (anything-attr 'subexp))))

(anything-document-attribute 'headline "Headline plug-in"
  "Regexp string for anything-headline to scan.")
(anything-document-attribute 'condition "Headline plug-in"
  "A sexp representing the condition to use anything-headline.")
(anything-document-attribute 'subexp "Headline plug-in"
  "Display (match-string-no-properties subexp).")


(defun anything-headline-get-candidates (regexp subexp)
  (with-current-buffer anything-current-buffer
    (save-excursion
      (goto-char (point-min))
      (if (functionp regexp) (setq regexp (funcall regexp)))
      (let (hierarchy curhead)
        (flet ((matched ()
                 (if (numberp subexp)
                     (cons (match-string-no-properties subexp) (match-beginning subexp))
                     (cons (buffer-substring (point-at-bol) (point-at-eol))
                           (point-at-bol))))
               (hierarchies (headlines)
                 (1+ (loop for (_ . hierarchy) in headlines
                        maximize hierarchy)))
               (vector-0-n (v n)
                 (loop for i from 0 to hierarchy
                    collecting (aref curhead i)))
               (arrange (headlines)
                 (unless (null headlines) ; FIX headlines empty bug!
                   (loop with curhead = (make-vector (hierarchies headlines) "")
                      for ((str . pt) . hierarchy) in headlines
                      do (aset curhead hierarchy str)
                      collecting
                        (cons
                         (mapconcat 'identity (vector-0-n curhead hierarchy) " / ")
                         pt)))))
            (if (listp regexp)
              (arrange
               (sort
                (loop for re in regexp
                   for hierarchy from 0
                   do (goto-char (point-min))
                   appending
                     (loop
                        while (re-search-forward re nil t)
                        collect (cons (matched) hierarchy)))
                (lambda (a b) (> (cdar b) (cdar a)))))
              (loop while (re-search-forward regexp nil t)
                 collect (matched))))))))


(defun anything-headline-make-candidate-buffer (regexp subexp)
  (with-current-buffer (anything-candidate-buffer 'local)
    (loop for (content . pos) in (anything-headline-get-candidates regexp subexp)
          do (insert
              (format "%5d:%s\n"
                      (with-current-buffer anything-current-buffer
                        (line-number-at-pos pos))
                      content)))))

(defun anything-headline-goto-position (pos recenter)
  (goto-char pos)
  (unless recenter
    (set-window-start (get-buffer-window anything-current-buffer) (point))))

(defun anything-revert-buffer (candidate)
  (with-current-buffer candidate
    (when (buffer-modified-p)
      (revert-buffer t t))))

(defun anything-revert-marked-buffers (candidate)
  (dolist (i (anything-marked-candidates))
    (anything-revert-buffer i)))

(defun anything-kill-marked-buffers (candidate)
  (dolist (i (anything-marked-candidates))
    (kill-buffer i)))

;; Plug-in: persistent-help
(defun anything-compile-source--persistent-help (source)
  (append source '((header-line . anything-persistent-help-string))))
(add-to-list 'anything-compile-source-functions 'anything-compile-source--persistent-help)

(defun anything-persistent-help-string ()
  (substitute-command-keys
   (concat "\\<anything-map>\\[anything-execute-persistent-action]: "
           (or (anything-attr 'persistent-help)
               (anything-aif (or (assoc-default 'persistent-action (anything-get-current-source))
                                 (assoc-default 'action (anything-get-current-source))
                                 )
                   (cond ((symbolp it) (symbol-name it))
                         ((listp it) (or (ignore-errors (caar it))  ""))))
               "")
           " (keeping session)")))

(anything-document-attribute 'persistent-help "persistent-help plug-in"
  "A string to explain persistent-action of this source.
It also accepts a function or a variable name.")

;;; (anything '(((name . "persistent-help test")(candidates "a")(persistent-help . "TEST"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun anything-delete-marked-files (candidate)
  (anything-aif (anything-marked-candidates)
      (if (y-or-n-p (format "Delete *%s Files " (length it)))
          (progn
            (dolist (i it)
              (set-text-properties 0 (length i) nil i)
              (anything-c-delete-file i))
            (message "%s Files deleted" (length it)))
          (message "(No deletions performed)"))
    (set-text-properties 0 (length candidate) nil candidate)
    (if (y-or-n-p (format "Really delete file `%s' " candidate))
        (progn
          (anything-c-delete-file candidate)
          (message "1 file deleted"))
        (message "(No deletions performed)"))))

(defun anything-ediff-marked-buffers (candidate &optional merge)
  "Ediff 2 marked buffers or 1 marked buffer and current-buffer.
With optional arg `merge' call `ediff-merge-buffers'."
  (let ((lg-lst (length (anything-marked-candidates)))
        buf1 buf2)
    (case lg-lst
      (0
       (error "Error:You have to mark at least 1 buffer"))
      (1
       (setq buf1 anything-current-buffer
             buf2 (first (anything-marked-candidates))))
      (2 
       (setq buf1 (first (anything-marked-candidates))
             buf2 (second (anything-marked-candidates))))
      (t
       (error "Error:To much buffers marked!")))
    (if merge
        (ediff-merge-buffers buf1 buf2)
        (ediff-buffers buf1 buf2))))

(defun anything-bookmark-get-bookmark-from-name (bmk)
  "Return bookmark name even if it is a bookmark with annotation.
e.g prepended with *.
Return nil if bmk is not a valid bookmark."
  (let ((bookmark (replace-regexp-in-string "\*" "" bmk)))
    (if (assoc bookmark bookmark-alist)
        bookmark
        (when (assoc bmk bookmark-alist)
          bmk))))

(defun anything-delete-marked-bookmarks (elm)
  "Delete this bookmark or all marked bookmarks."
  (let ((bookmark (anything-bookmark-get-bookmark-from-name elm)))
    (anything-aif (anything-marked-candidates)
        (dolist (i it)
          (let ((bmk (anything-bookmark-get-bookmark-from-name i)))
            (bookmark-delete bmk 'batch)))
      (bookmark-delete bookmark 'batch))))

(defun anything-require-or-error (feature function)
  (or (require feature nil t)
      (error "Need %s to use `%s'." feature function)))

(defun anything-find-buffer-on-elscreen (candidate)
  "Open buffer in new screen, if marked buffers open all in elscreens."
  (anything-require-or-error 'elscreen 'anything-find-buffer-on-elscreen)
  (anything-aif (anything-marked-candidates)
      (dolist (i it)
        (let ((target-screen (elscreen-find-screen-by-buffer
                              (get-buffer i) 'create)))
          (elscreen-goto target-screen)))
    (let ((target-screen (elscreen-find-screen-by-buffer
                          (get-buffer candidate) 'create)))
      (elscreen-goto target-screen))))

(defun anything-elscreen-find-file (file)
  (anything-require-or-error 'elscreen 'anything-elscreen-find-file)
  (elscreen-find-file file))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Type Attributes
(define-anything-type-attribute 'buffer
  `((action
     ,@(if pop-up-frames
           '(("Switch to buffer other window" . switch-to-buffer-other-window)
             ("Switch to buffer" . switch-to-buffer))
         '(("Switch to buffer" . switch-to-buffer)
           ("Switch to buffer other window" . switch-to-buffer-other-window)
           ("Switch to buffer other frame" . switch-to-buffer-other-frame)))
     ,(and (locate-library "elscreen") '("Display buffer in Elscreen" . anything-find-buffer-on-elscreen))
     ("Display buffer"   . display-buffer)
     ("Revert buffer" . anything-revert-buffer)
     ("Revert Marked buffers" . anything-revert-marked-buffers)
     ("Kill buffer" . kill-buffer)
     ("Kill Marked buffers" . anything-kill-marked-buffers)
     ("Ediff Marked buffers" . anything-ediff-marked-buffers)
     ("Ediff Merge marked buffers" . (lambda (candidate)
                                       (anything-ediff-marked-buffers candidate t))))
    (persistent-help . "Show this buffer")
    (candidate-transformer anything-c-skip-current-buffer anything-c-skip-boring-buffers))
  "Buffer or buffer name.")

(define-anything-type-attribute 'file
  `((action
     ,@(if pop-up-frames
           '(("Find file other window" . find-file-other-window)
             ("Find file(s)" . anything-find-many-files)
             ("Find file as root" . anything-find-file-as-root))
         '(("Find file" . anything-find-many-files)
           ("Find file as root" . anything-find-file-as-root)
           ("Find file other window" . find-file-other-window)
           ("Find file other frame" . find-file-other-frame)))
     ("Open dired in file's directory" . anything-c-open-dired)
     ("Delete file(s)" . anything-delete-marked-files)
     ("Open file externally" . anything-c-open-file-externally)
     ("Open file with default tool" . anything-c-open-file-with-default-tool))
    (persistent-help . "Show this file")
    (action-transformer anything-c-transform-file-load-el
                        anything-c-transform-file-browse-url)
    (candidate-transformer anything-c-w32-pathname-transformer
                           anything-c-skip-current-file
                           anything-c-skip-boring-files
                           anything-c-shorten-home-path))
  "File name.")

(define-anything-type-attribute 'command
  `((action ("Call interactively" . anything-c-call-interactively)
            ("Describe command" . anything-c-describe-function)
            ("Add command to kill ring" . anything-c-kill-new)
            ("Go to command's definition" . anything-c-find-function))
    ;; Sort commands according to their usage count.
    (filtered-candidate-transformer . anything-c-adaptive-sort))
  "Command. (string or symbol)")

(define-anything-type-attribute 'function
  '((action ("Describe function" . anything-c-describe-function)
            ("Add function to kill ring" . anything-c-kill-new)
            ("Go to function's definition" . anything-c-find-function))
    (action-transformer anything-c-transform-function-call-interactively)
    (candidate-transformer anything-c-mark-interactive-functions))
  "Function. (string or symbol)")

(define-anything-type-attribute 'variable
  '((action ("Describe variable" . anything-c-describe-variable)
            ("Add variable to kill ring" . anything-c-kill-new)
            ("Go to variable's definition" . anything-c-find-variable)
            ("Set variable" . anything-c-set-variable)))
  "Variable.")

(define-anything-type-attribute 'sexp
  '((action ("Eval s-expression" . (lambda (c) (eval (read c))))
            ("Add s-expression to kill ring" . kill-new))
    (action-transformer anything-c-transform-sexp-eval-command-sexp))
  "String representing S-Expressions.")

(define-anything-type-attribute 'bookmark
  `((action
     ("Jump to bookmark" . (lambda (candidate)
                             (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate))
                                   (current-prefix-arg anything-current-prefix-arg))
                               (bookmark-jump bookmark))
                             (anything-update)))
     ("Jump to BM other window" . (lambda (candidate)
                                    (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                      (bookmark-jump-other-window bookmark))
                                    (anything-update)))
     ("Bookmark edit annotation" . (lambda (candidate)
                                     (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                       (bookmark-edit-annotation bookmark))))
     ("Bookmark show annotation" . (lambda (candidate)
                                     (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                       (bookmark-show-annotation bookmark))))
     ("Delete bookmark(s)" . anything-delete-marked-bookmarks)
     ,@(when (fboundp 'bmkext-edit-bookmark)
             '(("Edit Bookmark" . (lambda (candidate)
                                    (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                            (bmkext-edit-bookmark bookmark))))))
     ("Rename bookmark" . (lambda (candidate)
                            (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                              (bookmark-rename bookmark))))
     ("Relocate bookmark" . (lambda (candidate)
                              (let ((bookmark (anything-bookmark-get-bookmark-from-name candidate)))
                                (bookmark-relocate bookmark))))))
     "Bookmark name.")

(define-anything-type-attribute 'line
  '((display-to-real . anything-c-display-to-real-line)
    (action ("Go to Line" . anything-c-action-line-goto)))
  "LINENO:CONTENT string, eg. \"  16:foo\".

Optional `target-file' attribute is a name of target file.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.
")

(define-anything-type-attribute 'file-line
  `((filtered-candidate-transformer anything-c-filtered-candidate-transformer-file-line)
    (multiline)
    (action ("Go to" . anything-c-action-file-line-goto)))
  "FILENAME:LINENO:CONTENT string, eg. \"~/.emacs:16:;; comment\".

Optional `default-directory' attribute is a default-directory
FILENAME is interpreted.

Optional `before-jump-hook' attribute is a function with no
arguments which is called before jumping to position.

Optional `after-jump-hook' attribute is a function with no
arguments which is called after jumping to position.

If `adjust' attribute is specified, searches the line whose
content is CONTENT near the LINENO.

If `recenter' attribute is specified, the line is displayed at
the center of window, otherwise at the top of window.
")

(define-anything-type-attribute 'timer
  '((real-to-display . anything-c-timer-real-to-display)
    (action ("Cancel Timer" . cancel-timer)))
  "Timer.")

;;;; unit test
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-expectations.el")
;; (install-elisp "http://www.emacswiki.org/cgi-bin/wiki/download/el-mock.el")
(dont-compile
  (when (fboundp 'expectations)
    (expectations
     (desc "candidates-file plug-in")
     (expect '(anything-p-candidats-file-init)
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")))
                                  '(anything-compile-source--candidates-file)))))
     (expect '(anything-p-candidats-file-init
               (lambda () 1))
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")
                                     (init . (lambda () 1))))
                                  '(anything-compile-source--candidates-file)))))
     (expect '(anything-p-candidats-file-init
               (lambda () 1))
             (assoc-default 'init
                            (car (anything-compile-sources
                                  '(((name . "test")
                                     (candidates-file . "test.txt")
                                     (init (lambda () 1))))
                                  '(anything-compile-source--candidates-file)))))
     (desc "anything-c-source-buffers")
     (expect '(("Buffers" ("foo" "curbuf")))
             (stub buffer-list => '("curbuf" " hidden" "foo" "*anything*"))
             (let ((anything-c-boring-buffer-regexp
                    (rx (or
                         (group bos  " ")
                         "*anything"
                         ;; echo area
                         " *Echo Area" " *Minibuf"))))
               (flet ((buffer-name (x) x))
                 (anything-test-candidates 'anything-c-source-buffers))))
     (desc "anything-c-stringify")
     (expect "str1"
             (anything-c-stringify "str1"))
     (expect "str2"
             (anything-c-stringify 'str2))
     (desc "anything-c-symbolify")
     (expect 'sym1
             (anything-c-symbolify "sym1"))
     (expect 'sym2
             (anything-c-symbolify 'sym2)))))


(provide 'anything-config)

;;; Local Variables:
;;; time-stamp-format: "%:y-%02m-%02d %02H:%02M:%02S (%Z) %u"
;;; End:

;; How to save (DO NOT REMOVE!!)
;; (progn (magit-push) (emacswiki-post "anything-config.el"))
;;; anything-config.el ends here

;;; LocalWords:  Tassilo Patrovics Vagn Johansen Dahl Clementson infos
;;; LocalWords:  Kamphausen informations McBrayer Volpiatto bbdb bb
;;; LocalWords:  iswitchb imenu Recentf sym samewindow pos bol eol
;;; LocalWords:  aif str lst func attrib recentf lessp prin mapatoms commandp
;;; LocalWords:  cmd stb Picklist picklist mapcan subentry destructuring dirs
;;; LocalWords:  darwin locat MacOS mdfind Firstname Lastname calc prepend jids
;;; LocalWords:  dotimes Thierry online vname
;;; LocalWords:  csharp javascript lua makefile cperl zcat lineno buf
;;; LocalWords:  multiline href fn cand NewTitle cwd filepath thru ret
;;; LocalWords:  bfn fOpen UNC cygdrive nt xdg macos FILE's elc rx svn hg
;;; LocalWords:  CANDIDATE's darcs facep pathname args pathnames subseq priorty
;;; LocalWords:  Vokes rfind berkeley JST ffap lacarte bos
;;; LocalWords:  Lacarte Minibuf epp LaCarte bm attrset migemo attr conf mklist
;;; LocalWords:  startpos noselect dont desc
