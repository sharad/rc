;;; loaddefs.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (with-report-error with-report-error deh-section
;;;;;;  deh-require-mustbe deh-require-maybe deh-require-todo deh-require-or-package-install
;;;;;;  deh-require-or-act deh-featurep) "macros-config" "macros-config.el"
;;;;;;  (22246 51159 701652 11000))
;;; Generated autoloads from macros-config.el

(autoload 'deh-featurep "macros-config" "\
Dot Emacs featurep

\(fn FEATURE &rest FORMS)" nil t)

(put 'deh-featurep 'lisp-indent-function '1)

(autoload 'deh-require-or-act "macros-config" "\
Dot Emacs require or act

\(fn FEATURE ACT &rest FORMS)" nil t)

(put 'deh-require-or-act 'lisp-indent-function '1)

(autoload 'deh-require-or-package-install "macros-config" "\
Dot Emacs require or package install

\(fn FEATURE &rest FORMS)" nil t)

(put 'deh-require-or-package-install 'lisp-indent-function '2)

(autoload 'deh-require-todo "macros-config" "\
Dot Emacs require TODO

\(fn FEATURE TODO-IF-NO-FEATURE &rest FORMS)" nil t)

(put 'deh-require-todo 'lisp-indent-function '1)

(autoload 'deh-require-maybe "macros-config" "\
Dot Emacs require Maybe

\(fn FEATURE &rest FORMS)" nil t)

(put 'deh-require-maybe 'lisp-indent-function '1)

(autoload 'deh-require-mustbe "macros-config" "\
Dot Emacs require Must

\(fn FEATURE &rest FORMS)" nil t)

(put 'deh-require-mustbe 'lisp-indent-function '1)

(defalias 'deh-require 'deh-require-maybe)

(autoload 'deh-section "macros-config" "\
Dot Emacs Section

\(fn SECTION &rest FORMS)" nil t)

(put 'deh-section 'lisp-indent-function '1)

(autoload 'with-report-error "macros-config" "\
run body and report error in echo area and message buffer

\(fn MSG &rest BODY)" nil t)

(put 'with-report-error 'lisp-indent-function '4)

(autoload 'with-report-error "macros-config" "\
run body and report error in echo area and message buffer

\(fn MSG &rest BODY)" nil t)

(put 'with-report-error 'lisp-indent-function '4)

;;;***

;;;### (autoloads (lotus-read-file) "utils-config" "utils-config.el"
;;;;;;  (21901 1609 737022 585000))
;;; Generated autoloads from utils-config.el

(autoload 'lotus-read-file "utils-config" "\


\(fn FILENAME)" nil nil)

;;;***

;;;### (autoloads nil nil ("annotation-conf.el" "appt-config.el"
;;;;;;  "art-config.el" "auto-load-config.el" "autoinstall-config.el"
;;;;;;  "autosavebackup-config.el" "autotext-config.el" "basic-config.el"
;;;;;;  "basic-utils-config.el" "bbdb-config.el" "binding-config.el"
;;;;;;  "bookmark-config.el" "buffer-config.el" "cache-config.el"
;;;;;;  "cal-org-config.el" "cedet-config.el" "clisp-config.el" "color-config.el"
;;;;;;  "comment-config.el" "controller-config.el" "copy-config.el"
;;;;;;  "cursor-config.el" "db-config.el" "debug-config.el" "dev-config.el"
;;;;;;  "dirs-config.el" "display-config.el" "doc-config.el" "ease-config.el"
;;;;;;  "ecb-layout-def-config.el" "ecbx-config.el" "echoserver-config.el"
;;;;;;  "ediff-config.el" "editing-config.el" "emacs-extra-config.el"
;;;;;;  "encrypt-config.el" "erc-config.el" "etask-config.el" "eudc-config.el"
;;;;;;  "expand-config.el" "exsubr-config.el" "faces-config.el" "files-config.el"
;;;;;;  "finance-config.el" "find-file-config.el" "flymake-config.el"
;;;;;;  "folding-config.el" "frame-config.el" "functionality-config.el"
;;;;;;  "g-client-config.el" "gnus-config.el" "help-config.el" "howm-config.el"
;;;;;;  "incoming-config.el" "info-config.el" "interactivity-config.el"
;;;;;;  "javascript-config.el" "jde-config.el" "journaling-config.el"
;;;;;;  "keymap-config.el" "keyring-config.el" "lock-config.el" "mail-config.el"
;;;;;;  "math-config.el" "minor-modes-act-config.el" "misc-config.el"
;;;;;;  "mode-line-config.el" "mozrepl-config.el" "muse-config.el"
;;;;;;  "mutlimedia-config.el" "mutt-config.el" "myanything-config.el"
;;;;;;  "note-config.el" "objnotation-config.el" "office-config.el"
;;;;;;  "orgmode-config.el" "pa-config.el" "package-config.el" "perl-config.el"
;;;;;;  "plan-config.el" "printing-config.el" "project-config.el"
;;;;;;  "publishing-config.el" "python-config.el" "reader-config.el"
;;;;;;  "recent-config.el" "remem-config.el" "remember-config.el"
;;;;;;  "ruby-config.el" "schedule-config.el" "scm-config.el" "scratch-config.el"
;;;;;;  "screen-config.el" "session-config.el" "shell-config.el"
;;;;;;  "signal-config.el" "slime-config.el" "slime-proxy-config.el"
;;;;;;  "snippet-config.el" "speechd-config.el" "sqlite-config.el"
;;;;;;  "startup-config.el" "stats-config.el" "tag-config.el" "tagging-config.el"
;;;;;;  "test-config.el" "things-config.el" "timeclock-config.el"
;;;;;;  "timer-config.el" "todo-config.el" "tramp-config.el" "unicode-config.el"
;;;;;;  "utf-config.el" "vc-config.el" "w3m-config.el" "web-config.el"
;;;;;;  "wiki-config.el" "window-config.el" "wrappers-config.el"
;;;;;;  "xml-config.el") (22246 51175 580696 327000))

;;;***

(provide 'loaddefs)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; loaddefs.el ends here
