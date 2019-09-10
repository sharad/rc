;;; org-capture+-autoloads.el --- automatically extracted autoloads
;;
;;; Code:

(add-to-list 'load-path (directory-file-name
                         (or (file-name-directory #$) (car load-path))))


;;;### (autoloads nil "org-capture+" "org-capture+.el" (0 0 0 0))
;;; Generated autoloads from org-capture+.el

(autoload 'org-capture-plus "org-capture+" "\
Capture something.
\\<org-capture-plus-mode-map>
This will let you select a template from `org-capture-templates', and
then file the newly captured information.  The text is immediately
inserted at the target location, and an indirect buffer is shown where
you can edit it.  Pressing `\\[org-capture-plus-finalize]' brings you back to the previous
state of Emacs, so that you can continue your work.

When called interactively with a `\\[universal-argument]' prefix argument GOTO, don't
capture anything, just go to the file/headline where the selected
template stores its notes.

With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to the last note stored.

When called with a `C-0' (zero) prefix, insert a template at point.

When called with a `C-1' (one) prefix, force prompting for a date when
a datetree entry is made.

ELisp programs can set KEYS to a string associated with a template
in `org-capture-templates'.  In this case, interactive selection
will be bypassed.

If `org-capture-use-agenda-date' is non-nil, capturing from the
agenda will use the date at point as the default date.  Then, a
`C-1' prefix will tell the capture process to use the HH:MM time
of the day at point (if any) or the current HH:MM time.

\(fn TYPE TARGET TEMPLATE &rest PLIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture+" '("org-")))

;;;***

;;;### (autoloads nil "org-capture+-eng" "org-capture+-eng.el" (0
;;;;;;  0 0 0))
;;; Generated autoloads from org-capture+-eng.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture+-eng" '("org-capture+")))

;;;***

;;;### (autoloads nil "org-capture+-helm" "org-capture+-helm.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-capture+-helm.el

(autoload 'org-capture+-build-helm-template-source "org-capture+-helm" "\


\(fn NAME ATTRIB-LIST &rest TEMPLATES)" nil nil)

(autoload 'org-capture+-build-helm-template-sources "org-capture+-helm" "\


\(fn ATTRIB-LIST ALIST)" nil nil)

(autoload 'org-capture+-helm-select-template "org-capture+-helm" "\


\(fn &optional ATTRIB-LIST ALIST)" nil nil)

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture+-helm" '("org-capture+-helm-template")))

;;;***

;;;### (autoloads nil "org-capture+-helm-dynamic" "org-capture+-helm-dynamic.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-capture+-helm-dynamic.el

(defvar org-capture+-helm-templates-plist nil)

(defvar org-capture+-helm-templates-tree '(t))

(autoload 'org-capture+-add-heading-template "org-capture+-helm-dynamic" "\


\(fn KEYS HEADING &rest TEMPLATES)" nil nil)

(org-capture+-add-heading-template '(xx) "TODO" "* TODO %? %^g\n %i\n [%a]\n")

(org-capture+-add-heading-template '(zz) "TODO" "* WRITING %? %^g\n %i\n [%a]\n")

(org-capture+-add-heading-template '(yy) "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture+-helm-dynamic" '("helm-template-gen-s" "org-capture+-")))

;;;***

;;;### (autoloads nil "org-capture+-macros" "org-capture+-macros.el"
;;;;;;  (0 0 0 0))
;;; Generated autoloads from org-capture+-macros.el

(if (fboundp 'register-definition-prefixes) (register-definition-prefixes "org-capture+-macros" '("before-org-capture" "after-org-capture" "with-org-capture")))

;;;***

;;;### (autoloads nil nil ("org-capture+-pkg.el") (0 0 0 0))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; org-capture+-autoloads.el ends here
