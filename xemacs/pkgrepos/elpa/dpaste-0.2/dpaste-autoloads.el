;;; dpaste-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (dpaste-region-or-buffer dpaste-buffer dpaste-region)
;;;;;;  "dpaste" "dpaste.el" (20362 25426))
;;; Generated autoloads from dpaste.el

(autoload 'dpaste-region "dpaste" "\
Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option.

\(fn BEGIN END TITLE &optional ARG)" t nil)

(autoload 'dpaste-buffer "dpaste" "\
Post the current buffer to dpaste.com and yank the url to the
kill-ring.

With a prefix argument, use hold option.

\(fn TITLE &optional ARG)" t nil)

(autoload 'dpaste-region-or-buffer "dpaste" "\
Post the current region or buffer to dpaste.com and yank the
url to the kill-ring.

With a prefix argument, use hold option.

\(fn TITLE &optional ARG)" t nil)

;;;***

;;;### (autoloads nil nil ("dpaste-pkg.el") (20362 25426 788602))

;;;***

(provide 'dpaste-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; dpaste-autoloads.el ends here
