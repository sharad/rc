;;; org-clock-unnamed-task-autoloads.el --- automatically extracted autoloads
;;
;;; Code:
(add-to-list 'load-path (or (file-name-directory #$) (car load-path)))

;;;### (autoloads nil "org-clock-unnamed-task" "org-clock-unnamed-task.el"
;;;;;;  (23243 787 361267 903000))
;;; Generated autoloads from org-clock-unnamed-task.el

(autoload 'lotus-org-unnamed-task-file "org-clock-unnamed-task" "\


\(fn &optional FILE)" nil nil)

(autoload 'lotus-org-unnamed-parent-task-name "org-clock-unnamed-task" "\


\(fn &optional NAME)" nil nil)

(autoload 'lotus-org-unnamed-task-name-fmt "org-clock-unnamed-task" "\


\(fn &optional FMT)" nil nil)

(autoload 'lotus-org-unnamed-task-clock-marker "org-clock-unnamed-task" "\


\(fn &optional MARKER)" nil nil)

(autoload 'lotus-org-get-incr-tasknum "org-clock-unnamed-task" "\


\(fn &optional BUFFER)" nil nil)

(autoload 'lotus-org-create-unnamed-task "org-clock-unnamed-task" "\
return newly created subtask and marker (suntask . marker)

\(fn &optional FILE TASK)" t nil)

(autoload 'lotus-org-create-unnamed-task-task-clock-in "org-clock-unnamed-task" "\


\(fn &optional FILE PARENT-TASK)" t nil)

(autoload 'lotus-org-merge-unnamed-task-at-point "org-clock-unnamed-task" "\


\(fn)" t nil)

(autoload 'org-clock-marker-is-unnamed-clock-p "org-clock-unnamed-task" "\


\(fn &optional CLOCK)" nil nil)

(autoload 'lotus-org-unnamed-task-at-point-p "org-clock-unnamed-task" "\


\(fn)" nil nil)

(autoload 'org-clock-make-child-task-and-clock-in "org-clock-unnamed-task" "\
Implement

\(fn)" t nil)

;;;***

;;;### (autoloads nil nil ("org-clock-unnamed-task-pkg.el") (23243
;;;;;;  787 459352 260000))

;;;***

;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; End:
;;; org-clock-unnamed-task-autoloads.el ends here
