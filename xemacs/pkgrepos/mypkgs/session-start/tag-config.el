;; ref: http://www.emacswiki.org/cgi-bin/wiki/TagFile
;; The following will automatically create a TAGS file from within Emacs
;; itself if none exists. Just hit `M-. and youre off.

;; (defadvice find-tag (before c-tag-file () preactivate)
;; (defadvice find-tag (before c-tag-file () preactivate)
;; (defadvice find-tag (before c-tag-file () activate)

;; (defadvice find-tag (before c-tag-file () disable)
(defadvice find-tag (before c-tag-file () disable)
  "Automatically create tags file."
  (let ((tag-file (concat default-directory "TAGS")))
    (unless (file-exists-p tag-file)
      (create-tags default-directory))
    (visit-tags-table tag-file)))


;; ref: http://www.emacswiki.org/cgi-bin/wiki/EmacsTags
;; Completion
;; You can use M-x complete-tag to get simple (ie context
;; free) symbol name Completion. This works like other types of
;; completion in emacs, if there are multiple possibilities a window will
;; be opened showing them all. This used to be bound to M-TAB by default
;; but as many window managers use this to switch between windows, I tend
;; to use M-RET instead.
(global-set-key (kbd "M-<return>") 'complete-tag)

;; ref: http://www.emacswiki.org/cgi-bin/wiki/BuildTags
;; Or to build a tags file for a source tree (e.g. the linux kernel) you can use something like:
;;
;;     find . -type f -iname "*.[ch]" | etags -
;;
;; Avoid using xargs with etags (e.g. find . -type f -iname *.[ch] |
;; xargs etags). For source trees with many files, xargs will execute
;; etags multiple times, overwriting the previous TAGS file on each
;; execution.
;;
;; Or to build the tags file within emacs, put this in your .emacs file:

(defvar *etag-cmd-fmt* "find %s  -path '*.svn*'  -prune -o -type f | etags --output=TAGS - 2>/dev/null")
(defvar *gtag-cmd-fmt* "find %s  -path '*.svn*'  -prune -o -type f | gtags -f - 2>/dev/null")

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (let ((cmd (read-from-minibuffer "tag cmd: " (format *gtag-cmd-fmt* dir-name))))
    (eshell-command cmd)))

;; (defun create-c-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -name \"*.[ch]\" | etags -" dir-name)))

;; (defun create-perl-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (eshell-command
;;    (format "find %s -type f -regex '.*\\.p\\(\\(l\\|m\\)\\|cgi\\)' | etags -l perl -" dir-name)))

;; This package provides a function which rebuilds the TagFile being used
;; by the current buffer. You can pre-configure the shell command based
;; on the tag file being built.


(deh-require-maybe gtags
  ;; http://www.emacswiki.org/emacs/GnuGlobal
  (setq global-supported-pgm-langs
      '(c))

  (defun gtags-root-dir ()
    "Returns GTAGS root directory or nil if doesn't exist."
    (with-temp-buffer
      (if (zerop (call-process "global" nil t nil "-pr"))
          (buffer-substring (point-min) (1- (point-max)))
          nil)))

  (defun gtags-update-synchronously ()
    "Make GTAGS incremental update, synchronously."
    (call-process "global" nil nil nil "-u"))

  (defun gtags-update-asynchronously ()
    "Make GTAGS incremental update, asynchronously."
    (if (eq (process-status "global") 'nil)
        (start-process "global" "global-update" "global" "-u")))

  ;; test (start-process "global" "global-update" "sleep" "10")
  ;; default-directory

    ;; (defun gtags-global-update ()
    ;;   "If current directory is part of a GLOBAL database update it."
    ;;   (interactive)
    ;;   (when (gtags-global-dir)
    ;;     (if (equal (call-process "global" nil nil nil "-vu") 0)
    ;;         (setq gtags-global-complete-list-obsolete-flag t)
    ;;       (error "global database update failed"))))



  (defun gtags-update ()
    (when (gtags-root-dir)
      (gtags-update-asynchronously)))

  (setq gtags-mode-hook
        '(lambda ()
          (setq gtags-path-style 'relative)))

  (add-element-to-lists '(lambda ()
                          (gtags-mode 1)
                          ) global-supported-pgm-langs)

  (add-hook 'after-save-hook #'gtags-update)

  ;; http://www.emacswiki.org/emacs/CyclingGTagsResult
  (defun ww-next-gtag ()
    "Find next matching tag, for GTAGS."
    (interactive)
    (let ((latest-gtags-buffer
           (car (delq nil  (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list)) ))))
      (cond (latest-gtags-buffer
             (switch-to-buffer latest-gtags-buffer)
             (next-line)
             (gtags-select-it nil)))))

  ;; Here’s my key binding for using GNU Global.


;;; http://lists.gnu.org/archive/html/help-gnu-emacs/2005-09/msg00157.html

  (deh-require-maybe gtags

  (autoload 'gtags-mode "gtags" nil t)

  (when (executable-find "global")

    (defadvice gtags-visit-rootdir (after make-complete-list activate)
      "Rebuilds completion list when changing GLOBAL database rootdir."
      (gtags-make-complete-list))

    (defun gtags-global-dir-p (dir)
      "Return non-nil if directory DIR contains a GLOBAL database."
      (and (file-exists-p (expand-file-name "GPATH" dir))
           (file-exists-p (expand-file-name "GRTAGS" dir))
           (file-exists-p (expand-file-name "GSYMS" dir))
           (file-exists-p (expand-file-name "GTAGS" dir))))

    (defun gtags-global-dir (&optional dir)
      "Return the nearest super directory that contains a GLOBAL database."
      (interactive)
      (when (null dir)
        (setq dir default-directory))
      (cond ((gtags-global-dir-p dir) dir)
            ((equal (file-truename dir) (file-truename "/")) nil)
            (t (gtags-global-dir (file-name-as-directory
                                  (expand-file-name ".." dir))))))

    (defvar gtags-global-complete-list-obsolete-flag nil
      "When non-nil, the GLOBAL complete list should be rebuilt.")

    (defun gtags-global-update ()
      "If current directory is part of a GLOBAL database update it."
      (interactive)
      (gtags-update-asynchronously))

      ;; (when (gtags-global-dir)
      ;;   (if (equal (call-process "global" nil nil nil "-vu") 0)
      ;;       (setq gtags-global-complete-list-obsolete-flag t)
      ;;     (error "global database update failed"))))

    (defun gtags-global-complete-list-maybe ()
      "Rebuild the GLOBAL complete list when indicated.
See `gtags-global-complete-list-obsolete-flag'."
      (interactive)
      (when gtags-global-complete-list-obsolete-flag
        (gtags-make-complete-list)
        (setq gtags-global-complete-list-obsolete-flag nil)))

    (add-hook 'gtags-mode-hook
              (lambda ()
                (add-hook 'after-save-hook 'gtags-global-update nil t)
                (defadvice gtags-find-tag
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-rtag
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-symbol
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-pattern
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-with-grep
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-with-idutils
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-file
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-parse-file
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                (defadvice gtags-find-tag-from-here
                  (before gtags-global-complete-list-maybe activate)
                  (gtags-global-complete-list-maybe))
                )                       ; (lambda () ...)
              )                        ; (add-hook 'gtags-mode-hook ...)
    )                            ; (when (executable-find "global") ...)

  ;; Use gtags in all modes for now.
  ;; (gtags-mode 1)
  )                                ; (when (locate-library "gtags") ...)

  )


(provide 'tag-config)
