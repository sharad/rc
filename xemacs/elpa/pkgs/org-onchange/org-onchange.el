;; Org insert log note un-interactively


;; [[file:~/.xemacs/elpa/pkgs/org-onchange/org-onchange.org::*Org%20insert%20log%20note%20un-interactively][Org insert log note un-interactively:1]]
;; copy of org-store-log-note
;;;###autoload
  (defun org-insert-log-note (marker txt &optional purpose effective-time state previous-state)
    "Finish taking a log note, and insert it to where it belongs."
    (let* ((note-marker marker)
           (txt txt)
           (note-purpose (or purpose 'note))
           (effective-time (or effective-time (org-current-effective-time)))
           (note-state state)
           (note-previous-state previous-state))
      (if (marker-buffer marker)
          (let ((note (cdr (assq note-purpose org-log-note-headings)))
                lines)
            (while (string-match "\\`# .*\n[ \t\n]*" txt)
              (setq txt (replace-match "" t t txt)))
            (when (string-match "\\s-+\\'" txt)
              (setq txt (replace-match "" t t txt)))
            (setq lines (org-split-string txt "\n"))
            (when (org-string-nw-p note)
              (setq note
                    (org-replace-escapes
                     note
                     (list (cons "%u" (user-login-name))
                           (cons "%U" user-full-name)
                           (cons "%t" (format-time-string
                                       (org-time-stamp-format 'long 'inactive)
                                       effective-time))
                           (cons "%T" (format-time-string
                                       (org-time-stamp-format 'long nil)
                                       effective-time))
                           (cons "%d" (format-time-string
                                       (org-time-stamp-format nil 'inactive)
                                       effective-time))
                           (cons "%D" (format-time-string
                                       (org-time-stamp-format nil nil)
                                       effective-time))
                           (cons "%s" (cond
                                        ((not note-state) "")
                                        ((string-match-p org-ts-regexp note-state)
                                         (format "\"[%s]\""
                                                 (substring note-state 1 -1)))
                                        (t (format "\"%s\"" note-state))))
                           (cons "%S"
                                 (cond
                                   ((not note-previous-state) "")
                                   ((string-match-p org-ts-regexp
                                                    note-previous-state)
                                    (format "\"[%s]\""
                                            (substring
                                             note-previous-state 1 -1)))
                                   (t (format "\"%s\""
                                              note-previous-state)))))))
              (when lines (setq note (concat note " \\\\")))
              (push note lines))

            (when lines ;; (and lines (not (or current-prefix-arg org-note-abort)))
              (with-current-buffer (marker-buffer note-marker)
                (org-with-wide-buffer
                 ;; Find location for the new note.
                 (goto-char note-marker)
                 ;; (set-marker note-marker nil)

                 ;; Note associated to a clock is to be located right after
                 ;; the clock.  Do not move point.
                 (unless (eq note-purpose 'clock-out)
                   (goto-char (org-log-beginning t)))
                 ;; Make sure point is at the beginning of an empty line.
                 (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
                       ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
                 ;; In an existing list, add a new item at the top level.
                 ;; Otherwise, indent line like a regular one.
                 (let ((itemp (org-in-item-p)))
                   (if itemp
                       (indent-line-to
                        (let ((struct (save-excursion
                                        (goto-char itemp) (org-list-struct))))
                          (org-list-get-ind (org-list-get-top-point struct) struct)))
                       (org-indent-line)))
                 (insert (org-list-bullet-string "-") (pop lines))
                 (let ((ind (org-list-item-body-column (line-beginning-position))))
                   (dolist (line lines)
                     (insert "\n")
                     (indent-line-to ind)
                     (insert line)))
                 (message "Note stored")
                 (org-back-to-heading t)
                 (org-cycle-hide-drawers 'children))
                ;; Fix `buffer-undo-list' when `org-store-log-note' is called
                ;; from within `org-add-log-note' because `buffer-undo-list'
                ;; is then modified outside of `org-with-remote-undo'.
                (when (eq this-command 'org-agenda-todo)
                  (setcdr buffer-undo-list (cddr buffer-undo-list))))))
          (error "merker %s buffer is nil" marker))))
;; Org insert log note un-interactively:1 ends here
