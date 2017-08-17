;;; org-misc-utils-lotus.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:


(progn ;; "org macro"

  (defmacro org-with-refile (refile-targets &rest body)
    "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
    ;; mark paragraph if no region is set
    `(let* ((org-refile-targets (or ,refile-targets org-refile-targets))
            (target (save-excursion (org-refile-get-location)))
            (file (nth 1 target))
            (pos (nth 3 target)))
       (with-current-buffer (find-file-noselect file)
         (save-excursion
           (goto-char pos)
           ,@body))))
  (put 'org-with-refile 'lisp-indent-function 1)

  (defmacro org-with-file-headline (file headline &rest body)
    `(with-current-buffer (if ,file (find-file-noselect ,file) (current-buffer))
       (save-excursion
         (goto-char (point-min))
         (let ((pos (org-find-exact-headline-in-buffer ,headline)))
           (when pos
             (goto-char pos)
             ,@body)
           pos))))
  (put 'org-with-file-headline 'lisp-indent-function 2)

  (defmacro org-with-clock-writeable-buffer (&rest body)
    `(let ((buff (org-base-buffer (marker-buffer org-clock-marker))))
       (when buff
         (with-current-buffer buff
           (let (buffer-read-only)
             ,@body))))))




(progn ;; "move org"

  (defun jay/refile-to (file headline)
    "Move current headline to specified location"
    (let ((pos (save-excursion
                 (find-file file)
                 (org-find-exact-headline-in-buffer headline))))
      (org-refile nil nil (list headline file nil pos))))

  (defun jay/refile-to-bookmarks ()
    "Move current headline to bookmarks"
    (interactive)
    (org-mark-ring-push)
    (jay/refile-to "~/Org/bookmarks.org" "New")
    (org-mark-ring-goto))

  ;; (save-excursion (org-refile-get-location))

  (setq org-refile-targets
        '((nil :maxlevel . 3)           ; only the current file
          (org-agenda-files :maxlevel . 3) ; all agenda files, 1st/2nd level
          (org-files-list :maxlevel . 4)   ; all agenda and all open files
          (my-org-files-list :maxlevel . 4))) ;all files returned by `my-org-files-list'

  (defun my-org-files-list ()
    (remove nil
     (mapcar (lambda (buffer)
              (buffer-file-name buffer))
            (org-buffer-list 'files t))))

  (defvar org-refile-region-format "\n%s\n")

  (defvar org-refile-region-position 'top
    "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

  (defun my/org-refile-region (beg end copy)
    "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
    (interactive "r\nP")
    ;; mark paragraph if no region is set
    (unless (use-region-p)
      (setq beg (save-excursion
                  (backward-paragraph)
                  (skip-chars-forward "\n\t ")
                  (point))
            end (save-excursion
                  (forward-paragraph)
                  (skip-chars-backward "\n\t ")
                  (point))))
    (org-with-refile nil
                     (let ((text (buffer-substring-no-properties beg end)))
                       (unless copy (kill-region beg end))
                       (deactivate-mark)
                       (with-current-buffer (find-file-noselect file)
                         (save-excursion
                           (goto-char pos)
                           (if (eql org-refile-region-position 'bottom)
                               (org-end-of-subtree)
                               ;; (org-end-of-meta-data-and-drawers)
                               (org-end-of-subtree))
                           (insert (format org-refile-region-format text)))))))

  (defvar org-refile-string-format "%s\n")

  (defvar org-refile-string-position 'top
    "Where to refile a region. Use 'bottom to refile at the
end of the subtree. ")

  (defun org-refile-string (text arg)
    "Refile the active region.
If no region is active, refile the current paragraph.
With prefix arg C-u, copy region instad of killing it."
    (interactive "sorg entry: \nP")
    ;; mark paragraph if no region is set
    (org-with-refile nil
                     ;; (unless arg (kill-region beg end))
                     ;; (deactivate-mark)
                     (with-current-buffer (find-file-noselect file)
                       (let ((buffer-read-only nil))
                         (save-excursion
                           (goto-char pos)
                           (if (eql org-refile-string-position 'bottom)
                               (org-end-of-subtree)
                               ;; (org-end-of-meta-data-and-drawers)
                               ;; (org-end-of-meta-data)
                               (org-end-of-subtree)
                               )
                           (org-insert-subheading nil)
                           (insert (format org-refile-string-format text)))))))

  (defun org-insert-subheading-to-file-headline (text file headline)
    (org-with-file-headline
     file headline
     (let ((buffer-read-only nil))
       (if (eql org-refile-string-position 'bottom)
           (org-end-of-subtree)
           ;; (org-end-of-meta-data-and-drawers)
           ;; (org-end-of-meta-data)
           (org-end-of-subtree)
           )
       (org-insert-subheading nil)
       (insert (format org-refile-string-format text)))))

  (defun org-insert-heading-to-file-headline (text file headline)
    (org-with-file-headline
     file headline
     (let ((buffer-read-only nil))
       (if (eql org-refile-string-position 'bottom)
           (org-end-of-subtree)
           ;; (org-end-of-meta-data-and-drawers)
           ;; (org-end-of-meta-data)
           (org-end-of-subtree)
           )
       (org-insert-heading nil)
       (insert (format org-refile-string-format text))))))


  (progn ;; "property"

    (defun org-refile-entry-put (property value)
      (interactive
       (let ((property (read-from-minibuffer "property: "))
             (value    (read-from-minibuffer "value: ")))
         (list property value)))
      (org-with-refile nil
                       (let ((buffer-read-only nil))
                         (org-entry-put nil property value))))


    (defun org-refile-entry-put-multivalued-property (property &rest values)
      (interactive
       (let ((property (read-from-minibuffer "property: "))
             (value    (read-from-minibuffer "value: ")))
         (list property value)))
      (org-with-refile nil
                       (let ((buffer-read-only nil))
                         (org-entry-put-multivalued-property nil property values)))))



  (progn ;; "org log note"
    (setq org-log-into-drawer "LOGBOOK")

    (defun org-insert-log-note (txt)
      "Finish taking a log note, and insert it to where it belongs."
      ;; (setq org-log-note-purpose purpose
      ;;       org-log-note-state state
      ;;       org-log-note-previous-state prev-state
      ;;       org-log-note-how how
      ;;       org-log-note-extra extra
      ;;       org-log-note-effective-time (org-current-effective-time))
      (unless (> (marker-position-nonil org-log-note-return-to) 0)
        (move-marker org-log-note-return-to (point)))
      (unless (> (marker-position-nonil org-log-note-marker) 0)
        (move-marker org-log-note-marker (point)))
      ;; Preserve position even if a property drawer is inserted in the
      ;; process.
      (set-marker-insertion-type org-log-note-marker t)
      (let ((txt txt)
            (org-log-note-purpose 'clock-out)
            (org-log-note-effective-time (org-current-effective-time)))
        ;; (kill-buffer (current-buffer))
        (let ((note (cdr (assq org-log-note-purpose org-log-note-headings)))
              lines)
          ;; (while (string-match "\\`# .*\n[ \t\n]*" txt)
          ;;   (setq txt (replace-match "" t t txt)))
          ;; (if (string-match "\\s-+\\'" txt)
          ;;     (setq txt (replace-match "" t t txt)))
          (setq lines (org-split-string txt "\n"))
          (when (and note (string-match "\\S-" note))
            (setq note
                  (org-replace-escapes
                   note
                   (list (cons "%u" (user-login-name))
                         (cons "%U" user-full-name)
                         (cons "%t" (format-time-string
                                     (org-time-stamp-format 'long 'inactive)
                                     org-log-note-effective-time))
                         (cons "%T" (format-time-string
                                     (org-time-stamp-format 'long nil)
                                     org-log-note-effective-time))
                         (cons "%d" (format-time-string
                                     (org-time-stamp-format nil 'inactive)
                                     org-log-note-effective-time))
                         (cons "%D" (format-time-string
                                     (org-time-stamp-format nil nil)
                                     org-log-note-effective-time))
                         (cons "%s" (cond
                                      ((not org-log-note-state) "")
                                      ((org-string-match-p org-ts-regexp
                                                           org-log-note-state)
                                       (format "\"[%s]\""
                                               (substring org-log-note-state 1 -1)))
                                      (t (format "\"%s\"" org-log-note-state))))
                         (cons "%S"
                               (cond
                                 ((not org-log-note-previous-state) "")
                                 ((org-string-match-p org-ts-regexp
                                                      org-log-note-previous-state)
                                  (format "\"[%s]\""
                                          (substring
                                           org-log-note-previous-state 1 -1)))
                                 (t (format "\"%s\""
                                            org-log-note-previous-state)))))))
            (when lines (setq note (concat note " \\\\")))
            (push note lines))
          (when (or current-prefix-arg org-note-abort)
            (when (org-log-into-drawer)
              (org-remove-empty-drawer-at org-log-note-marker))
            (setq lines nil))
          (when lines
            (with-current-buffer (marker-buffer org-log-note-marker)
              (org-with-wide-buffer
               (goto-char org-log-note-marker)
               (move-marker org-log-note-marker nil)
               ;; Make sure point is at the beginning of an empty line.
               (cond ((not (bolp)) (let ((inhibit-read-only t)) (insert "\n")))
                     ((looking-at "[ \t]*\\S-") (save-excursion (insert "\n"))))
               ;; In an existing list, add a new item at the top level.
               ;; Otherwise, indent line like a regular one.
               (let ((itemp (org-in-item-p)))
                 (if itemp
                     (org-indent-line-to
                      (let ((struct (save-excursion
                                      (goto-char itemp) (org-list-struct))))
                        (org-list-get-ind (org-list-get-top-point struct) struct)))
                     (org-indent-line)))
               (insert (org-list-bullet-string "-") (pop lines))
               (let ((ind (org-list-item-body-column (line-beginning-position))))
                 (dolist (line lines)
                   (insert "\n")
                   (org-indent-line-to ind)
                   (insert line)))
               (message "Note stored")
               (org-back-to-heading t)
               (org-cycle-hide-drawers 'children))
              ;; Fix `buffer-undo-list' when `org-store-log-note' is called
              ;; from within `org-add-log-note' because `buffer-undo-list'
              ;; is then modified outside of `org-with-remote-undo'.
              (when (eq this-command 'org-agenda-todo)
                (setcdr buffer-undo-list (cddr buffer-undo-list)))))))
      ;; Don't add undo information when called from `org-agenda-todo'
      (let ((buffer-undo-list (eq this-command 'org-agenda-todo)))
        (set-window-configuration org-log-note-window-configuration)
        (with-current-buffer (marker-buffer org-log-note-return-to)
          (goto-char org-log-note-return-to))
        (move-marker org-log-note-return-to nil)
        (move-marker org-log-note-marker nil)
        (and org-log-post-message (message "%s" org-log-post-message)))))

(provide 'org-misc-utils-lotus)
;;; org-misc-utils-lotus.el ends here
