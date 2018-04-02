;; Required libraries

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Required%20libraries][Required libraries:1]]
(require 'org-capture)
;; Required libraries:1 ends here

;; Preamble

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Preamble][Preamble:1]]
;;; org-capture+.el --- org capture plus

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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

;;; Code:
;; Preamble:1 ends here

;; Overriding org-capture-place-template function


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Overriding%20org-capture-place-template%20function][Overriding org-capture-place-template function:1]]
(defun org-capture-place-template (&optional inhibit-wconf-store)
  "Insert the template at the target location, and display the buffer.
When `inhibit-wconf-store', don't store the window configuration, as it
may have been stored before."
  (unless inhibit-wconf-store
    (org-capture-put :return-to-wconf (current-window-configuration)))
  (delete-other-windows)
  (org-switch-to-buffer-other-window
   (org-capture-get-indirect-buffer (org-capture-get :buffer) "CAPTURE"))
  (widen)
  (outline-show-all)
  (goto-char (org-capture-get :pos))
  (setq-local outline-level 'org-outline-level)
  (pcase (org-capture-get :type)
    ((or `nil `entry) (org-capture-place-entry))
    (`table-line      (org-capture-place-table-line))
    (`plain           (org-capture-place-plain-text))
    (`item            (org-capture-place-item))
    (`checkitem       (org-capture-place-item))
    (`log-note        (org-capture-place-log-note)))
  (org-capture-mode 1)
  (setq-local org-capture-current-plist org-capture-plist))
;; Overriding org-capture-place-template function:1 ends here

;; Providing log note function for capture


;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Providing%20log%20note%20function%20for%20capture][Providing log note function for capture:1]]
(defun org-capture-place-log-note ()
  "Place the template plainly.
If the target locator points at an Org node, place the template into
the text of the entry, before the first child.  If not, place the
template at the beginning or end of the file.
Of course, if exact position has been required, just put it there."
  (let* ((txt (org-capture-get :template))
         beg end)
    ;; (cond
    ;;   ((org-capture-get :exact-position)
    ;;    (goto-char (org-capture-get :exact-position)))
    ;;   ((and (org-capture-get :target-entry-p)
    ;;         (bolp)
    ;;         (looking-at org-outline-regexp))
    ;;    ;; we should place the text into this entry
    ;;    (if (org-capture-get :prepend)
    ;;        ;; Skip meta data and drawers
    ;;        (org-end-of-meta-data t)
    ;;        ;; go to ent of the entry text, before the next headline
    ;;        (outline-next-heading)))
    ;;   (t
    ;;    ;; beginning or end of file
    ;;    (goto-char (if (org-capture-get :prepend) (point-min) (point-max)))))

    (if (and (org-capture-get :target-entry-p)
             (bolp)
             (looking-at org-outline-regexp))
        (let ((note (cdr (assq note-purpose org-log-note-headings)))
              lines)
          (progn
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
              (push note lines)))

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

          ;; (or (bolp) (newline))
          ;; (org-capture-empty-lines-before)
          (setq beg (point))
          (insert (org-list-bullet-string "-") (pop lines))
          (let ((ind (org-list-item-body-column (line-beginning-position))))
            (dolist (line lines)
              (insert "\n")
              (indent-line-to ind)
              (insert line)))
          ;; (message "Note stored")
          ;; (org-capture-empty-lines-after)
          (org-capture-position-for-last-stored beg)
          (setq end (point))
          (org-capture-mark-kill-region beg (1- end))
          (org-capture-narrow beg (1- end))
          (if (or (re-search-backward "%\\?" beg t)
                  (re-search-forward "%\\?" end t))
              (replace-match ""))
          (org-back-to-heading t)
          (org-cycle-hide-drawers 'children)))))
;; Providing log note function for capture:1 ends here

;; Provide this file

;; [[file:~/.repos/git/main/resource/userorg/main/readwrite/public/user/rc/xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Provide%20this%20file][Provide this file:1]]
(provide 'org-capture+)
;;; org-capture+.el ends here
;; Provide this file:1 ends here
