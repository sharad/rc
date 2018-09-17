;;; gnus-schedule-config.el --- sched

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(deh-require-maybe (and diary-lib mm-decode)
  ;; diary-from-outlook-gnus is an interactive compiled Lisp function in
  ;; `diary-lib.el'.

  ;; (diary-from-outlook-gnus &optional NOCONFIRM)

  ;; Maybe snarf diary entry from Outlook-generated message in Gnus.
  ;; Unless the optional argument NOCONFIRM is non-nil (which is the case when
  ;; this function is called interactively), then if an entry is found the
  ;; user is asked to confirm its addition.
  ;; Add this function to `gnus-article-prepare-hook' to notice appointments
  ;; automatically.

  (require 'mm-decode)
  (require 'mm-util)

  (defun diary-from-outlook-gnus-safe ()
    (ignore-errors
     (diary-from-outlook-gnus)))
  (remove-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus)
  ;; this function `diary-from-outlook-gnus'
  ;; when failed with error "no buffer name with multipart/related"
  ;; it left article in the end, so I have to remove it.
  (add-hook 'gnus-article-prepare-hook 'diary-from-outlook-gnus-safe)

  ;; using icalendar.el wotrking

  (deh-require-maybe mm-decode
    (defvar icalendar-outlook-file nil)
    (defun my-save-icalendar (handle)
      (let ((diary "~/.Organize/emacs/diary/outlook"))
        (when (and (equal (car (mm-handle-type handle)) "text/calendar")
                   (gnus-mime-view-part-internally handle)
                   (mm-with-part handle (icalendar-import-buffer diary)))
          (message "Saved calendar entry in %s" diary))))

    (setq gnus-article-mime-part-function 'my-save-icalendar)

    (add-hook
     'gnus-mime-multipart-functions
     '("text/calendar" . my-save-icalendar))))




    (deh-require-maybe (and gnus-sum nntodo todo-gnus)
      ;; http://www.emacswiki.org/emacs/TodoGnus
      (setq nntodo-mbox-file "~/.nntodo")

      ;; Then go into the Server Buffer (with ^) and add there a new Server
      ;; (with a; nntodo as server method) Now go back to the group buffer
      ;; and add your new todo-group (with G m and nntodo as the
      ;; method). You can’t access an empty group, so first you eigther have
      ;; to create a message (C-u a in the group buffer, when over the
      ;; group’s name) or copy/move a message (B c or B m) to the group.

      ;; You maybe don’t want todo groups to be hidden, if there are
      ;; no unread items.

      (setq gnus-permanently-visible-groups "^nntodo+")

      ;; Also it could be usefull to see always all todo items,
      ;; regardless if they are marked as unread or read:

      (setq gnus-parameters
            '(("^nntodo+"
               (display . all)))))







(provide 'gnus-schedule-config)
;;; schedule.el ends here
