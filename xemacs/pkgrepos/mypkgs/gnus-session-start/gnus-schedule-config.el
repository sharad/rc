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



(deh-require-maybe diary-lib
  ;; diary-from-outlook-gnus is an interactive compiled Lisp function in
  ;; `diary-lib.el'.

  ;; (diary-from-outlook-gnus &optional NOCONFIRM)

  ;; Maybe snarf diary entry from Outlook-generated message in Gnus.
  ;; Unless the optional argument NOCONFIRM is non-nil (which is the case when
  ;; this function is called interactively), then if an entry is found the
  ;; user is asked to confirm its addition.
  ;; Add this function to `gnus-article-prepare-hook' to notice appointments
  ;; automatically.

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
    (defun my-save-icalendar (handle)
      (let ((diary "~/.Organize/emacs/diary/outlook)"))
        (when (and (equal (car (mm-handle-type handle)) "text/calendar")
                   (gnus-mime-view-part-internally handle)
                   (mm-with-part handle (icalendar-import-buffer diary)))
          (message "Saved calendar entry in %s" diary))))

  (setq gnus-article-mime-part-function 'my-save-icalendar)

  (add-hook
   'gnus-mime-multipart-functions
   '("text/calendar" . my-save-icalendar))))









(provide 'gnus-schedule-config)
;;; schedule.el ends here
