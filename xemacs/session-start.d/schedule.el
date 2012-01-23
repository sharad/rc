;;; schedule.el --- Diary, Calendar etc

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



(defun fancy-diary-display-week-graph-if-appt ()

  (if (or (not diary-entries-list)
          (and (not (cdr diary-entries-list))
               (string-equal (car (cdr (car diary-entries-list))) "")))

      (let* ((holiday-list (if holidays-in-diary-buffer
                               (check-calendar-holidays original-date)))
             (msg (format "No diary entries for %s %s"
                          (concat date-string (if holiday-list ":" ""))
                          (mapconcat 'identity holiday-list "; "))))
        (if (<= (length msg) (frame-width))
            (message msg)
          (set-buffer (get-buffer-create holiday-buffer))
          (setq buffer-read-only nil)
          (calendar-set-mode-line date-string)
          (erase-buffer)
          (insert (mapconcat 'identity holiday-list "\n"))
          (goto-char (point-min))
          (set-buffer-modified-p nil)
          (setq buffer-read-only t)
          (display-buffer holiday-buffer)
          (message  "No diary entries for %s" date-string)))

      (fancy-diary-display-week-graph)))

(deh-require-maybe 'weekly-view
  ;; http://www.emacswiki.org/emacs/CalendarWeeklyView
  ;; (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph))
  (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt)


  (defun toggle-fancy-diary-display-week-graph ()
    (interactive)
    (if (memq 'fancy-diary-display-week-graph-if-appt diary-display-function)
        (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph-if-appt)
        (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt))))





(user-provide 'schedule)
;;; schedule.el ends here

