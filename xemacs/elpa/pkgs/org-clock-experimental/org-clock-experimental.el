;;; org-clock-experimental.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

;; https://emacs.stackexchange.com/questions/3970/org-mode-warning-when-scheduling-task-on-top-of-another-task
(defun check-availability (&optional date)
  "Doc-string."
  (interactive)
  (let* (
         not-available
         (date (if date date (read-string "Date:  ")))
         (date-parsed (parse-time-string date))
         (date-time
          (cond
            ((and
              (not (null (nth 0 date-parsed)))
              (not (null (nth 1 date-parsed)))
              (not (null (nth 2 date-parsed))))
             (date-to-time date))
            ((and
              (null (nth 0 date-parsed))
              (null (nth 1 date-parsed))
              (null (nth 2 date-parsed)))
             (date-to-time (concat date " 00:00")))))
         (date-seconds (time-to-seconds date-time)) )
    (with-current-buffer (get-buffer ".todo")
      (save-excursion
        (goto-char (point-max))
        (catch 'found
          (while (re-search-backward "\\* \\(TODO\\|WORK\\|DONE\\)" nil t)
            (unless (org-at-heading-p)
              (org-back-to-heading t))
            (let* ((element (org-element-at-point))
                   (deadline (org-element-property :deadline element))
                   (deadline-seconds
                    (when deadline
                      (time-to-seconds
                       (org-time-string-to-time
                        (org-element-property :raw-value deadline)))))
                   (scheduled (org-element-property :scheduled element))
                   (scheduled-seconds
                    (when scheduled
                      (time-to-seconds
                       (org-time-string-to-time
                        (org-element-property :raw-value scheduled))))) )
              (when
                  (or
                   (and
                    deadline-seconds
                    (= date-seconds deadline-seconds))
                   (and
                    scheduled-seconds
                    (= date-seconds scheduled-seconds)))
                (setq not-available t)
                (throw 'found (message "Not Available!")))))))
      (unless not-available
        (message "Congratulations -- you are available!")))))

;;{{{ https://stackoverflow.com/questions/4872088/is-there-any-way-for-subtasks-to-inherit-deadlines-in-org-mode

;;How about a function for adding subtasks? This one adds a deadline to the
;;subtask if its parent has one:

(defun my-org-insert-sub-task ()
  (interactive)
  (let ((parent-deadline (org-get-deadline-time nil)))
    (org-goto-sibling)
    (org-insert-todo-subheading t)
    (when parent-deadline
      (org-deadline nil parent-deadline))))

;; Don't forget to bind it to a key:

;; (define-key org-mode-map (kbd "C-c s") 'my-org-insert-sub-task)

;; Also you might find these settings useful:

(setq org-enforce-todo-dependencies t)
(setq org-agenda-dim-blocked-tasks 'invisible)
;;}}}



;;;{{{ https://emacs.stackexchange.com/questions/34905/how-to-clock-offline-hours-quickly


;; The following command inserts the typical org logbook entry, a time range
;; starting from N minutes ago:

(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (let* ((ctime (cdr (decode-time (current-time))))
         (min (car ctime))
         (start (apply 'encode-time 0 (- min n) (cdr ctime))))
    (org-insert-time-stamp start t t "CLOCK: ")
    (insert "--")
    (org-insert-time-stamp (current-time) t t)))




;; mutbuerger's answer helped me a lot. It's my first time programming in elisp
;; other than configuration. His answer was close to the mark, but I still
;; wanted the normal functionality of clock-in and clock-out for finding or
;; creating the logbook under the closest heading, and adding a note.

;; My amended solution was this:

(defun offset-current-time (n)
  (let* ((ctime (cdr (decode-time (current-time))))
         (minutes (car ctime)))
    (apply 'encode-time 0 (- minutes n) (cdr ctime))))

(defun org-insert-clock-range (&optional n)
  (interactive "NTime Offset (in min): ")
  (org-clock-in nil (offset-current-time n))
  (org-clock-out))

(defadvice org-clock-out (after org-clock-out-after activate)
  (ignore-errors                        ;for now
   (when nil (org-update-all-dblocks))))

(add-hook 'org-mode-hook
          (lambda ()
                                        ; Keys for org mode
                                        ;snip
            (define-key evil-normal-state-map (kbd "gl") 'org-insert-clock-range)
                                        ;snip

            ))


;;;}}}

;;;{{{ https://orgmode.org/worg/org-hacks.html#org86e75a5
(defun jump-to-org-agenda ()
  (interactive)
  (let ((buf (get-buffer "*Org Agenda*"))
        wind)
    (if buf
        (if (setq wind (get-buffer-window buf))
            (select-window wind)
            (if (called-interactively-p 'interactive)
                (progn
                  (select-window (display-buffer buf t t))
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )
                (with-selected-window (display-buffer buf)
                  (org-fit-window-to-buffer)
                  ;; (org-agenda-redo)
                  )))
        (call-interactively 'org-agenda-list)))
  ;;(let ((buf (get-buffer "*Calendar*")))
  ;;  (unless (get-buffer-window buf)
  ;;    (org-agenda-goto-calendar)))
  )

(run-with-idle-timer 300 t 'jump-to-org-agenda)
;;;}}}

(provide 'org-clock-experimental)
;;; org-clock-experimental.el ends here
