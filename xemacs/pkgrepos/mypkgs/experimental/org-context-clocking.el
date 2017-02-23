;;; org-context-clocking.el --- org-context-clocking               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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


(require 'org-clock)
(require 'org-context-clocking-api)



(defvar task-current-file  nil)
(defvar task-previous-file nil)
(defvar task-current-file-time 2)
(defvar last-buffer-select-time (current-time))
(defvar buffer-select-timer nil)
(defvar update-current-file-msg "")
;; (defvar org-entry-clocking-api-name :predicate "API")
(defvar org-entry-clocking-api-name :keys "API")
(defvar org-clocking-api-entries-associated-to-file-p (org-entry-clocking-api-get org-entry-clocking-api-name :entries))
(defvar org-clocking-api-entry-associated-to-file-p   (org-entry-clocking-api-get org-entry-clocking-api-name :entry))
(defvar org-clocking-api-entry-update-task-infos      (org-entry-clocking-api-get org-entry-clocking-api-name :update))


(defun custom-plist-keys (in-plist)
  (if (null in-plist)
      in-plist
      (cons (car in-plist) (custom-plist-keys (cddr in-plist)))))

(defun org-task-clocking-api ()
  "org task clocking select api to use."
  (interactive)
  (let* ((api-keys (custom-plist-keys org-entry-clocking-api))
         (api-name (ido-completing-read
                    "org task clocking api name: "
                    (mapcar 'symbol-name api-keys)
                    nil
                    t
                    (symbol-name org-entry-clocking-api-name)))
         (api-key (intern api-name)))
    (setq org-entry-clocking-api-name api-key)
    (if (and
         (org-entry-clocking-api-get org-entry-clocking-api-name :entries)
         (org-entry-clocking-api-get org-entry-clocking-api-name :entry)
         (org-entry-clocking-api-get org-entry-clocking-api-name :update))
        (setq
         org-clocking-api-entries-associated-to-file-p (org-entry-clocking-api-get org-entry-clocking-api-name :entries)
         org-clocking-api-entry-associated-to-file-p   (org-entry-clocking-api-get org-entry-clocking-api-name :entry)
         org-clocking-api-entry-update-task-infos      (org-entry-clocking-api-get org-entry-clocking-api-name :update)))))

(defun org-clocking-entry-update-task-infos (&optional force)
  "Update task infos"
  (interactive "P")
  (funcall org-clocking-api-entry-update-task-infos force))

(defun update-current-file ()
  (if (> (float-time
          (time-since last-buffer-select-time))
         task-current-file-time)
      (let* ((buff (window-buffer))
             (file (buffer-file-name buff)))
        (unless (or
                 (and
                  (string-equal task-previous-file file)
                  (string-equal task-current-file  file))
                 (minibufferp buff))
          (setq
           task-previous-file task-current-file
           task-current-file  file)

          (unless (org-clock-entry-associated-to-file-p file)
            (org-entry-run-associated-clock file))))))

(defun org-clock-entry-current-entry ()
  (and
   ;; file
   org-clock-marker
   (> (marker-position-nonil org-clock-marker) 0)
   (org-with-clock-position (list org-clock-marker)
     (org-previous-visible-heading 1)
     (let ((info (org-entry-collect-task-info)))
       info))))

;; not workiong
;; (defun org-clock-entry-associated-to-file-p (file)
;;   (and
;;    ;; file
;;    org-clock-marker
;;    (> (marker-position-nonil org-clock-marker) 0)
;;    (org-with-clock-position (list org-clock-marker)
;;      (org-previous-visible-heading 1)
;;      (let ((info (org-entry-collect-task-info)))
;;        (if (funcall org-clocking-api-entry-associated-to-file-p info file)
;;            info)))))

(defun org-clock-entry-associated-to-file-p (file)
  (let ((info (org-clock-entry-current-entry)))
    (funcall org-clocking-api-entry-associated-to-file-p info file)))

(defun org-entry-run-associated-clock (file)
  (let ()
    (let* ((matched-clocks
            (remove-if-not
             '(lambda (marker) (marker-buffer marker))
             (org-markers-associated-to-file file)))
           (selected-clock (if (> (length matched-clocks) 1)
                               (org-clock-select-task-from-clocks matched-clocks)
                               (car matched-clocks))))
      (if selected-clock
          (let ((org-log-note-clock-out nil)
                (prev-org-clock-buff (marker-buffer org-clock-marker)))

            (let ((prev-clock-buff-read-only
                   (if prev-org-clock-buff
                       (with-current-buffer (marker-buffer org-clock-marker)
                         buffer-read-only))))

              (if prev-org-clock-buff
                  (with-current-buffer prev-org-clock-buff
                    (setq buffer-read-only nil)))

              (setq update-current-file-msg org-clock-marker)

              (with-current-buffer (marker-buffer selected-clock)
                (let ((buffer-read-only nil))
                  (org-clock-clock-in (list selected-clock))))

              (if prev-org-clock-buff
                  (with-current-buffer prev-org-clock-buff
                    (setq buffer-read-only prev-clock-buff-read-only)))))
          (setq update-current-file-msg "null clock")))))

;; (defun run-task-current-file-timer ()
;;   (let ()
;;     (setq last-buffer-select-time (current-time))
;;     (when buffer-select-timer
;;       (cancel-timer buffer-select-timer)
;;       (setq buffer-select-timer nil))
;;     (setq buffer-select-timer
;;           (run-with-timer
;;            (1+ task-current-file-time)
;;            nil
;;            'update-current-file))))

(defun run-task-current-file-timer ()
  (let ()
    (setq last-buffer-select-time (current-time))
    (when buffer-select-timer
      (cancel-timer buffer-select-timer)
      (setq buffer-select-timer nil))
    (setq buffer-select-timer
          ;; distrubing while editing.
          ;; (run-with-timer
          (run-with-idle-timer
           (1+ task-current-file-time)
           nil
           'update-current-file))))

(defun org-entries-select (entries &optional prompt)
  "Select a task that was recently associated with clocking."
  (interactive)
  (let (och
        chl
        sel-list
        rpl
        (i 0)
        s)
    ;; Remove successive dups from the clock history to consider
    ;; (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
    ;;       clocks)
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
        (user-error "No recent clock")
        (save-window-excursion
          (org-switch-to-buffer-other-window
           (get-buffer-create "*Clock Task Select*"))
          (erase-buffer)
          (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
          (mapc
           (lambda (m)
             (when (marker-buffer m)
               (setq i (1+ i)
                     s (org-clock-insert-selection-line
                        (if (< i 10)
                            (+ i ?0)
                            (+ i (- ?A 10))) m))
               (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
               (push s sel-list)))
           och)
          (run-hooks 'org-clock-before-select-task-hook)
          (goto-char (point-min))
          ;; Set min-height relatively to circumvent a possible but in
          ;; `fit-window-to-buffer'
          (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
          (message (or prompt "Select task for clocking:"))
          (setq cursor-type nil rpl (read-char-exclusive))
          (kill-buffer)
          (cond
            ((eq rpl ?q) nil)
            ((eq rpl ?x) nil)
            ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
            (t (user-error "Invalid task choice %c" rpl)))))))

(defun org-clock-select-task-from-clocks (clocks &optional prompt)
  "Select a task that was recently associated with clocking."
  (interactive)
  (let (och chl sel-list rpl (i 0) s)
    ;; Remove successive dups from the clock history to consider
    (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
          clocks)
    (setq och (reverse och) chl (length och))
    (if (zerop chl)
        (user-error "No recent clock")
        (save-window-excursion
          (org-switch-to-buffer-other-window
           (get-buffer-create "*Clock Task Select*"))
          (erase-buffer)
          ;; (when (marker-buffer org-clock-default-task)
          ;;   (insert (org-add-props "Default Task\n" nil 'face 'bold))
          ;;   (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
          ;;   (push s sel-list))
          ;; (when (marker-buffer org-clock-interrupted-task)
          ;;   (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
          ;;   (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
          ;;   (push s sel-list))
          ;; (when (org-clocking-p)
          ;;   (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
          ;;   (setq s (org-clock-insert-selection-line ?c org-clock-marker))
          ;;   (push s sel-list))
          (insert (org-add-props "Guessed Tasks\n" nil 'face 'bold))
          (mapc
           (lambda (m)
             (when (marker-buffer m)
               (setq i (1+ i)
                     s (org-clock-insert-selection-line
                        (if (< i 10)
                            (+ i ?0)
                            (+ i (- ?A 10))) m))
               (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
               (push s sel-list)))
           och)
          (run-hooks 'org-clock-before-select-task-hook)
          (goto-char (point-min))
          ;; Set min-height relatively to circumvent a possible but in
          ;; `fit-window-to-buffer'
          (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
          (message (or prompt "Select task for clocking:"))
          (setq cursor-type nil rpl (read-char-exclusive))
          (kill-buffer)
          (cond
            ((eq rpl ?q) nil)
            ((eq rpl ?x) nil)
            ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
            (t (user-error "Invalid task choice %c" rpl)))))))

(progn
  (add-hook 'buffer-list-update-hook     'run-task-current-file-timer)
  (add-hook 'elscreen-screen-update-hook 'run-task-current-file-timer)
  (add-hook 'elscreen-goto-hook          'run-task-current-file-timer))

(when nil
  (remove-hook 'buffer-list-update-hook 'run-task-current-file-timer)
  (setq buffer-list-update-hook nil)
  (remove-hook 'elscreen-screen-update-hook 'run-task-current-file-timer)
  (remove-hook 'elscreen-goto-hook 'run-task-current-file-timer))


(progn ;; "Org task clock reporting"
  ;; #+BEGIN: task-clock-report-with-comment :parameter1 value1 :parameter2 value2 ...
  ;; #+END:
  (defun org-dblock-write:task-clock-report-with-comment (params)
    (let ((fmt (or (plist-get params :format) "%d. %m. %Y")))
      (insert "Last block update at: "
              (format-time-string fmt))))

  (progn ;; "time sheet"
    ))








(when nil

  (org-entry-run-associated-clock
   (buffer-file-name))

  (org-entry-run-associated-clock
   "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")

  (org-markers-associated-to-file
   (buffer-file-name))

  (org-clock-entry-associated-to-file-p
   (buffer-file-name))

  ;; sharad
  (setq test-info-entry
        (let ((xfile (buffer-file-name)))
          (org-with-clock-position (list org-clock-marker)
            (org-previous-visible-heading 1)
            (let ((info (org-entry-collect-task-info)))
              (if (funcall org-clocking-api-entry-associated-to-file-p info xfile)
                  info))))
        )

  (funcall org-clocking-api-entry-associated-to-file-p (org-clock-entry-current-entry) (buffer-file-name))




  (test-info-entry)
  (funcall org-clocking-api-entry-associated-to-file-p test-info-entry (buffer-file-name))

  ;; org-clock-marker
  (org-entries-associated-key-fn-value :current-clock test-info-entry (buffer-file-name))

  (org-clock-entry-associated-to-file-p
   "~/Docume1nts/CreatedContent/contents/org/tasks/meru/report.org")

  (org-clock-entry-associated-to-file-p
   "~/Documents/CreatedContent/contents/org/tasks/meru/features/patch-mgm/todo.org")

  ;; (org-entry-associated-file-org-file-p
  ;;  "~/Documents/CreatedContent/contents/org/tasks/meru/report.org"
  ;;  (cadr org-entry-list-task-infos)))


  (length
   (funcall org-clocking-api-entries-associated-to-file-p (buffer-file-name))
   )

  (length (funcall org-clocking-api-entries-associated-to-file-p "/home/s/paradise/releases/global/patch-upgrade/Makefile"))

  (org-markers-associated-to-file (buffer-file-name))
  (length
   (funcall org-clocking-api-entries-associated-to-file-p "~/Documents/CreatedContent/contents/org/tasks/meru/report.org")
   )

  (org-entries-associated-to-file-by-keys-p (buffer-file-name))

  (length
   (org-entries-associated-to-file-by-keys-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")
   )

  (org-clock-entry-associated-to-file-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")

  ;; (org-entry-associated-to-file-by-keys-p "/home/s/paradise/releases/global/patch-upgrade/Makefile")

  (if (org-clock-entry-associated-to-file-p (buffer-file-name))
      (message "current clock is with file")
      ))

(provide 'org-context-clocking)
;;; org-context-clocking.el ends here
