;;; org-clock-resolve-advanced.el ---   -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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


(provide 'org-clock-resolve-advanced)



(require 'org-rl-clock)


;;;###autoload
(defvar org-clock-last-user-idle-seconds nil)

(defun org-rl-resolve-clocks-if-idle ()
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  ;; last-input-event
  ;; last-event-frame
  (when nil (message "called org-rl-resolve-clocks-if-idle"))
  (lotus-with-other-frame-event-debug "org-rl-resolve-clocks-if-idle" :restart
    (message "org-clock-last-user-idle-seconds: %s" org-clock-last-user-idle-seconds)
    ;; (message "(org-user-idle-seconds) %s" (org-user-idle-seconds))
    (when (and
           org-clock-idle-time
           (not org-clock-resolving-clocks)
           org-clock-marker
           (marker-buffer org-clock-marker))
      (let* ((org-clock-user-idle-seconds
              (or
               org-clock-last-user-idle-seconds
               (org-user-idle-seconds)))
             (org-clock-user-idle-start
              (time-subtract (current-time)
                             (seconds-to-time org-clock-user-idle-seconds)))
             (org-clock-resolving-clocks-due-to-idleness t))

        (setq org-clock-last-user-idle-seconds org-clock-user-idle-seconds)

        (when nil
          (message "1. Idle time now sec[%d] min[%d]"
                   org-clock-user-idle-seconds
                   (/ org-clock-user-idle-seconds 60)))

        (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
            (org-rl-clock-resolve-time
             (org-rl-make-clock org-clock-marker
                                org-clock-start-time
                                org-clock-user-idle-start) ;TODO: what important.
             (org-rl-make-clock nil 'now 'now))
          (when nil
            (message "2. Idle time now sec[%d] min[%d]"
                     org-clock-user-idle-seconds
                     (/ org-clock-user-idle-seconds 60))))
        (setq org-clock-last-user-idle-seconds nil)))))

(defalias 'org-resolve-clocks-if-idle 'org-rl-resolve-clocks-if-idle)

(defun org-rl-clock-set-correct-idle-timer ()
  (interactive)
  (let* ((minor-offset 10)
         (idle-time (+ (* org-clock-idle-time 60) minor-offset)))
    (when org-clock-idle-timer
      (cancel-timer org-clock-idle-timer)
      (setq org-clock-idle-timer
            (run-with-idle-timer
             idle-time idle-time
             'org-rl-resolve-clocks-if-idle)))))

;;;###autoload
(defun org-rl-resolve-clocks (&optional only-dangling-p prompt-fn last-valid)
  "Resolve all currently open Org clocks.
If `only-dangling-p' is non-nil, only ask to resolve dangling
\(i.e., not currently open and valid) clocks."
  (interactive "P")
  ;; last-input-event
  ;; last-event-frame
  (unless org-clock-resolving-clocks
    (let ((org-clock-resolving-clocks t))
      (dolist (file (org-files-list))
        (let ((clocks (org-find-open-clocks file)))
          (dolist (clock clocks)
            (let ((dangling (or (not (org-clock-is-active))
                                (/= (car clock) org-clock-marker))))
              (when (or (not only-dangling-p) dangling)
                (org-rl-clock-time
                 (org-rl-make-clock (car clock) (cdr clock) nil)
                 (org-rl-make-clock 'imaginary 'now (cdr clock)))))))))))

;;;###autoload
(defalias 'org-resolve-clocks 'org-rl-resolve-clocks)

;;;###autoload
(defun org-clock-resolve-clocks (clocks) ;TODO
  (let ((next (pop clocks))
        (prev (pop clocks)))
    (org-rl-clock-time next prev)))

;;;###autoload
(defun org-clock-resolve-advanced-insinuate ()
  (interactive)
  (defalias 'org-resolve-clocks-if-idle 'org-rl-resolve-clocks-if-idle)
  (add-hook 'org-clock-in-hook
            #'org-rl-clock-set-correct-idle-timer)
  (defalias 'org-resolve-clocks 'org-rl-resolve-clocks))

;;;###autoload
(defun org-clock-resolve-advanced-uninsinuate ()
  (remove-hook 'org-clock-in-hook
               #'org-rl-clock-set-correct-idle-timer))



(defun org-find-open-clocks (file)
  "Search through the given file and find all open clocks."
  (let ((buf (or (get-file-buffer file)
                 (find-file-noselect file)))
        (org-clock-re (concat org-clock-string " \\(\\[.*?\\]\\)$"))
        clocks)
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward org-clock-re nil t)
          (push (cons (copy-marker (match-end 1) t)
                      (org-time-string-to-time (match-string 1))) clocks))))
    clocks))




(defun org-rl-first-clock-started-mins (marker)
  (let* ((clock-time (org-clock-get-nth-half-clock-time marker 1))
         (mins-spent
          (when clock-time
            (/
             (float-time
              (time-subtract
               (current-time)
               (org-clock-get-nth-half-clock-time marker 1)))
             60))))
    mins-spent))

;;;###autoload
(defun test-org-rl-resolve-clock-force (idle-sec)
  "Resolve all currently open Org clocks.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  (interactive
   (let* ((marker
           (if current-prefix-arg
               (point-marker)
             org-clock-marker))
          (mins-spent
           (or
            (org-rl-first-clock-started-mins marker)
            0)))
     (list (*
            (read-number
             (format "clock[ %s ] Spent mins: " (org-get-heading-from-clock (list marker)))
             (org-rl-first-clock-started-mins marker))
            60))))
  (let* ((marker
          (if current-prefix-arg
              (point-marker)
            org-clock-marker))
         (start-time (org-clock-get-nth-half-clock-time marker 1))
         (mins-spent
          (or
           (org-rl-first-clock-started-mins marker)
           0)))
    (if (> mins-spent 1)
        (if (< 1 (/ (abs idle-sec) 60) (1- mins-spent))
            (when (and
                   org-clock-idle-time
                   (not org-clock-resolving-clocks)
                   marker
                   (marker-buffer marker))
              (let* ((org-clock-user-idle-seconds (abs idle-sec))
                     (org-clock-user-idle-start
                      (time-subtract (current-time)
                                     (seconds-to-time org-clock-user-idle-seconds)))
                     (org-clock-resolving-clocks-due-to-idleness t))
                (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
                    (org-rl-clock-resolve-time
                     (org-rl-make-clock marker start-time nil)
                     (org-rl-make-clock 'imaginary 'now org-clock-user-idle-start))
                  (when t
                    (message "Idle time now min[%d] sec[%d]"
                             (/ org-clock-user-idle-seconds 60)
                             (% org-clock-user-idle-seconds 60)
                             )))))
          (org-rl-debug :warning "Selected min[ = %d ] is more than mins-spent[ = %d ]" (/ idle-sec 60) mins-spent))
      (org-rl-debug :warning "Not one min is spent with clock mins-spent = %d" mins-spent))))

;;; org-clock-utils-lotus.el ends here
