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
(require 'org-rl-obj-cps)



(defvar org-rl-clock-resolve-time #'org-rl-clock-cps-resolve-time "org clock resolver.")

;;;###autoload
(defvar org-clock-last-idle-start-time nil)

(defun org-rl-resolve-clocks-if-idle ()
  "Resolve all currently open Org clock.
This is performed after `org-clock-idle-time' minutes, to check
if the user really wants to stay clocked in after being idle for
so long."
  ;; last-input-event
  ;; last-event-frame

  (org-rl-debug nil "%s: org-rl-resolve-clocks-if-idle: begin" (time-stamp-string))
  (lotus-with-override-minibuffer-if
      (progn
        (org-rl-debug nil "org-rl-resolve-clocks-if-idle: [minibuffer-body] lotus-with-override-minibuffer-if active minibuffer found aborting it."))
    (lotus-with-other-frame-event-debug "org-rl-resolve-clocks-if-idle" :restart)
    (condition-case err
        (progn
          (org-rl-debug nil "org-rl-resolve-clocks-if-idle: lotus-with-other-frame-event-debug")
          (org-rl-debug nil
           "org-rl-resolve-clocks-if-idle: pass1 org-clock-last-idle-start-time: %s, (org-user-idle-seconds) %s"
           (if org-clock-last-idle-start-time
               (time-to-seconds (time-subtract (current-time) org-clock-last-idle-start-time)))
           (org-user-idle-seconds))
          ;; (org-rl-debug nil "(org-user-idle-seconds) %s" (org-user-idle-seconds))
          (when (and
                 org-clock-idle-time
                 (not org-clock-resolving-clocks)
                 org-clock-marker
                 (marker-buffer org-clock-marker))
            (org-rl-debug nil
             "org-rl-resolve-clocks-if-idle: pass2 org-clock-last-idle-start-time: %s, (org-user-idle-seconds) %s"
             (if org-clock-last-idle-start-time
                 (time-to-seconds (time-subtract (current-time) org-clock-last-idle-start-time)))
             (org-user-idle-seconds))
            (let* ((org-clock-user-idle-seconds
                    (if org-clock-last-idle-start-time
                        (time-to-seconds
                         (time-subtract (current-time) org-clock-last-idle-start-time))
                      (org-user-idle-seconds)))
                   (org-clock-user-idle-start
                    (time-subtract (current-time)
                                   org-clock-user-idle-seconds))
                   (org-clock-resolving-clocks-due-to-idleness t))

              (setq org-clock-last-idle-start-time org-clock-user-idle-start)

              (if (> org-clock-user-idle-seconds (* 60 org-clock-idle-time))
                  (funcall org-rl-clock-resolve-time
                   (org-rl-make-clock org-clock-marker
                                      org-clock-start-time
                                      org-clock-user-idle-start
                                      t) ;TODO: what important.
                   (org-rl-make-clock nil 'now 'now)
                   'ask nil nil)
                (org-rl-debug nil
                 "org-rl-resolve-clocks-if-idle: pass3 not calling resolve time org-clock-last-idle-start-time: %s, (org-user-idle-seconds) %s"
                 (if org-clock-last-idle-start-time
                     (time-to-seconds (time-subtract (current-time) org-clock-last-idle-start-time)))
                 (org-user-idle-seconds)))
              (org-rl-debug :warning "Resetting org-clock-last-idle-start-time [= %s] to nil" org-clock-last-idle-start-time)
              (setq org-clock-last-idle-start-time nil)
              (org-rl-debug :warning "Reset org-clock-last-idle-start-time to %s" org-clock-last-idle-start-time))))))

      ;; (error
      ;;  (progn
      ;;    (setq org-clock-last-idle-start-time nil)
      ;;    (error "Error: %s" err)))

  (org-rl-debug nil "%s: org-rl-resolve-clocks-if-idle: finished" (time-stamp-string)))

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
                (funcall org-rl-clock-resolve-time
                 (org-rl-make-clock (car clock) (cdr clock) (cdr clock))
                 (org-rl-make-clock 'imaginary 'now 'now)
                 nil nil nil))))))))) ;TODO: should not be now ?

;;;###autoload
(defalias 'org-resolve-clocks 'org-rl-resolve-clocks)

;;;###autoload
(defun org-clock-resolve-clocks (clocks) ;TODO
  (let ((next (pop clocks))
        (prev (pop clocks)))
    (funcall org-rl-clock-resolve-time next prev)))


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
             (format "clock[ %s ] Resolve mins: " (org-get-heading-from-clock (list marker)))
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
                    (funcall org-rl-clock-resolve-time
                     (org-rl-make-clock marker start-time org-clock-user-idle-start t)
                     (org-rl-make-clock 'imaginary 'now 'now))
                  (when t
                    (org-rl-debug nil "Idle time now min[%d] sec[%d]"
                             (/ org-clock-user-idle-seconds 60)
                             (% org-clock-user-idle-seconds 60))))))
          (org-rl-debug nil "Selected min[ = %d ] is more than mins-spent[ = %d ]" (/ idle-sec 60) mins-spent))
      (org-rl-debug nil "Not one min is spent with clock mins-spent = %d" mins-spent))))

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


;;; org-clock-utils-lotus.el ends here
