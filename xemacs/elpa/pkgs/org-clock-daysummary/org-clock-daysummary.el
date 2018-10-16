;;; org-clock-daysummary.el --- Basic macros                 -*- lexical-binding: t; -*-

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
;; put in .emacs
;; (org-clock-work-day-mode-line-add t)

;;

;;; Code:



;; (require 'mode-line-config)

(require 'spaceline)

(eval-when-compile
  '(require 'spaceline))

(require 'file-utils)

(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(defvar org-clock-work-day-hours 8 "work day hours")

(defvar org-clock-monitor-files nil
  "org clock monitor files")

(defvar org-clock-monitor-files-mins-aggregate-internal nil
  "org clock monitor files")

(defvar org-clock-work-day-start 10)
(defvar org-clock-work-day-lunch-break-hour 1)
(defvar org-clock-work-day-end (+ org-clock-work-day-start org-clock-work-day-lunch-break-hour org-clock-work-day-hours))

(defvar org-clock-work-day-msg nil)

(defvar org-clock-work-day-mode-line-map (make-sparse-keymap))
(defvar org-mode-work-day-mode-line-string "" "Hello")
(put 'org-mode-work-day-line-string 'risky-local-variable t)
;; (get 'org-mode-work-day-line-string 'risky-local-variable)
;; (get 'org-mode-line-string 'risky-local-variable)

(defvar org-clock-get-work-day-clock-string-separator nil)
(defvar org-mode-work-mode-line-timer nil)
(defvar org-mode-work-day-pause nil)
(defvar org-mode-work-day-display 'mode-line)

(defvar org-clock-get-work-day-start-timer nil)
(unless org-clock-get-work-day-start-timer
  (setq org-clock-get-work-day-start-timer
        (run-at-time "00:01am" (* 24 60 60) 'org-clock-get-work-day-clock-string t)))


(defface org-mode-line-wday
    '((t (:inherit mode-line)))
  "Face used for clock display in mode line."
  :group 'org-faces)

(defface org-mode-line-wday-underrun
    '((t (:inherit mode-line :foreground "Green")))
  "Face used for clock display for overrun tasks in mode line."
  :group 'org-faces)

(defface org-mode-line-wday-overrun
    '((t (:inherit mode-line :background "red")))
  "Face used for clock display for overrun tasks in mode line."
  :group 'org-faces)

(defvar org-work-day-face 'org-mode-line-clock)
(defvar org-work-day-face-overrun 'org-mode-line-clock-overrun)


(setq
 org-work-day-face          'org-mode-line-wday
 org-work-day-face-underrun 'org-mode-line-wday-underrun
 org-work-day-face-overrun  'org-mode-line-wday-overrun)

;; (setq
;;  org-work-day-face          'org-mode-line-clock
;;  org-work-day-face-underrun 'org-mode-line-wday-overrun
;;  org-work-day-face-overrun  'org-mode-line-clock-overrun)

(defun org-clock-unclocked-files-mins-today (files)
  (let* ((totalmins 0)
         file)
    (org-agenda-prepare-buffers files)
    (while (setq file (pop files))
      (with-current-buffer (find-buffer-visiting file)
        (save-excursion
          (save-restriction
            (incf totalmins (org-clock-sum-today))))))
    totalmins))

(defun org-clock-clocked-secs-today ()
  "Get the clocked time for the current item in minutes.
The time returned includes the time spent on this task in
previous clocking intervals."
  (if (org-clock-is-active)
      (let ((currently-clocked-time
             (floor (- (org-float-time)
                       (org-float-time org-clock-start-time)))))
        currently-clocked-time)
      0))

(defun org-clock-files-secs (files &optional all)
  (+
   (org-clock-clocked-secs-today)
   (*
    60
    (if (or
         all
         (null org-clock-monitor-files-mins-aggregate-internal))
        (setq
         org-clock-monitor-files-mins-aggregate-internal
         (org-clock-unclocked-files-mins-today files))
        org-clock-monitor-files-mins-aggregate-internal))))

(defun org-clock-files-min-today (&optional force)
  (interactive "P")
  (if org-clock-monitor-files
      (let* ((today-clock-secs (org-clock-files-secs org-clock-monitor-files force))
             (secs (- (* org-clock-work-day-hours 60 60) today-clock-secs))
             (remiain-today-clock-hms (org-timer-secs-to-hms secs)))
        (message "%s left for today." (format "%s" remiain-today-clock-hms)))))


(defun org-clock-work-day-start-secs ()
  (floor
   (org-float-time
    (apply 'encode-time
           (append '(0 0 0) (cdddr (decode-time)))))))
;; (org-float-time org-clock-work-day-end)

(define-key org-clock-work-day-mode-line-map [mode-line mouse-2] 'org-clock-files-min-today)
(define-key org-clock-work-day-mode-line-map [mode-line mouse-1] 'org-clock-work-day-menu)

(defun org-clock-work-day-menu ()
  (interactive)
  (popup-menu
   '("Clock"
     ["Clock out" org-clock-out t]
     ["Change effort estimate" org-clock-modify-effort-estimate t]
     ["Go to clock entry" org-clock-goto t]
     ["Switch task" (lambda () (interactive) (org-clock-in '(4))) :active t :keys "C-u C-c C-x C-i"])))

;; TODO: optimize it.
(defun org-clock-get-work-day-clock-string (&optional force)
  "Form a clock-string, that will be shown in the mode line.
If an effort estimate was defined for the current item, use
01:30/01:50 format (clocked/estimated).
If not, show simply the clocked time like 01:50."
  (if org-clock-monitor-files
      (let* ((now-sec             (floor (org-float-time)))
             (day-start-secs      (org-clock-work-day-start-secs))
             (work-day-start-secs (+ day-start-secs (* org-clock-work-day-start 60 60)))
             (work-day-end-secs   (+ day-start-secs (* org-clock-work-day-end 60 60)))
             (work-day-over-secs  (- now-sec work-day-start-secs))
             (work-day-left-secs  (- work-day-end-secs now-sec))
             (work-day-over-str   (org-timer-secs-to-hms work-day-over-secs))
             (work-day-left-str   (org-timer-secs-to-hms work-day-left-secs))
             (today-clocked-secs  (org-clock-files-secs org-clock-monitor-files force))
             (today-dur-left-sec  (- (* org-clock-work-day-hours 60 60) today-clocked-secs))
             (today-dur-left-str  (org-timer-secs-to-hms today-dur-left-sec))
             (work-done-str
              (org-propertize
               today-dur-left-str
               'face (if (< today-dur-left-sec work-day-left-secs) ;; t ;; org-clock-task-overrun
                         org-work-day-face-underrun
                       org-work-day-face-overrun)))
             (work-day-time-str
              (org-minutes-to-clocksum-string (* org-clock-work-day-hours 60)))
             (clockstr (org-propertize
                        (concat
                         (if org-clock-get-work-day-clock-string-separator " " "")
                         "["
                         "%s %s/%s %s"
                         "]"
                         ;; (if org-clock-work-day-msg
                         ;;     (concat " (" (replace-regexp-in-string "%" "%%" org-clock-work-day-msg) ")"))
                         )
                        'face org-work-day-face)))
        (format clockstr
                work-day-over-str
                work-done-str
                work-day-time-str
                work-day-left-str))
      (message "org-clock-monitor-files is not set")))

(defun org-clock-monitor-files-set-from-dir (monitor-dir)
  (interactive "Dset org clock monitor dir: ")
  (setq
   org-clock-monitor-files (directory-files-recursive monitor-dir "\\.org$" 2 "\\(rip\\|stage\\)")))

(defun org-clock-monitor-files-add-from-dir (monitor-dir)
  (interactive "Dadd org clock monitor dir: ")
  (setq
   org-clock-monitor-files
   (append
    org-clock-monitor-files
    (directory-files-recursive monitor-dir "\\.org$" 2 "\\(rip\\|stage\\)"))))

;;;###autoload
(defun org-clock-work-day-update-mode-line-internal (&optional force)
  ;; (defun org-clock-work-day-update-mode-line ()
  (if org-clock-monitor-files
      (progn
        (setq org-mode-work-day-mode-line-string
              (org-propertize
               (let ((clock-string (org-clock-get-work-day-clock-string force))
                     (help-text
                      ;; "Org-mode clock is running.\nmouse-1 shows a menu\nmouse-2 will jump to task"
                      "Today's work clocks."
                      ))
                 (if (and (> org-clock-string-limit 0)
                          (> (length clock-string) org-clock-string-limit))
                     (org-propertize
                      (substring clock-string 0 org-clock-string-limit)
                      'help-echo (concat help-text ": " org-clock-heading))
                   (org-propertize clock-string 'help-echo help-text)))

               'local-map org-clock-work-day-mode-line-map
               'mouse-face (if (featurep 'xemacs) 'highlight 'mode-line-highlight)))
        (force-mode-line-update))
    (message "org-clock-monitor-files is not set")))

;;;###autoload
(defun org-clock-work-day-update-mode-line (&optional force)
  "Update the timer time in the mode line."
  (if org-clock-monitor-files
      (if org-mode-work-day-pause
          nil
        (org-clock-work-day-update-mode-line-internal force)
        (force-mode-line-update))
    (message "org-clock-monitor-files is not set")))

;;;###autoload
(defun org-clock-work-day-mode-line-add (force)
  (interactive "P")
  ;; (or global-mode-string (setq global-mode-string '("")))
  ;; (or (memq 'org-mode-work-day-mode-line-string global-mode-string)
  ;;     (setq global-mode-string
  ;;           (append global-mode-string
  ;;                   '(org-mode-work-day-mode-line-string))))

  (when (and
         (boundp 'global-mode-line-list)
         global-mode-line-list)
    (progn
      (or global-mode-line-list (setq global-mode-string '("")))
      (or (memq 'org-mode-work-day-mode-line-string global-mode-line-list)
          (setq global-mode-line-list
                (append global-mode-line-list
                        '(org-mode-work-day-mode-line-string))))))

  (when (fboundp 'spaceline-define-segment)
    (spaceline-define-segment workdaysummary
      "Test"
      (powerline-raw
       (s-trim org-mode-work-day-mode-line-string)))
    (spaceline-toggle-workdaysummary-on)
    (spaceline-spacemacs-theme 'workdaysummary))

  (when (eq org-mode-work-day-display 'mode-line)
    (org-clock-work-day-update-mode-line force)
    (when org-mode-work-mode-line-timer
      (cancel-timer org-mode-work-mode-line-timer)
      (setq org-mode-work-mode-line-timer nil))
    (setq org-mode-work-mode-line-timer
          (run-with-timer 20 20 'org-clock-work-day-update-mode-line))))

;;;###autoload
(defun org-clock-work-day-mode-line-remove ()
  (interactive)
  (if (and
       (boundp 'global-mode-line-list)
       global-mode-line-list)
      (progn
       ;; (setq global-mode-string
       ;;       (delq 'org-mode-work-day-mode-line-string global-mode-string))
       (setq global-mode-line-list
            (delq 'org-mode-work-day-mode-line-string global-mode-line-list))))

      (when org-mode-work-mode-line-timer
        (cancel-timer org-mode-work-mode-line-timer)
        (setq org-mode-work-mode-line-timer nil)))


(provide 'org-clock-daysummary)
;;; org-clock-daysummary.el ends here
