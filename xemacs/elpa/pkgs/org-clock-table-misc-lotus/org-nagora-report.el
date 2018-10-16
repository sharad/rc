;;; org-nagora-report.el --- org-nagora-report               -*- lexical-binding: t; -*-

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

(require 'org-table)
(require 'org-clock)

(defun dfeich/org-clock-get-tr-for-ivl (buffer tstart-str tend-str &optional limit)
  "Return clocking information touching a given time interval."
  (cl-assert (and buffer (get-buffer buffer)) nil "Error: :buffer must be defined")
  (with-current-buffer buffer
    (save-excursion
      (let ((re (concat "^\\(\\*+[ \t]*.*\\)\\|^[ \t]*"
                        org-clock-string
                        "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
            (counter 0)
            (tmphd "BEFORE FIRST HEADING")
            (tstart (org-time-string-to-seconds tstart-str))
            (tend (org-time-string-to-seconds tend-str))
            (limit (or limit (point-max)))
            headings timelst
            lvl title result ts te)
        (goto-char (point-min))
        (cl-block myblock
          (while (re-search-forward re nil t)
            (cond
              ;; found a org heading
              ((match-end 1)
               (if (> (length timelst) 0)
                   (setq result (nconc result (list (list
                                                     (copy-sequence headings)
                                                     timelst)))))
               (setq tmphd (org-heading-components)
                     lvl (car tmphd)
                     title (nth 4 tmphd)
                     timelst nil)
               ;; maintain a list of the current heading hierarchy
               (cond
                 ((> lvl (length headings))
                  (setq headings  (nconc headings `(,title))))
                 ((= lvl (length headings))
                  (setf (nth (1- lvl) headings) title))
                 ((< lvl (length headings))
                  (setq headings (cl-subseq headings 0 lvl))
                  (setf (nth (1- lvl) headings) title))))
              ;; found a clock line with 2 timestamps
              ((match-end 3)
               (setq ts (save-match-data (org-time-string-to-seconds
                                          (match-string-no-properties 2)))
                     te (save-match-data (org-time-string-to-seconds
                                          (match-string-no-properties 3))))
               ;; the clock lines progress from newest to oldest. This
               ;; enables skipping the rest if this condition is true
               (if (> tstart te)
                   (if (re-search-forward "^\\(\\*+[ \t]*.*\\)" nil t)
                       (beginning-of-line)
                       (goto-char (point-max)))
                   (when (> tend ts)
                     (setq timelst (nconc timelst (list
                                                   (list (match-string-no-properties 2)
                                                         (match-string-no-properties 3)))))))))
            (when (>= (point) limit)
              (cl-return-from myblock))))
        (if (> (length timelst) 0)
            (setq result (nconc result (list (list (copy-sequence headings)
                                                   timelst)))))
        result))))

(defun dfeich/org-slice-tr (tstart-str tend-str cutstart-str cutend-str)
  "Return time slice of a time range in minutes."
  (let ((tstart (org-time-string-to-seconds tstart-str))
        (tend (org-time-string-to-seconds tend-str))
        (cutstart (if (stringp cutstart-str)
                      (org-time-string-to-seconds cutstart-str)
                      cutstart-str))
        (cutend (if (stringp cutend-str)
                    (org-time-string-to-seconds cutend-str)
                    cutend-str))
        result)
    (setq result (max 0
                      (/  (- (min tend cutend) (max tstart cutstart))
                          60)))))

(defun dfeich/org-clock-hourly-report (struct tstart-str tend-str)
  "Return a structure containing a per hour report within an interval."
  (let* ((tstart (org-time-string-to-seconds tstart-str))
         (tend (org-time-string-to-seconds tend-str))
         (delta 3600)
         (intvls (cl-loop for tm from tstart to (- tend delta) by delta
                          collect `(,tm ,(+ tm delta))))
         result)
    ;; iterate over the intervals for the final table
    (cl-loop for iv in intvls
             collect (list
                      iv
                      (let* ((cutstart (car iv))
                             (cutend (cadr iv))
                             (tmsum 0.0)
                             headings trlst)
                        ;; iterate over the task structure
                        (cl-loop
                         for item in struct
                         do (progn
                              (setq headings (car item)
                                    trlst (cadr item)
                                    ;; sum up the parts of the time
                                    ;; ranges falling into this
                                    ;; interval
                                    tmsum (apply
                                           #'+
                                           (mapcar
                                            (lambda (tr)
                                              (dfeich/org-slice-tr (car tr)
                                                                   (cadr tr)
                                                                   cutstart
                                                                   cutend))
                                            trlst))))
                         if (> tmsum 0) collect `(,headings ,tmsum) into lst
                         finally return lst))))))

(defun org-dblock-write:nagora-report (params)
  "Fill in a dynamic timesheet reporting block."
  (let* ((buffer (plist-get params :buffer))
         (day (symbol-name (plist-get params :day)))
         (tstart (if (string-match-p "^[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}$" day)
                     day
                     (error "Error: day format must be in YYYY-mm-dd format")))
         (tend (concat day " 23:59"))
         (table (dfeich/org-clock-hourly-report
                 (dfeich/org-clock-get-tr-for-ivl buffer tstart tend)
                 tstart tend)))
    (insert (format "#+CAPTION: timesheet for day %s\n" day))
    (insert "|Time|Customer| Task |Minutes|\n|------\n")
    (cl-loop
     for item in table
     do (let ((ivl (car item))
              (entries (cadr item)))
          (cl-loop for e in entries
                   do (let ((headings (car e))
                            (minutes (cadr e)))
                        (insert (concat
                                 "|"
                                 (format-time-string "%H:%M" (seconds-to-time
                                                              (car ivl)))
                                 "-"
                                 (format-time-string "%H:%M" (seconds-to-time
                                                              (cadr ivl)))
                                 "|" (nth 1 headings)
                                 "|" (car (last headings))
                                 "|" (format "%d" minutes)
                                 "|\n"))))))
    (insert "|----\n|TOTAL||||\n#+TBLFM: @>$>=vsum(@I..@II)")
    (search-backward "Time")
    (org-table-align)
    (org-table-recalculate '(16))))


(provide 'org-nagora-report)
;;; org-nagora-report.el ends here
