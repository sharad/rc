;;; debug-info.el --- debug informations             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'debug-info)


(require 'notify)

;; https://www.emacswiki.org/emacs/EmacsMemoryDebugging
;; https://www.reddit.com/r/emacs/comments/aol1oz/what_are_some_good_strategies_to_reduce_emacs/
;; https://lists.gnu.org/archive/html/help-gnu-emacs/2004-09/msg00342.html
;; http://davin.50webs.com/research/2010/diagnose.el.html

cons-cells-consed
floats-consed
vector-cells-consed
symbols-consed
string-chars-consed
misc-objects-consed
intervals-consed
strings-consed


(assoc 'pmem (process-attributes (emacs-pid)))

(/ (* (cdr (assoc 'vsize (process-attributes (emacs-pid)))) 100.000)
   (+ (car (memory-info)) (nth 3 (memory-info))))

(/ (* (cdr (assoc 'vsize (process-attributes (emacs-pid)))) 100.000)
   (car (memory-info)))


(defun process-percentage-mem-usage (pid)
  (let ((attrib (process-attributes pid)))
    (when attrib
      (/ (* (cdr (assoc 'rss attrib)) 100.000)
         (car (memory-info))))))

(defvar process-monitor-mem-usage-alist nil)
(defvar process-monitor-mem-usage-timer-alist nil)
(defun process-check-mem-usage-diff (pid)
  (interactive)
  (let ((attribs (process-attributes pid)))
    (if attribs
        (let ((old-mem  (or (cdr (assoc pid process-monitor-mem-usage-alist)) 0))
              (new-mem  (or (process-percentage-mem-usage pid) 0))
              (pgm-name (or (cdr (assoc 'comm attribs)) "anonymous")))
          (unless (= old-mem new-mem)
            (let ((msg (format "pid %d memory usage %s by (%f - %f) = %f"
                               pid
                               (if (< old-mem new-mem) "increased" "decreaded")
                               new-mem old-mem
                               (- new-mem old-mem))))
              (if (assoc pid process-monitor-mem-usage-alist)
                  (setf (cdr (assoc pid process-monitor-mem-usage-alist)) new-mem)
                (push (cons pid new-mem) process-monitor-mem-usage-alist))
              (notify (format "memory usage of %s[%d]" pgm-name pid)
                      msg))))
      (process-monitor-memory-usage-stop pid))))
(defun process-monitor-memory-usage-stop (pid)
  (interactive)
  (when (assoc pid process-monitor-mem-usage-timer-alist)
    (cancel-timer
     (cdr (assoc pid process-monitor-mem-usage-timer-alist)))
    (setq process-monitor-mem-usage-timer-alist
          (remove (assoc pid process-monitor-mem-usage-timer-alist) process-monitor-mem-usage-timer-alist))
    (setq process-monitor-mem-usage-alist
          (remove (assoc pid process-monitor-mem-usage-alist) process-monitor-mem-usage-alist))))
(defun process-monitor-memory-usage (pid)
  (interactive)
  (if (process-attributes pid)
      (unless (assoc pid process-monitor-mem-usage-timer-alist)
        (let ((timer (run-with-timer 7 7 #'process-check-mem-usage-diff pid)))
          (push (cons pid timer) process-monitor-mem-usage-timer-alist)))
    (process-monitor-memory-usage-stop pid)))

;;; debug-info.el ends here
