;;; config.el --- config                             -*- lexical-binding: t; -*-

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

   ;; http://gnuvola.org/software/personal-elisp/dist/lisp/diversions/emacs-uptime.el
   (require 'cl)

   (defvar *emacs-start-time* nil "emacs-start-time")

   (setq
    *emacs-start-time* *emacs-load-start*)

   (defun emacs-uptime (&optional start-time)
     "Gives Emacs' uptime, based on global var `*emacs-start-time*'."
     (interactive)
     (let ((st (or start-time *emacs-start-time*)))                ; set in do-it-now.el
       (if st
           (let* ((cur (current-time))
                  (hi-diff (- (car cur) (car st)))
                  (tot-sec (+ (ash hi-diff 16) (- (cadr cur) (cadr st))))
                  (days (/ tot-sec (* 60 60 24)))
                  (hrs  (/ (- tot-sec (* days 60 60 24)) (* 60 60)))
                  (mins (/ (- tot-sec (* days 60 60 24) (* hrs 60 60)) 60))
                  (secs (/ (- tot-sec (* days 60 60 24) (* hrs 60 60) (* mins 60)) 1)))
             (message "Up %dd %dh %dm %ds (%s), %d buffers, %d files"
                      days hrs mins secs
                      (format-time-string "%a %Y-%m-%d %T" st)
                      (length (buffer-list))
                      (count t (buffer-list)
                             :test-not
                             (lambda (ignore buf)
                               (null (cdr (assoc 'buffer-file-truename
                                                 (buffer-local-variables buf)))))))))))

   (add-hook 'after-make-frame-functions '(lambda (f) (run-at-time "1 sec" nil 'emacs-uptime)) t)

;; (provide 'config)
;;; config.el ends here
