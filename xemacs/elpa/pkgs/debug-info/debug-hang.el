;;; debug-hang.el --- debug hang                     -*- lexical-binding: t; -*-

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

(provide 'debug-hang)


(defvar fast-helm-test-timer nil)

(defun fast-helm-test ()
  (helm-timed 2 nil
    (let* ((prompt (format "fast edit - (recursion-depth): %d" (recursion-depth)))
           (source (helm-build-sync-source prompt :candidates '(a b c))))
      (helm
       :sources (list source))
      (message "(recursion-depth): %d" (recursion-depth)))))

(defun fast-helm-test-stop-timer ()
  (interactive)
  (when fast-helm-test-timer
    (cancel-timer fast-helm-test-timer)
    (setq fast-helm-test-timer nil)))

(defun fast-helm-test-start-timer ()
  (interactive)
  (fast-helm-test-stop-timer)
  (setq fast-helm-test-timer (run-with-timer 2 2 'fast-helm-test)))

;;; debug-hang.el ends here
