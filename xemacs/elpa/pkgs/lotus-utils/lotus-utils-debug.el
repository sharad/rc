;;; lotus-utils-debug.el --- utils debug             -*- lexical-binding: t; -*-

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

(provide 'lotus-utils-debug)


(defvar lotus-utils-debug nil)

;;;###autoload
(defun lotus-utils-enable-debug ()
  (interactive)
  (setq lotus-utils-debug t))
;;;###autoload
(defun lotus-utils-disable-debug ()
  (interactive)
  (setq lotus-utils-debug nil))
(defun lotus-utils-debug (type level &rest args)
  (when lotus-utils-debug
    (when (car args)
      (apply #'format args)
      (when (member level '(:emergency :error :warning :debug))
        ;; (apply #'lwarn 'lotus-utils level args)
        (apply #'lwarn type level args))
      (unless (eq level :nodisplay)
        (apply #'message args)))))

(defun lotus-utils-message (&rest args)
  (apply #'message args)
  (apply #'lotus-utils-debug :debug args))

;;; lotus-utils-debug.el ends here
