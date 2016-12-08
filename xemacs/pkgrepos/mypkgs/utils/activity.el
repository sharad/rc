;;; activity.el --- Emacs Activity logger, analyzer and reporter  -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d@gmail.com>
;; Keywords: data

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

;; This package meant to log, analyze and report all emacs activity of
;; user which could further utilized to visualize activity of user
;; during period of time or editing session.


;;; Code:


(define-minor-mode activity-mode
      "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
    :initial-value nil
    :lighter " Act"
    :global t
    (condition-case e
        (when office-mode
          (message "calling office mode")
          (if (or (eq major-mode 'c-mode)
                  (eq major-mode 'c++-mode))
              (c-set-style "stroustrup" 1))
          (set (make-local-variable 'before-save-hook) before-save-hook)
          (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
          (message "called office mode"))
      (error (message "Error: %s" e))))


(provide 'activity)
;;; activity.el ends here
