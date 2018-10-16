;;; debug-config.el --- session setting

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

;;

;;; Code:





(defun keyboard-debug ()
  "Jump ito debugger
During execution of Lisp code, this character causes a quit directly.
At top-level, as an editor command, this simply beeps."
  (interactive)
  (deactivate-mark)
  (let ((debug-on-error t))
   (if (fboundp 'kmacro-keyboard-quit)
       (kmacro-keyboard-quit))
   (setq defining-kbd-macro nil)
   ;; (error "User throw error")
   (debug)))

(defvar debug-tags-level-list  nil "Debug message tag and level assoc list")

(defun dmessage (tag &optional level fmt &rest args)
  (let ((level (or level 0))
        (tagl (find tag debug-tags-level-list
                    :test (lambda (e tag)
                            (or (eq tag e) (eq (car tag) e))))))
    (if (and tagl
             (if (consp tagl)
                 (<= level (cadr tagl))
                 (<= level 0)))
        (apply 'message fmt args))))

(defvar inhibit-debugger-default inhibit-debugger)
(defvar debug-ignored-errors-default debug-ignored-errors)


(defun enable-backtrace-for-error ()
  (interactive)
  (when debug-ignored-errors
   (setq
    inhibit-debugger-default inhibit-debugger
    debug-ignored-errors-default debug-ignored-errors)
   (setq
    inhibit-debugger nil
    debug-ignored-errors nil)))

(defun reset-backtrace-for-error ()
  (interactive)
  (when debug-ignored-errors-default
    (setq
     inhibit-debugger inhibit-debugger-default
     debug-ignored-errors debug-ignored-errors-default)
    (setq
     inhibit-debugger-default nil
     debug-ignored-errors-default nil)))


(provide 'debug-config)

;;; debug-config.el ends here
