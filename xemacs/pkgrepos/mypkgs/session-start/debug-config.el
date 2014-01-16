;;; debug-config.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <>
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
        (tagcons (assoc tag debug-tags-level-list)))
    (if (and tagcons
             (<= level (cadr tagcons)))
        (apply 'message fmt args))))





(provide 'debug-config)

;;; debug-config.el ends here



