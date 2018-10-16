;;; read-utils.el --- misc utils

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: lisp

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

;; misc utilities

;;; Code:

;;{{ Read from minibuffer


(defun read-list-from-minibuffer (prompt &optional inittext km)
  (flet ((value nil)
         (read-string-or-null (prompt inittext km)
           (let ((retval (read-from-minibuffer prompt inittext km)))
             (if (not (string-equal retval ""))
                 retval))))
    (loop until (not (setq value (read-string-or-null prompt inittext km)))
       collect value)))

(defun trynextmethod ()
  (interactive)
  (throw 'goforlist
    (progn
      (move-beginning-of-line nil)
      (setq mbstr (buffer-substring (point) (point-max)))
      (catch 'exit (exit-minibuffer))
      nil)))

(defun condread (fn inittext)
  (let ((km (copy-keymap minibuffer-local-map)))
    (define-key km (kbd "C-v") 'trynextmethod)
    (if (functionp fn)
        (funcall fn inittext km)
        (read-from-minibuffer fn inittext km))))

(defun read-from-minibuffer-fns (&rest fns)
  (let (mbstr
        retval
        (wfns fns))
    (while (null (setq retval (catch 'goforlist (condread (car wfns) mbstr))))
      (setq wfns (or (cdr wfns) fns)))
    retval))


;;}}


(provide 'read-utils)
;;; read-utils.el ends here
