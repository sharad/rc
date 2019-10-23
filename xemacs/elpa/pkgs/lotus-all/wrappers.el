;;; wrappers.el --- wrapper

;; Copyright (C) 2013  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:lisp

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

;; emacs24 server-execute

(if (< emacs-major-version 24)
    (defun custom-display-graphic-p ()
      (eq (frame-parameter (selected-frame) 'window-system) 'x))
    (defun custom-display-graphic-p ()
      (display-graphic-p)))

(provide 'wrappers)
;;; wrappers.el ends here
