;;; occ-resolve-clock.el --- Occ resolve clock       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

(provide 'occ-resolve-clock)

(defun occ-rl-clock-p (clock))
(defun occ-rl-clock-clock-in (clock &optional resume start-time))
(defun occ-rl-clock-out (&optional switch-to-state fail-quietly at-time))
(defun occrl-clock-clock-out (clock &optional fail-quietly at-time))
(defun occ-rl-select-other-clock (&optional target))
(defun occ-rl-capture+-helm-templates-alist ())

;;; occ-resolve-clock.el ends here
