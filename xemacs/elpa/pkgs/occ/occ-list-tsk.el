;;; occ-list-tsk.el --- list tsk                     -*- lexical-binding: t; -*-

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

(provide 'occ-list-tsk)


(require 'occ-ctor)


(defun occ-list-tsk-build (&optional
                           file
                           tsk-builder)
  "Build recursive org tsks from org FILE (or current buffer) using TSK-BUILDER function e.g. occ-collect-tsk"
  (let ((tsk-builder   (or tsk-builder #'occ-make-tsk-at-point)))
    (with-current-buffer (if file
                             (find-file-noselect file)
                           (current-buffer))
      (if file (goto-char (point-min)))
      (cons entry
            (org-map-entries tsk-builder t file)))))

;;; occ-list-tsk.el ends here
