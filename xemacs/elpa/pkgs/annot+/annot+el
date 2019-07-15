;;; annot+.el --- Annot Plus                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
;; Keywords: convenience, extensions, files, data, help

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

;; * TODOs
;; ** Add back link or button or both and command to follow it.
;; ** Option to add at righ margin
;; ** Proper book keepign for outside file changes and survival
;; ** Support for export import as comment (may be selectively.)
;; ** Annotation location (file and heading.) is stored in buffer local variable
;;    May be per annotation location support could be tried to be implemented.
;; * DONE

;;; Code:

(provide 'annot+)


(require 'annot)
(require 'org-annotate-file)


;; * `annot+-edit/add'  - either edit the annotation at point, if there is,
;;                       or else add a new annotation or highlight.
;; * `annot+-remove'    - remove the annotation/highlight at point.
;; * `annot+-add'       - add a new annotation/highlight at point.
;; * `annot+-edit'      - edit the annotation at point.
;; * `annot+-add-image' - insert an image at point.


(defvar annot-location nil "Associated annot location file and heading")

(defun annot+-add (&optional text/image/region)
  ())

(defun org-annotate-file-annotate (filename line)
  "Add annotation for FILENAME at LINE using current buffer."
  (let* ((link (org-make-link-string (concat "file:" filename) filename))
         (search-link (org-make-link-string
                       (concat "file:" filename "::" line)
                       (org-annotate-file-ellipsify-desc line))))
    (unless (eq major-mode 'org-mode)
      (org-mode))
    (goto-char (point-min))
    (widen)
    (when org-annotate-file-always-open
      (show-all))
    (unless (search-forward-regexp
             (concat "^* " (regexp-quote link)) nil t)
      (org-annotate-file-add-upper-level link))
    (beginning-of-line)
    (org-narrow-to-subtree)
    ;; deal with a '::' search if need be
    (when org-annotate-file-add-search
      (unless (search-forward-regexp
               (concat "^** " (regexp-quote search-link)) nil t)
        (org-annotate-file-add-second-level search-link)))))


;;; annot+.el ends here
