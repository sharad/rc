;;; org-context-clocking-api-common.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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



;; "org entries accss common api"
    ;; (defvar org-)

(defun org-entry-collect-task-info ()
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task-info (cadr (org-element-at-point))))
      (when heading
        ;; (if root   (push (cons "Root" root) task-info))
        (if marker    (org-entry-task-info-set-property task-info :task-clock-marker marker))
        (if file      (org-entry-task-info-set-property task-info :task-clock-file file))
        (if point     (org-entry-task-info-set-property task-info :task-clock-point point))
        (if heading   (org-entry-task-info-set-property task-info :task-clock-heading heading))
        (if clock-sum (org-entry-task-info-set-property task-info :task-clock-clock-sum clock-sum)))
      task-info)))

(defun org-entry-collect-task-clock-info ()
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task-info (cadr (org-element-at-point)))
          (task-content-start ))
      (when heading
        ;; (if root   (push (cons "Root" root) task-info))
        (if marker    (org-entry-task-info-set-property task-info :task-clock-marker marker))
        (if file      (org-entry-task-info-set-property task-info :task-clock-file file))
        (if point     (org-entry-task-info-set-property task-info :task-clock-point point))
        (if heading   (org-entry-task-info-set-property task-info :task-clock-heading heading))
        (if clock-sum (org-entry-task-info-set-property task-info :task-clock-clock-sum clock-sum))
        (if heading-with-string-prop
            (org-entry-task-info-set-property task-info :task-clock-content (org-heading-content-only))))
      task-info)))

(defun org-heading-content-only ()
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((start (progn
                         (goto-char (org-element-property :contents-begin (org-element-at-point)))
                         (while (org-at-drawer-p)
                           (goto-char (org-element-property :end (org-element-at-point))))
                         ;; (if (org-at-heading-p) (backward-char))
                         (point))))
            (unless (org-at-heading-p)
              (progn
                (outline-next-heading)
                ;; (outline-next-visible-heading 1)
                (backward-char)
                (buffer-substring start (point)))))))))

    ;; (let ((re org-clock-string))
    ;;   (re-search-backward re nil t))

(defun org-clock-items (&optional tstart tend)
  "Return time, clocked on current item in total."
  (if (org-at-heading-p)
      (save-excursion
        (save-restriction
          (let ((ele (org-element-at-point))
                (re org-clock-string))
            (let ((start (org-element-property :contents-begin ele))
                  (end (progn
                         (outline-next-heading)
                         ;; (org-next-visible-heading 1)
                         (point))))
              (narrow-to-region start end)
              (goto-char (point-max))
              (while (re-search-backward re nil t)
                (let ((clock (org-element-at-point)))
                  ))))))))

(defun org-entry-task-info-get-property (task-info property)
  (plist-get task-info property))

(defun org-entry-task-info-set-property (task-info property value)
  (plist-put task-info property value))

(defun org-markers-associated-to-file (file)
  (mapcar '(lambda (e)
            (org-entry-task-info-get-property e :task-clock-marker))
          (funcall org-context-clocking-api-entries-associated-to-file file)))





(progn                                  ;; general use function

  (defun org-entry-task-info-get-heading (task-info)
    (org-entry-task-info-get-property task-info :task-clock-heading)))

(provide 'org-context-clocking-api-common)
;;; org-context-clocking-api-common.el ends here
