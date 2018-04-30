;;; occ-api-common.el --- occ-api               -*- lexical-binding: t; -*-
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

;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; "org tasks accss common api"
    ;; (defvar org-)
(defvar occ-verbose 0)

(defclass occ-obj ()
  )
(defclass occ-task (occ-obj)
  )
(defclass occ-context (occ-obj)
  )
(defclass occ-context-task (occ-obj)
  )


(defun occ-message (level &rest args)
  "If LEVEL is lower than `gnus-verbose' print ARGS using `message'.

Guideline for numbers:
1 - error messages, 3 - non-serious error messages, 5 - messages for things
that take a long time, 7 - not very important messages on stuff, 9 - messages
inside loops."
  (if (<= level occ-verbose)
      (let (
            ;; (message
            ;;  (if gnus-add-timestamp-to-message
            ;;      (apply 'gnus-message-with-timestamp args)
            ;;      (apply 'message args)))
            )
        ;; (when (and (consp gnus-action-message-log)
        ;;            (<= level 3))
        ;;   (push message gnus-action-message-log))
        (progn
          ;; not doing anything
          ))
      ;; We have to do this format thingy here even if the result isn't
      ;; shown - the return value has to be the same as the return value
      ;; from `message'.
      (apply 'format args)))

;; (defun org-task-collect-task ()
(defun occ-collect-task ()
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task (cadr (org-element-at-point))))
      (when heading
        ;; (if root   (push (cons "Root" root) task))
        (if marker    (occ-task-set-property task :task-clock-marker marker))
        (if file      (occ-task-set-property task :task-clock-file file))
        (if point     (occ-task-set-property task :task-clock-point point))
        (if heading   (occ-task-set-property task :task-clock-heading heading))
        (if heading-prop (occ-task-set-property task :task-clock-heading-prop heading-prop))
        (if clock-sum (occ-task-set-property task :task-clock-clock-sum clock-sum))
        (let ((inherited-props (occ-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-task-get-property task prop)
                (occ-task-set-property task prop val))))))
      task)))

;; (defun org-task-collect-task-clock-info ()
(defun occ-collect-task-from-clock ()
  ;; NOT used anywhere
  ;; (org-element-at-point)
  (let ((heading-with-string-prop
         (unless (org-before-first-heading-p)
           (org-get-heading 'notags))))
    (let ((heading (if heading-with-string-prop
                       (substring-no-properties heading-with-string-prop)))
          (heading-prop (if heading-with-string-prop
                            heading-with-string-prop))
          (marker  (move-marker
                    (make-marker)
                    (point)
                    (org-base-buffer (current-buffer))))
          (file    (buffer-file-name))
          (point   (point))
          (clock-sum (if (org-before-first-heading-p)
                         0
                         (org-clock-sum-current-item)))
          (task (cadr (org-element-at-point)))
          (task-content-start ))
      (when heading
        ;; (if root   (push (cons "Root" root) task))
        (if marker    (occ-task-set-property task :task-clock-marker marker))
        (if file      (occ-task-set-property task :task-clock-file file))
        (if point     (occ-task-set-property task :task-clock-point point))
        (if heading   (occ-task-set-property task :task-clock-heading heading))
        (if heading-prop   (occ-task-set-property task :task-clock-heading-prop heading-prop))
        (if clock-sum (occ-task-set-property task :task-clock-clock-sum clock-sum))
        (let ((inherited-props (occ-keys-with-operation :getter nil)))
          (dolist (prop inherited-props)
            (let* ((propstr (if (keywordp prop) (substring (symbol-name prop) 1) (symbol-name prop)))
                   (val (org-entry-get nil propstr t)))
              (unless (occ-task-get-property task prop)
                (occ-task-set-property task prop val)))))
        (if heading-with-string-prop
            (occ-task-set-property task :task-clock-content (occ-heading-content-only))))
      task)))

;; (defun org-Xheading-content-only ()
(defun occ-heading-content-only ()
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


(defun occ-fontify-like-in-org-mode (task)
  (let* ((level   (or (occ-task-get-property task :level) 0))
         (heading (occ-task-get-property task :task-clock-heading-prop))
         (prefix  (concat (make-string level ?\*) " ")))
    (if nil ;; if test without else with prefix
        (substring
         (org-fontify-like-in-org-mode
          (concat prefix heading)
          org-odd-levels-only)
         (1+ level))

        (org-fontify-like-in-org-mode
         (concat prefix heading)
         org-odd-levels-only))))

;; (defun org-Xclock-items (&optional tstart tend)
(defun occ-clock-items (&optional tstart tend)
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
                (let ((clock (org-element-at-point)))))))))))

;; (defun org-Xtask-get-property (task property)
(defun occ-task-get-property (task property)
  (plist-get task property))

;; (defun org-Xtask-set-property (task property value)
(defun occ-task-set-property (task property value)
  (plist-put task property value))

(defun occ-tasks-associated-to-context (context)
  ;; (funcall occ-api-tasks-associated-to-context context)
  (funcall occ-matching-tasks context))

;; (defun occ-markers-associated-to-context (context)
(defun occ-markers-associated-to-context (context)
  (mapcar #'(lambda (e)
              (occ-task-get-property e :task-clock-marker))
          (occ-tasks-associated-to-context context)))

;; ;; (defun occ-markers-associated-to-context (context)
;; (defun occ-rankmarkers-associated-to-context (context)
;;   (mapcar #'(lambda (e)
;;               (cons
;;                (car e)
;;                (occ-task-get-property (cdr e) :task-clock-marker)))
;;           (occ-ranktasks-associated-to-context context)))

;; Dynamic plist task format
;; plist of :rank :marker :task etc

(defun occ-build-dyntaskpl (task context)
  (funcall occ-build-dyntaskpl task context))

(defun occ-dyntaskpl-get-task (dyntaskpl)
  (plist-get dyntaskpl :task))

(defun occ-dyntaskpl-get-marker (dyntaskpl)
  (let ((task (plist-get dyntaskpl :task)))
    (plist-get task :task-clock-marker)))

(defun occ-task-get-marker (task)
  (plist-get task :task-clock-marker))

(defun occ-dyntaskpls-associated-to-context (context)
  (funcall occ-matching-dyntaskpls context))

(defun occ-dyntaskpls-associated-to-context-filtered (context)
  ;; TODO Here do variance based filtering.
  (let* ((dyntaskpls (funcall occ-matching-dyntaskpls context))
         (rankslist  (mapcar #'(lambda (dyntaskpl) (plist-get dyntaskpl :rank))
                             dyntaskpls))
         (avgrank    (/
                      (reduce #'+ rankslist)
                      (length rankslist)))
         (varirank   (sqrt
                      (/
                       (reduce #'+
                               (mapcar #'(lambda (rank) (expt (- rank avgrank) 2)) rankslist))
                       (length rankslist)))))
    (remove-if-not
     #'(lambda (dyntaskpl)
         (>= (plist-get dyntaskpl :rank) avgrank))
     dyntaskpls)))

(defun occ-task-get-heading (task)
  (occ-task-get-property task :task-clock-heading))

(defun occ-dyntaskpl-print (dyntaskpl heading)
  (funcall occ-api-dyntaskpl-print dyntaskpl heading))

(provide 'occ-api-common)
;;; occ-api-common.el ends here
