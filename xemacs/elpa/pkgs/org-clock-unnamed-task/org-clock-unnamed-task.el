;;; org-clock-unnamed-task.el --- org-context-clock-api               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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


;;;{{{ Emacs tasks https://emacs.stackexchange.com/questions/29128/programmatically-setting-an-org-mode-heading
;; https://emacs.stackexchange.com/questions/21713/how-to-get-property-values-from-org-file-headers

;; BUG org-global-get-property and org-global-put-property are not defined.


(defvar *lotus-org-unnamed-task-file*        "~/Unnamed.org")
(defvar *lotus-org-unnamed-parent-task-name* "Unnamed tasks")
(defvar *lotus-org-unnamed-task-name-fmt*    "Unnamed task %d")
(defvar *lotus-org-unnamed-task-clock-marker* nil)
(defvar lotus-org-unnamed-task-org-clock-persist nil "Control org-clock-persist at time of unnamed task clock-in")
(defvar lotus-org-unnamed-task-org-clock-auto-clock-resolution nil "Control occ-org-clock-auto-clock-resolution at time of unnamed task clock-in")

;;;###autoload
(defun lotus-org-unnamed-task-straight-org-clock-clock-in (clock &optional resume start-time)
  (let ((org-clock-persist               lotus-org-unnamed-task-org-clock-persist)
        (org-clock-auto-clock-resolution lotus-org-unnamed-task-org-clock-auto-clock-resolution))
    (lotus-org-clock-load-only)
    (prog1
        (org-clock-clock-in clock resume start-time)
      (setq org-clock-loaded t))))

;;;###autoload
(defun lotus-org-unnamed-task-file (&optional file)
  (if file
      (setq *lotus-org-unnamed-task-file* file)
    *lotus-org-unnamed-task-file*))

;;;###autoload
(defun lotus-org-unnamed-parent-task-name (&optional name)
  (if name
      (setq *lotus-org-unnamed-parent-task-name* name)
      *lotus-org-unnamed-parent-task-name*))

;;;###autoload
(defun lotus-org-unnamed-task-name-fmt (&optional fmt)
  (if fmt
      (setq *lotus-org-unnamed-task-name-fmt* fmt)
      *lotus-org-unnamed-task-name-fmt*))

;;;###autoload
(defun lotus-org-unnamed-task-clock-marker (&optional marker)
  (if marker
      (setq *lotus-org-unnamed-task-clock-marker* marker)
      *lotus-org-unnamed-task-clock-marker*))

;;;###autoload
(defun lotus-org-get-incr-tasknum (&optional buffer)
  (with-current-buffer (or buffer (current-buffer))
    (let* ((tasknumstr (or (org-global-get-property "TASKNUM") "0"))
           (tasknum (string-to-number tasknumstr)))
      (org-global-put-property "TASKNUM" (number-to-string (1+ tasknum)))
      tasknum)))

;;;###autoload
(defun lotus-org-create-unnamed-task (&optional file task)
  "return newly created subtask and marker (suntask . marker)"
  (interactive
   (let ((file *lotus-org-unnamed-task-file*)
         (task *lotus-org-unnamed-parent-task-name*))
     (list file task)))

  (org-without-org-clock-persist
   (let* ((file (or file *lotus-org-unnamed-task-file*))
          (task (or task *lotus-org-unnamed-parent-task-name*))
          (subtask (format *lotus-org-unnamed-task-name-fmt*
                           (1+
                            (with-current-buffer (find-file-noselect file)
                              (org-with-heading-pos pos task
                                (if pos
                                    (org-number-of-subheadings)
                                  0)))))))
     ;; (assert file)
     (org-find-file-heading-marker file task t)
     (let ((marker (org-insert-subheadline-to-file-headline
                    subtask
                    file
                    task
                    t)))
       (unless (markerp marker)
         (error "No marker %s returned" marker))
       (with-current-buffer (marker-buffer marker)
         (goto-char marker)
         (org-with-inhibit-modification-hooks
           (org-entry-put nil "Effort" "10")))
       (cons subtask marker)))))

(when nil
  (progn
    (org-find-file-heading-marker *lotus-org-unnamed-task-file* *lotus-org-unnamed-parent-task-name* t)

    (org-insert-subheadline-to-file-headline
     (format *lotus-org-unnamed-task-name-fmt*
             (1+ (org-with-file-headline *lotus-org-unnamed-task-file* *lotus-org-unnamed-parent-task-name* (org-number-of-subheadings))))
     *lotus-org-unnamed-task-file*
     *lotus-org-unnamed-parent-task-name*
     t)
    )



  (let ((marker (point-marker)))
    (unless (markerp marker)
      (error "No marker %s returned" marker)))

  (lotus-org-create-unnamed-task)
  )

;;;###autoload
(defun lotus-org-create-unnamed-task-task-clock-in (&optional file parent-task)
  (interactive
   (let ((file *lotus-org-unnamed-task-file*)
         (parent-task *lotus-org-unnamed-parent-task-name*))
     (list file parent-task)))
  (let ((file (or file *lotus-org-unnamed-task-file*))
        (parent-task (or parent-task *lotus-org-unnamed-parent-task-name*)))
    (org-with-file-headline
     file
     (lotus-org-create-unnamed-task file parent-task)
     (org-entry-put nil "Effort" "10")
     ;; BUG: It is clocking to last unfinished task
     (lotus-org-unnamed-task-straight-org-clock-clock-in (list (lotus-org-unnamed-task-clock-marker (mark-marker))))
     ;; (org-clock-in)
     ;; (lotus-org-unnamed-task-clock-marker (mark-marker))
     )))

(when nil

  (lotus-org-create-unnamed-task
   *lotus-org-unnamed-task-file*
   *lotus-org-unnamed-parent-task-name*)

  (lotus-org-create-unnamed-task-task-clock-in
   *lotus-org-unnamed-task-file*
   *lotus-org-unnamed-parent-task-name*)

  )
;;;###autoload
(defun lotus-org-merge-unnamed-task-at-point ()
  (interactive)
  ;; Implement
  )

;;;###autoload
(defun org-clock-marker-is-unnamed-clock-p (&optional clock)
  (let ((clock (or clock org-clock-marker)))
    (and
     clock
     *lotus-org-unnamed-task-clock-marker*
     (equal
      (marker-buffer org-clock-marker)
      (marker-buffer *lotus-org-unnamed-task-clock-marker*)))))

;;;###autoload
(defun lotus-org-unnamed-task-at-point-p ()
  (save-restriction
   (save-excursion
    (org-back-to-heading t)
    (let ((element (org-element-at-point)))
      (if (and
           element
           (eq (car element) 'headline))
          (let (;; (begin (plist-get (cadr element) :begin))
                ;; (level (plist-get (cadr element) :level))
                (title (plist-get (cadr element) :title)))
            (string-match-p "Unnamed task [0-9]+" title)))))))

;; (lotus-org-create-unnamed-task "~/Unnamed.org" "Unnamed tasks")
;;;###autoload
(defun org-clock-make-child-task-and-clock-in ()
  ;; TODO
  "Implement"
  (interactive)
  (if org-clock-marker
      (org-with-narrow-to-marker org-clock-marker
        (org-insert-subheading-at-point
         (read-from-minibuffer "Subtask: "))
        (org-entry-put nil "Effort" "10")
        (lotus-org-unnamed-task-straight-org-clock-clock-in (list (point-marker))))))

(defun lotus-org-clockin-last-time (min)
  )

(provide 'org-clock-unnamed-task)
;;; org-clock-unnamed-task.el ends here
