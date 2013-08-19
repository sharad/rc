;;
;; office.el
;; Login : <spratap@spratap>
;; Started on  Wed Dec  1 17:11:05 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;



;; (define-minor-mode office-mode
;;     "Prepare for working with collarative office project."
;;   :initial-value nil
;;   :lighter " Office"
;;   :global nil
;;   (cond
;;     (office-mode
;;      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
;;      )
;;     (t
;;      (add-hook 'before-save-hook 'delete-trailing-whitespace t t))))


;; This is the mode to be enabled when I am working in some files on
;; which other peoples are also working.





(define-minor-mode office-mode
    "Prepare for working with collarative office project."
  :initial-value nil
  :lighter " Office"
  :global nil
  (condition-case e
      (when office-mode
        (message "calling office mode")
        (set (make-local-variable 'before-save-hook) before-save-hook)
        (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
        (message "called office mode"))
    (error (message "Error: %s" e))))

(defvar taskdir "/home/s/paradise/tasks" "Task Directory")
(defvar current-task nil "Current task")

(defvar task-alist '(("bugs" (files "todo.org" "notes.org" "an0.org"))
                     ("features"
                      (files "reqirement.org" "feasibility.org"
                             "design.org" "todo.org" "notes.org" "an0.org"))))
(defvar task-file-properties '((buffer-read-only . t)
                               (fill-column . 172))
  "Task file properties.")

(defun find-task-dir (&optional force)
  (interactive "P")
  (if (or force (null current-task))
      (setq current-task (ido-read-directory-name "dir: " taskdir nil t))
      current-task))



(defun create-task (task name &optional desc)
  (interactive
   (let* ((task (completing-read "what: " (mapcar 'car task-alist) nil t))
          (name (completing-read "name: " (directory-files (concat taskdir "/" task "/")) nil))
          (bug (if (string-equal task "bugs")
                   (car
                    (bugzilla-get-bugs
                     '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                     `(("ids" ,name))))))
          (desc (if bug
                    (cdr (assoc "summary" bug))
                    (read-from-minibuffer (format "Desc of %s: " name)))))
     (list task name desc)))

  (let* ((dir (concat taskdir "/" task "/" name)))
    (if (file-directory-p dir)
        (find-task dir)
        (progn

          (make-directory (concat dir "/logs") t)
          (make-directory (concat "/home/s/hell/SCRATCH/bugs/" task) t)
          (make-symbolic-link (concat "/home/s/hell/SCRATCH/bugs/" task) (concat dir "/scratch"))

          (dolist (f (cdr (assoc 'files (cdr (assoc task task-alist)))))
            (let ((nfile (expand-file-name f (concat dir "/")))
                  find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
              (with-current-buffer (or (find-buffer-visiting nfile)
                                       (find-file-noselect nfile))
                (if (goto-char (point-min))
                    (insert "# -*-  -*-\n"))
                (dolist (pv task-file-properties)
                  (add-file-local-variable-prop-line (car pv) (cdr pv)))
                (goto-char (point-max))
                (insert (format "\n\n* %s %s: %s\n\n\n\n" (capitalize task) name desc))
                (set-buffer-file-coding-system
                 (if (coding-system-p 'utf-8-emacs)
                     'utf-8-emacs
                     'emacs-mule))
                (write-file nfile))))

          ;; Planner
          (let ((plan-page (planner-read-non-date-page (planner-file-alist))))
            (if (string-equal task "bugs")
                (planner-bugzilla-create-bug-to-task bug plan-page t)
                (planner-create-task
                 (format "%s: %s" name desc)
                 (let ((planner-expand-name-favor-future-p
                        (or planner-expand-name-favor-future-p
                            planner-task-dates-favor-future-p)))
                   (planner-read-date))
                 nil plan-page
                 (task-status-of-sys 'planner 'inprogress))))
          ;; Project-Buffer
          ()

          (find-file (expand-file-name
                      (cadr (assoc 'files (cdr (assoc task task-alist))))
                      (concat dir "/")))))

    (if (y-or-n-p (format "Should set %s current task" dir))
        (setq current-task dir))))

(defun find-task (task)
  (interactive
   (let ((task (ido-read-directory-name "dir: " taskdir nil t)))
     (list task)))
  (let ((buf (format "*task %s*"
                     (file-name-nondirectory
                      (substring (expand-file-name (concat task "/"))
                                 0 -1)))))
    (with-current-buffer (get-buffer-create buf)
      (dolist (f (directory-files task t "/*.org$"))
        (insert-file-contents f))
      (not-modified)
      (setq default-directory task)
      (View-exit-and-edit)
      (make-local-variable 'view-read-only)
      (make-local-variable 'buffer-read-only)
      (setq view-read-only t
            buffer-read-only t)
      (org-mode)
      (goto-char (point-min))
      (org-cycle t))
    (switch-to-buffer buf)
    (if (y-or-n-p (format "Should set %s current task" task))
        (setq current-task task))))


(deh-section "Forgive"

  (defun forgive/them ()
    (interactive)
    (if (and
         (featurep 'develock)
         (assq major-mode develock-keywords-alist))
        (develock-mode -1))
    (highlight-changes-visible-mode -1)))


(provide 'office-config)

