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
        (if (or (eq major-mode 'c-mode)
                (eq major-mode 'c++-mode))
            (c-set-style "stroustrup" 1))
        (set (make-local-variable 'before-save-hook) before-save-hook)
        (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
        (message "called office mode"))
    (error (message "Error: %s" e))))

(defvar taskdir "/home/s/paradise/tasks" "Task Directory")
(defvar task-local-path "/~s/tasks" "task-local-path")



(defvar *taskdir-current-task* nil "Current task")
(add-to-list
 'desktop-globals-to-save
 '*taskdir-current-task*)
(add-to-list
 'session-globals-include
 '(*taskdir-current-task* 100))

(defvar task-config '(("bug"
                       (files "todo.org" "notes.org" "an0.org")
                       (dirs "logs" "programs" "patches" "deliverables")
                       (links ("notes.html" . "index.html"))
                       (project "/susengg-01:releases/projects/bugs.pb"))
                      ("feature"
                       (files "reqirement.org" "feasibility.org"
                        "design.org" "todo.org" "notes.org" "an0.org")
                       (dirs "logs" "programs" "patches" "deliverables")
                       (links ("notes.html" . "index.html"))
                       (project "/susengg-01:releases/projects/features-dev.pb"))))

(defvar task-file-properties '((buffer-read-only . t)
                               (fill-column . 172))
  "Task file properties.")

(defun find-task-dir (&optional force)
  (interactive "P")
  (if (or force (null *taskdir-current-task*))
      (setq *taskdir-current-task* (ido-read-directory-name "dir: " taskdir nil t))
      *taskdir-current-task*))

(defun create-plan-task (dir task name desc)
  (let* ((plan-page (planner-read-non-date-page (planner-file-alist)))
         (hname
          (if task-local-path
              (format "[[%s][b%s]]:" (concat task-local-path "/" (pluralize-string task) "/" name) name)
              (format "b%s:" name))))
    ;; (read-from-minibuffer (format "task-description %s: " task-description)
    ;;                       task-description)
    (if (and (string-equal task "bug")
             bug)
        (flet ((my-formattor (id summary url)
                 (format "[[%s][b%s]]: %s %s"
                         (concat task-local-path "/" (pluralize-string task) "/" (number-to-string id))
                         (number-to-string id) summary (concat "[[" url "][url]]"))))
          (let ((planner-bugz-formattor #'my-formattor))
            (planner-bugzilla-create-bug-to-task bug plan-page t)))
        (let ((task-description
               (cond

                 ((string-equal task "bug")
                  (format "%s %s %s"
                          hname desc (concat "[[" (read-from-minibuffer (format "url for bug %s: " name)) "][url]]")))

                 ((string-equal task "feature") (format "%s %s" hname desc))

                 (t (error "task is not bound.")))))
          (planner-create-task
           task-description
           (let ((planner-expand-name-favor-future-p
                  (or planner-expand-name-favor-future-p
                      planner-task-dates-favor-future-p)))
             (planner-read-date))
           nil plan-page
           (task-status-of-sys 'planner 'inprogress))))))

(defun create-task-dir (dir task name desc)
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (make-directory dir t)
  (make-directory (concat "/home/s/hell/SCRATCH/" (pluralize-string task) "/" name) t)
  (make-symbolic-link (concat "/home/s/hell/SCRATCH/" (pluralize-string task) "/" name) (concat dir "/scratch"))

  ;; files
  (dolist (f (cdr (assoc 'files (cdr (assoc task task-config)))))
    (let ((nfile (expand-file-name f (concat dir "/")))
          find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
      (with-current-buffer (or (find-buffer-visiting nfile)
                               (find-file-noselect nfile))
        ;; (if (goto-char (point-min))
        ;;     (insert "# -*-  -*-\n"))
        (dolist (pv task-file-properties)
          (add-file-local-variable-prop-line (car pv) (cdr pv)))
        (goto-char (point-max))
        (insert (format "\n\n* %s - %s: %s\n\n\n\n" (capitalize task) name desc))
        (set-buffer-file-coding-system
         (if (coding-system-p 'utf-8-emacs)
             'utf-8-emacs
             'emacs-mule))
        (write-file nfile))))
  ;; links
  (dolist (lp (cdr (assoc 'links (cdr (assoc task task-config)))))
    (make-symbolic-link
     (car lp) ;; (expand-file-name (car lp) (concat dir "/"))
     (expand-file-name (cdr lp) (concat dir "/"))))
  ;; dirs
  (dolist (dname (cdr (assoc 'dirs (cdr (assoc task task-config)))))
    (make-directory (concat dir "/" dname) t)))

(defun create-task (task name &optional desc)
  (interactive
   (let* ((task (completing-read "what: " (mapcar 'car task-config) nil t))
          (name (completing-read "name: " (directory-files (concat taskdir "/" (pluralize-string task) "/")) nil))
          (bug (if (string-equal task "bug")
                   (car
                    (condition-case e
                         (bugzilla-get-bugs
                          '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                          `(("ids" ,name)))
                      ('error (progn (message "bugzilla some problem is there.") nil))))))
          (desc (if bug
                    (cdr (assoc "summary" bug))
                    (read-from-minibuffer (format "Desc of %s: " name)))))
     (list task name desc)))

  (let* ((dir (concat taskdir "/" (pluralize-string task) "/" name))
         (bug (if (string-equal task "bug")
                  (car
                    (condition-case e
                         (bugzilla-get-bugs
                          '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                          `(("ids" ,name)))
                      ('error (progn (message "bugzilla some problem is there.") nil))))))
         (desc (or desc (if bug (read-from-minibuffer (format "Desc of %s: " name))))))
    (if (file-directory-p dir)
        (find-task dir)
        (progn

          ;; Planner
          (create-plan-task dir task name desc)
          ;; Project-Buffer
          (when nil
            (iproject-add-project
             nil                          ;project-type
             nil                          ;project-main-file
             nil                          ;project-root-folder
             project-name                 ;project-name
             nil)                         ;file-filter
            )
          ;; create task dir
          (create-task-dir dir task name desc)
          (find-file (expand-file-name
                      (cadr (assoc 'files (cdr (assoc task task-config))))
                      (concat dir "/")))))

    (if (y-or-n-p (format "Should set %s current task" dir))
        (setq *taskdir-current-task* dir))))

(defun delete-task-dir (dir task name desc)
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (make-directory dir t)
  (make-directory (concat "/home/s/hell/SCRATCH/" (pluralize-string task) "/" name) t)
  (make-symbolic-link (concat "/home/s/hell/SCRATCH/" (pluralize-string task) "/" name) (concat dir "/scratch"))

  ;; files
  (dolist (f (cdr (assoc 'files (cdr (assoc task task-config)))))
    (let ((nfile (expand-file-name f (concat dir "/")))
          find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
      (with-current-buffer (or (find-buffer-visiting nfile)
                               (find-file-noselect nfile))
        ;; (if (goto-char (point-min))
        ;;     (insert "# -*-  -*-\n"))
        (dolist (pv task-file-properties)
          (add-file-local-variable-prop-line (car pv) (cdr pv)))
        (goto-char (point-max))
        (insert (format "\n\n* %s - %s: %s\n\n\n\n" (capitalize task) name desc))
        (set-buffer-file-coding-system
         (if (coding-system-p 'utf-8-emacs)
             'utf-8-emacs
             'emacs-mule))
        (write-file nfile))))
  ;; links
  (dolist (lp (cdr (assoc 'links (cdr (assoc task task-config)))))
    (make-symbolic-link
     (car lp) ;; (expand-file-name (car lp) (concat dir "/"))
     (expand-file-name f (concat (cdr lp) "/"))))
  ;; dirs
  (dolist (dname (cdr (assoc 'dirs (cdr (assoc task task-config)))))
    (make-directory (concat dir "/" dname) t)))

(defun delete-task (task name &optional desc)
  (interactive
   (let* ((task (completing-read "what: " (mapcar 'car task-config) nil t))
          (name (completing-read "name: " (directory-files (concat taskdir "/" (pluralize-string task) "/")) nil))
          (bug (if (string-equal task "bug")
                   (car
                    (condition-case e
                         (bugzilla-get-bugs
                          '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                          `(("ids" ,name)))
                      ('error (progn (message "bugzilla some problem is there.") nil))))))
          (desc (if bug
                    (cdr (assoc "summary" bug))
                    (read-from-minibuffer (format "Desc of %s: " name)))))
     (list task name desc)))

  (let* ((dir (concat taskdir "/" (pluralize-string task) "/" name))
         (bug (if (string-equal task "bug")
                  (car
                    (condition-case e
                         (bugzilla-get-bugs
                          '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                          `(("ids" ,name)))
                      ('error (progn (message "bugzilla some problem is there.") nil))))))
         (desc (or desc (if bug (read-from-minibuffer (format "Desc of %s: " name))))))
    (if (file-directory-p dir)
        (find-task dir)
        (progn

          ;; Planner
          (let ((plan-page (planner-read-non-date-page (planner-file-alist))))
            (if (and (string-equal task "bug")
                     bug)
                (planner-bugzilla-create-bug-to-task bug plan-page t)
                (planner-create-task
                 (cond ((string-equal task "bug")
                        (if task-local-path
                            (format "[[%s/%s][b%s]] %s %s"
                                    task-local-path (concat (pluralize-string task) "/" name) name desc (concat "[[" (read-from-minibuffer (format "url for bug %s" name)) "][url]]"))
                            (format "b%s %s %s" name desc (concat "[[" (read-from-minibuffer (format "url for bug %s: " name)) "][url]]"))))
                       ((string-equal task "feature")
                        (if task-local-path
                            (format "[[%s/%s][%s]]: %s" task-local-path (concat (pluralize-string task) "/" name) name desc)
                            (format "%s: %s" name desc)))
                       (t (error "task is not bound.")))
                 (let ((planner-expand-name-favor-future-p
                        (or planner-expand-name-favor-future-p
                            planner-task-dates-favor-future-p)))
                   (planner-read-date))
                 nil plan-page
                 (task-status-of-sys 'planner 'inprogress))))
          ;; Project-Buffer
          (when nil
            (iproject-add-project
             nil                          ;project-type
             nil                          ;project-main-file
             nil                          ;project-root-folder
             project-name                 ;project-name
             nil)                         ;file-filter
            )
          ;; create task dir
          (create-task-dir dir task name desc)
          (find-file (expand-file-name
                      (cadr (assoc 'files (cdr (assoc task task-config))))
                      (concat dir "/")))))

    (if (y-or-n-p (format "Should set %s current task" dir))
        (setq *taskdir-current-task* dir))))

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
        (setq *taskdir-current-task* task))))

(deh-section "Forgive"
  (defun forgive/them ()
    (interactive)
    (if (and
         (featurep 'develock)
         (assq major-mode develock-keywords-alist))
        (develock-mode -1))
    (highlight-changes-visible-mode -1)))


(provide 'office-config)
