;;; task-manager.el --- task-manager               -*- lexical-binding: t; -*-

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


(require org)
;; (require 'publishing)
(require 'org-publishing)
(require 'muse-publishing)
(require 'iproject)
(require 'project-buffer-file)


(defvar *task-desc-file-name* ".task-desc" "*task-desc-file-name*")

(defvar *task-party-base-dir*
  (org-publish-get-attribute "tasks" "org" :base-directory)
  "Task Party Directory")

(defvar task-scratch-dir "~/SCRATCH/" "task scratch directory")

(defvar task-parties
  '(("meru"
     (org-master-file "report.org")
     (org-heading     "Office related work")
     (bugz-url        "https://bugzilla.merunetworks.com"))
    ("personal"
     (org-master-file "report.org")
     (org-heading     "Office related work")
     (bugz-url        "https://bugzilla.merunetworks.com"))))

(defvar task-current-party "meru")

(defvar task-file-properties '((buffer-read-only . t)
                               (fill-column . 172))
  "Task file properties.")

(defvar task-org-headers
  '("#+CATEGORY: Work"
    "#+STARTUP: overview"
    "#+STARTUP: hidestars"
    "#+TAGS: PERFORCE(4)  BUGZILLA(b) SVN(v) SCMBUG(m) PROJECT(j)"
    "#+TAGS: CVS(i) PHONE(p) INTERNET(i)"
    "#+SEQ_TODO: TODO DONE")
  "Desc")

(defvar *task-projbuffs-base-dir* (expand-file-name "contents/misc/projbuffs" *created-content-dir*))

(defvar task-config '(("bug"
                       (org-master-file "report.org")
                       (org-files       "todo.org" "notes.org" "analysis.org")
                       (org-todo-file    "todo.org")
                       (dirs            "logs" "programs" "patches" "deliverables")
                       (links           ("notes.html" . "index.html"))
                       (project         "bugs.pb")
                       (name            "[0-9]+")) ;
                      ("feature"
                       (org-master-file "report.org")
                       (org-files       "reqirement.org" "feasibility.org" "design.org" "todo.org" "notes.org" "analysis.org")
                       (org-todo-file    "todo.org")
                       (dirs            "logs" "programs" "patches" "deliverables")
                       (links           ("notes.html" . "index.html"))
                       (project         "features.pb"))
                      ("work"
                       (org-master-file "report.org")
                       (org-files       "reqirement.org" "feasibility.org" "design.org" "todo.org" "notes.org" "analysis.org")
                       (org-todo-file    "todo.org")
                       (dirs            "logs" "programs" "patches" "deliverables")
                       (links           ("notes.html" . "index.html"))
                       (project         "works.pb"))))

(defvar *taskdir-current-task* nil "Current task")

(add-to-list
 'desktop-globals-to-save
 '*taskdir-current-task*)
(add-to-list
 'session-globals-include
 '(*taskdir-current-task* 100))























(defmacro task-create-org-file (file &rest body)
  `(progn
     (let ((find-file-not-found-functions nil))
       (with-current-buffer (or (find-buffer-visiting ,file)
                                (find-file-noselect ,file))
         (with-writable-buffer
             ;; (if (goto-char (point-min))
             ;;     (insert "# -*-  -*-\n"))
             (dolist (pv task-file-properties)
               (add-file-local-variable-prop-line (car pv) (cdr pv)))
           (goto-char (point-max))
           (insert (reduce '(lambda (a b) (concat a "\n" b)) task-org-headers))
           (goto-char (point-max))

           ,@body

           (set-buffer-file-coding-system
            (if (coding-system-p 'utf-8-emacs)
                'utf-8-emacs
                'emacs-mule))
           (write-file ,file)
           (org-html-export-to-html))))
     (kill-buffer (find-buffer-visiting ,file))))
(put 'task-create-org-file 'lisp-indent-function 1)

;;;#autoload
(defun task-select-party-dir ()
  (interactive)
  (ido-read-directory-name "dir: " *task-party-base-dir* nil t))

;;;#autoload
(defun find-task-dir (&optional force)
  (interactive "P")
  (if (or
       force
       (null *taskdir-current-task*))
      (setq *taskdir-current-task*
            (ido-read-directory-name "dir: " (task-party-dir) nil t))
      *taskdir-current-task*))

(defun task-select-task-type (prompt)
  (ido-completing-read prompt (mapcar 'car task-config) nil t))

;;
(defun task-party-dir (&optional party)
  "Task Directory"
  (let ((party (or party task-current-party)))
    (if (member party (mapcar 'car task-parties))
        (expand-file-name party
                          (org-publish-get-attribute "tasks" "org" :base-directory))
        (error "party is not from task-parties"))))

(defun task-party-org-heading (&optional party)
  (let ((party (or party task-current-party)))
    (if (member party (mapcar 'car task-parties))
        (cadr
         (assoc 'org-heading
                (cdr (assoc party task-parties))))
        (error "party is not from task-parties"))))

(defun task-party-org-master-file (&optional party)
  (let* ((party                (or party task-current-party))
         (org-master-file      (cadr
                                (assoc 'org-master-file
                                       (cdr (assoc party task-parties)))))
         (org-master-file-path (expand-file-name
                                org-master-file
                                (task-party-dir party))))
    (if (member party (mapcar 'car task-parties))
        (progn
          (unless (file-exists-p org-master-file-path)
            (let ((nfile org-master-file-path)) ;find alternate of find-file-noselect to get non-existing file.
              (task-create-org-file nfile
                                    (insert "\n\n")
                                    (insert (format "* %s - %s: %s\n\n\n" (capitalize "party") (capitalize party) "tasks."))
                                    (insert (format "* Reports\n\n\n"))
                                    (insert (format "* %s\n" (task-party-org-heading party))))))

          org-master-file)
        (error "party is not from task-parties"))))

(defun task-party-bugz-url (&optional party)
  (let ((party (or party task-current-party)))
    (if (member party (mapcar 'car task-parties))
        (cadr
         (assoc 'bugz-url
                (cdr (assoc party task-parties))))
        (error "party is not from task-parties"))))

(defun task-party-url-base (&optional party)
  "task-party-url-base"
  (let ((party (or party task-current-party)))
    (if (member party (mapcar 'car task-parties))
        (concat "/~s/tasks/" party)
        (error "party is not from task-parties"))))

(defun task-projbuffs-dir (&optional party)
  (let ((party (or party task-current-party)))
    (if (member party (mapcar 'car task-parties))
        (expand-file-name party *task-projbuffs-base-dir*)
        (error "party is not from task-parties"))))

;;
(defun task-org-master-file (task)
  (if (member task (mapcar 'car task-config))
      (cdr (assoc 'org-master-file (cdr (assoc task task-config))))
      (error "task is not from task-config")))

(defun task-first-org-master-file (task)
  (if (member task (mapcar 'car task-config))
      (cadr (assoc 'org-master-file (cdr (assoc task task-config))))
      (error "task is not from task-config")))

(defun task-org-todo-file (task)
  (if (member task (mapcar 'car task-config))
      (cadr (assoc 'org-todo-file (cdr (assoc task task-config))))
      (error "task is not from task-config")))

(defun task-org-files (task)
  (if (member task (mapcar 'car task-config))
      (cdr (assoc 'org-files (cdr (assoc task task-config))))
      (error "task is not from task-config")))

;;
(defun task-get-task-name (prompt party task &optional new)
  (let* ((party (or party task-current-party))
         (task-name (completing-read
                     prompt
                     (unless new (directory-files (concat (task-party-dir party) "/" (pluralize-string task) "/")))
                     nil
                     (not new))))
    (if new
        (let* ((predicate (cadr (assoc 'name (cdr (assoc task task-config)))))
               (task-name-match
                (cond
                  ((stringp predicate)
                   (string-match predicate task-name))
                  ((functionp predicate)
                   (funcall predicate task-name))
                  ((null predicate)
                   (> (length task-name) 0)))))
          (if task-name-match
              (if (file-exists-p (task-get-task-dir party task task-name))
                  (error "task %s name %s already exists." task task-name)
                  task-name)
              (error "task %s name %s not matching to %s" task task-name predicate)))
        task-name)))

(defun task-get-task-desc (party task name &optional new)
  (if new
      (let* ((bug (if (string-equal task "bug")
                      (car
                       (condition-case e
                           (bugzilla-get-bugs
                            '("id" "summary" "short_desc" "status" "bug_status" "_bugz-url")
                            `(("ids" ,name))
                            (task-party-bugz-url party))
                         ('error (progn (message "bugzilla some problem is there.") nil))))))
             (desc (if bug
                       (cdr (assoc "summary" bug))
                       (read-from-minibuffer (format "Desc of %s: " name)))))
        desc)
      (let* ((task-dir       (task-get-task-dir task-current-party task name))
             (task-desc-file (expand-file-name task-dir *task-desc-file-name*)))
        (if (file-exists-p task-desc-file)
            (sharad/read-file task-desc-file)
            (task-get-task-desc party task name t)))))

(defun task-get-task-dir (party task name)
  (concat (task-party-dir party) "/" (pluralize-string task) "/" name))

(defun task-get-task-data (&optional new)
  (let* ((task                (task-select-task-type "what: "))
         (name                (task-get-task-name "name: " task-current-party task new))
         (desc                (task-get-task-desc task task-current-party name))
         (task-dir            (task-get-task-dir task-current-party task name)))
    (list task name desc task-dir)))

(defun task-get-task-data-all (&optional new)
  (let* ((task-data (task-get-task-data new))
         (project-type        (iproject-choose-project-type))
         (project-main-file   (iproject-choose-main-file project-type))
         (project-root-folder (iproject-choose-root-folder project-main-file))
         (project-file-filter (iproject-choose-file-filter))
         (doc-file-filter     '(custom ("\\.\\(?:org\\)$"))) ;; (iproject-choose-file-filter)
         (doc-base-virtual-folder (let* ((def-string "doc"))
                                    (read-from-minibuffer
                                     (format "Enter the base directory in the project%s: " def-string)
                                     def-string))))
    (append task-data (list project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder))))

;;
(defun task-get-planner-description (task name desc)
  (cl-flet ((my-formattor (id summary url)
                          ;; BUG: w f b
                          (format "[[%s][%c%s]]: %s %s"
                                  (concat (task-party-url-base) "/" (pluralize-string task) "/" (number-to-string id))
                                  (aref task 0)
                                  (number-to-string id) summary (concat "[[" url "][url]]"))))
    (let* ((planner-bugz-formattor #'my-formattor)
           (hname
            (if (task-party-url-base)
                ;; BUG: w f b
                (format "[[%s][%c%s]]:" (concat (task-party-url-base) "/" (pluralize-string task) "/" name) (aref task 0) name)
                (format "%c%s:" (aref task 0) name)))
           (task-description
            (cond
              ((string-equal task "bug")     (planner-bugzilla-bug-to-task-name name (task-party-bugz-url)))
              ((string-equal task "feature") (format "%s %s" hname desc))
              ((string-equal task "work")    (format "%s %s" hname desc))
              (t                             (error "task is not bound.")))))
      task-description)))

(defun task-get-links-text (task formatterfn)
  (let ((description ""))
    (dolist
        (f (task-org-files task) description)
      (setq description
            (concat
             description
             ;; "\n - "
             (funcall formatterfn f))))))

(defun task-get-org-description (task name desc)
  (cl-flet ((my-formattor (id summary url)
                          ;; BUG: w f b
                          (format
                           "[[%s][%s - %s]]: %s %s"
                           (concat (task-party-url-base) "/" (pluralize-string task) "/" (number-to-string id))
                           (pluralize-string task)
                           (number-to-string id)
                           summary
                           (concat "[[" url "][url]]"))))
    (let* ((planner-bugz-formattor #'my-formattor)
           (hname
            (if (task-party-url-base)
                ;; BUG: w f b
                ;; (format "[[%s][%s - %s]]:" (concat (task-party-url-base) "/" (pluralize-string task) "/" name) (capitalize task) name)
                (format "%s - %s:" (capitalize task) name)
                (format "%s - %s:" (capitalize task) name)))
           (task-description
            (cond
              ((string-equal task "bug")     (planner-bugzilla-bug-to-task-name name (task-party-bugz-url)))
              ((string-equal task "feature") (format "%s %s" hname desc))
              ((string-equal task "work")    (format "%s %s" hname desc))
              (t                             (error "task is not bound."))))
           (description ""))

      (setq
       description
       (concat
        description
        task-description
        (format "\n  [[file:%s/%s/%s][dir]]"
                (pluralize-string task)
                name
                (task-first-org-master-file task))
        (task-get-links-text task
                             '(lambda (file)
                               (format "\n - [[file:%s/%s/%s][%s]]"
                                (pluralize-string task)
                                name
                                file
                                (capitalize (file-name-sans-extension file)))))
        (concat description (format "\nend %s\n" task)))))))

;;;#autoload
(defun create-plan-task (task name desc task-dir)
  (interactive (task-get-task-data t))
  (let* ((plan-page
          (planner-read-non-date-page (planner-file-alist))))

    (planner-create-task
     (task-get-planner-description task name desc)
     (let ((planner-expand-name-favor-future-p
            (or planner-expand-name-favor-future-p
                planner-task-dates-favor-future-p)))
       (planner-read-date))
     nil
     plan-page
     (task-status-of-sys 'planner 'inprogress))
    t))

;;;#autoload
(defun delete-plan-task (task name desc task-dir)
  (interactive (task-get-task-data))
  (message "Not doing anything."))

;;;#autoload
(defun create-org-task (task name desc task-dir project-main-file project-root-folder)
  (interactive
   (let* ((task-data (task-get-task-data t))
          (project-root-folder (iproject-choose-root-folder project-main-file)))
     (append
      task-data
      (list project-main-file project-root-folder))))
  (let ((file    (expand-file-name (task-party-org-master-file) (task-party-dir)))
        (heading (task-party-org-heading))
        (child-heading (task-get-org-description task name desc)))
    (org-with-file-headline file heading
                            (org-insert-heading-to-file-headline
                             child-heading
                             file
                             (capitalize (pluralize-string task))))
    (org-with-file-headline file child-heading
                            (let ((buffer-read-only nil))
                              (org-entry-put nil "SubtreeFile"
                                             (format "%s/%s/%s"
                                                     (pluralize-string task)
                                                     name
                                                     (task-first-org-master-file task))

                                             ;; (file-relative-name
                                             ;;  (concat task-dir "/" (task-first-org-master-file task))
                                             ;;  (file-name-directory file))
                                             )
                              ;; (org-entry-put nil "Root" project-root-folder)
                              ))
    (with-current-buffer (find-file-noselect file)
      (let ((buffer-read-only nil))
        (save-buffer)))
    t))

;;;#autoload
(defun delete-org-task (task name desc task-dir)
  (interactive (task-get-task-data))
  (let ()
    (org-remove-heading-from-file-headline
     (task-get-org-description task name desc)
     (expand-file-name (task-party-org-master-file) (task-party-dir))
     (task-party-org-heading))
    t))

;; Project-Buffer
;;;#autoload
(defun create-pbm-task (task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
  (interactive (task-get-task-data-all t))
  (let* ((project-name (concat task ":" name " - " desc)))
    (with-project-buffer (find-file-noselect
                          (expand-file-name
                           (concat (pluralize-string task) ".pb") (task-projbuffs-dir)))
      (iproject-add-project
       project-type                 ;project-type
       project-main-file            ;project-main-file
       project-root-folder          ;project-root-folder
       project-name                 ;project-name
       project-file-filter)         ;file-filter
      (project-buffer-set-master-project-no-status project-name)
      (let ((root-folder task-dir))
        (iproject-add-files-to-project
         project-name ;; (project-buffer-get-master-project)
         root-folder
         doc-file-filter
         doc-base-virtual-folder)))))

;; (defun delete-pbm-task (task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
;;;#autoload
(defun delete-pbm-task (task name desc task-dir)
  (interactive (task-get-task-data-all))
  (message "Not doing anything."))

;;;#autoload
(defun create-task-dir (task name desc task-dir project-root-folder)
  (interactive (task-get-task-data t))
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (let* ()
    (make-directory task-dir t)
    (sharad/write-file (expand-file-name *task-desc-file-name* task-dir) desc)
    (make-directory (concat task-scratch-dir (pluralize-string task) "/" name) t)
    (make-symbolic-link (concat task-scratch-dir (pluralize-string task) "/" name) (concat task-dir "/scratch"))

    (let ((org-heading (format "%s - %s: %s" (capitalize task) name desc)))
      ;; files
      (dolist (file
                (cons
                 (task-org-todo-file task)
                 (task-org-files task)))
        (when file
          (let ((nfile (expand-file-name file task-dir))
                (heading (format
                          "%s %s"
                          (capitalize (file-name-sans-extension file))
                          org-heading))) ;find alternate of find-file-noselect to get non-existing file.
            (message "file %s" nfile)
            (task-create-org-file nfile
                                  (insert (format "\n\n* %s\n\n\n\n" heading))
                                  (org-with-file-headline nil heading
                                                          (org-entry-put nil "Root" project-root-folder))))))

      (let ((file (task-first-org-master-file task)))
        (when file

          (let ((nfile (expand-file-name file task-dir))
                (heading (format "%s %s"
                                 (capitalize (file-name-sans-extension file))
                                 org-heading))) ;find alternate of find-file-noselect to get non-existing file.
            (task-create-org-file nfile
                                  (insert (concat
                                           (format "\n\n* %s\n" heading)
                                           (task-get-links-text task
                                                                ;; '(lambda (f)
                                                                ;;   (format "[[file:%s][%s]]" f (capitalize (file-name-sans-extension f))))
                                                                '(lambda (f)
                                                                  (format "** %s\n- [[file:%s][%s]]\n"
                                                                   (capitalize (file-name-sans-extension f))
                                                                   f
                                                                   "here"))))))

            (dolist (sfile
                      (cons
                       (task-org-todo-file task)
                       (task-org-files task)))
              (org-with-file-headline nfile (capitalize (file-name-sans-extension sfile))
                                      (with-writable-buffer
                                          (message "adding property %s in %s" sfile nfile)
                                        (org-entry-put nil "SubtreeFile" sfile)))))))

      ;; links
      (dolist (lp (cdr (assoc 'links (cdr (assoc task task-config)))))
        (make-symbolic-link
         (car lp) ;; (expand-file-name (car lp) (concat task-dir "/"))
         (expand-file-name (cdr lp) (concat task-dir "/"))))
      ;; dirs
      (dolist
          (dname
            (cdr (assoc 'dirs (cdr (assoc task task-config))))
           t)
        (make-directory (concat task-dir "/" dname) t)))))

;;;#autoload
(defun delete-task-dir (task name desc task-dir)
  (interactive (task-get-task-data))
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (delete-directory task-dir t))

;;;#autoload
(defun create-task (task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
  (interactive (task-get-task-data-all t))
  (let* ()
    (if (file-directory-p task-dir)
        (find-task task-dir)
        (progn
          (create-plan-task task name desc task-dir)
          (create-org-task  task name desc task-dir project-main-file project-root-folder)
          (create-task-dir  task name desc task-dir project-root-folder)
          (create-pbm-task  task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
          (org-clocking-entry-update-task-infos t)))

    (when (y-or-n-p (format "Should set %s current task" task-dir))
      (setq *taskdir-current-task* task-dir)
      (find-file (expand-file-name (task-first-org-master-file task) task-dir)))))

;;;#autoload
(defun delete-task (task name desc task-dir)
  (interactive (task-get-task-data))
  (let* ()
    (progn
      (delete-plan-task task name desc task-dir)
      (delete-org-task  task name desc task-dir)
      (delete-task-dir  task name desc task-dir)
      ;; (delete-pbm-task  task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
      (delete-pbm-task  task name desc task-dir))))

;;;#autoload
(defun find-task (task)
  (interactive
   (let ((task (ido-read-directory-name "dir: " (task-party-dir) nil t)))
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



;;(progn ;; org file management

(require 'file-utils)

(defun org-task-files (&optional party)
  (let ((party (or party task-current-party)))
    (directory-files-recursive
     (task-party-dir party) "\\.org$" 7)))

(defun org-all-task-files ()
  (let ()
    (directory-files-recursive
     (org-publish-get-attribute "tasks" "org" :base-directory)
     "\\.org$" 7)))

(defun org-task-refile-target (party)
  (let* ((party (or party task-current-party))
         (task-files (org-task-files party)))
                                        ;all files returned by `org-task-files'
    `((,task-files :maxlevel . 3))))

;;;#autoload
(defun org-clock-in-refile-task (party)
  (interactive
   (list (ido-completing-read
          "Seletc Party: "
          (mapcar 'car task-parties)
          nil
          t
          task-current-party)))
  (org-clock-in-refile (org-task-refile-target party)))
;; )



  (when nil
    (defun org-clock-select-task-from-clocks (clocks &optional prompt)
      "Select a task that was recently associated with clocking."
      (interactive)
      (let (och chl sel-list rpl (i 0) s)
        ;; Remove successive dups from the clock history to consider
        (mapc (lambda (c) (if (not (equal c (car och))) (push c och)))
              clocks)
        (setq och (reverse och) chl (length och))
        (if (zerop chl)
            (user-error "No recent clock")
            (save-window-excursion
              (org-switch-to-buffer-other-window
               (get-buffer-create "*Clock Task Select*"))
              (erase-buffer)
              ;; (when (marker-buffer org-clock-default-task)
              ;;   (insert (org-add-props "Default Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?d org-clock-default-task))
              ;;   (push s sel-list))
              ;; (when (marker-buffer org-clock-interrupted-task)
              ;;   (insert (org-add-props "The task interrupted by starting the last one\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?i org-clock-interrupted-task))
              ;;   (push s sel-list))
              ;; (when (org-clocking-p)
              ;;   (insert (org-add-props "Current Clocking Task\n" nil 'face 'bold))
              ;;   (setq s (org-clock-insert-selection-line ?c org-clock-marker))
              ;;   (push s sel-list))
              (insert (org-add-props "Recent Tasks\n" nil 'face 'bold))
              (mapc
               (lambda (m)
                 (when (marker-buffer m)
                   (setq i (1+ i)
                         s (org-clock-insert-selection-line
                            (if (< i 10)
                                (+ i ?0)
                                (+ i (- ?A 10))) m))
                   (if (fboundp 'int-to-char) (setf (car s) (int-to-char (car s))))
                   (push s sel-list)))
               och)
              (run-hooks 'org-clock-before-select-task-hook)
              (goto-char (point-min))
              ;; Set min-height relatively to circumvent a possible but in
              ;; `fit-window-to-buffer'
              (fit-window-to-buffer nil nil (if (< chl 10) chl (+ 5 chl)))
              (message (or prompt "Select task for clocking:"))
              (setq cursor-type nil rpl (read-char-exclusive))
              (kill-buffer)
              (cond
                ((eq rpl ?q) nil)
                ((eq rpl ?x) nil)
                ((assoc rpl sel-list) (cdr (assoc rpl sel-list)))
                (t (user-error "Invalid task choice %c" rpl)))))))

    ;;     (defun org-clock-insert-selection-line (i marker)
    ;;       "Insert a line for the clock selection menu.
    ;; And return a cons cell with the selection character integer and the marker
    ;; pointing to it."
    ;;       (when (marker-buffer marker)
    ;;         (let (file cat task heading prefix)
    ;;           (with-current-buffer (org-base-buffer (marker-buffer marker))
    ;;             (save-excursion
    ;;               (save-restriction
    ;;                 (widen)
    ;;                 (ignore-errors
    ;;                   (goto-char marker)
    ;;                   (setq file (buffer-file-name (marker-buffer marker))
    ;;                         cat (org-get-category)
    ;;                         heading (org-get-heading 'notags)
    ;;                         prefix (save-excursion
    ;;                                  (org-back-to-heading t)
    ;;                                  (looking-at org-outline-regexp)
    ;;                                  (match-string 0))
    ;;                         task (substring
    ;;                               (org-fontify-like-in-org-mode
    ;;                                (concat prefix heading)
    ;;                                org-odd-levels-only)
    ;;                               (length prefix)))))))
    ;;           (when (and cat task)
    ;;             (insert (format "[%c] %-12s  %s\n" i cat task))
    ;;             (cons i marker)))))
    )

(provide 'task-manager)
;;; task-manager.el ends here
