;;; task-manager.el --- task-manager               -*- lexical-binding: t; -*-

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


(require 'org)
;; (require 'publishing)
;; (require 'org-publishing)
;; (require 'muse-publishing)
(require 'iproject)
(require 'project-buffer-file)
(require 'file-utils)
(require 'inflections) ;; for pluralize-string

(defvar *task-desc-file-name* ".task-desc" "*task-desc-file-name*")

(defvar *task-party-base-org-master-file* "start.org")
(defvar *task-party-base-dir*
  "~/Documents/org/tasks"
  "Task Party Directory")

(defvar *task-url-path* "/~s/tasks/" "task url path")

(defvar task-scratch-dir "~/SCRATCH/" "task scratch directory")

(defvar task-parties
  nil
  "tasks parties
 example
  '((\"office\"
    (org-master-file \"report.org\")
    (org-heading     \"Office related work\")
    (bugz-url        \"https://bugzilla.merunetworks.com\"))
   (\"personal\"
    (org-master-file \"report.org\")
    (org-heading     \"Personal work\")
    (bugz-url        \"https://bugzilla.merunetworks.com\")))"
    )

(defvar task-current-party nil "Task current party")
(defvar task-current-party-change-hook nil "run hook when task-current-party chnage.")

(defvar task-file-properties '(;; (buffer-read-only . t)
                               (fill-column . 172))
  "Task file properties.")

(defvar task-org-headers
  '("#+CATEGORY: Work"
    "#+STARTUP: overview"
    "#+STARTUP: hidestars"
    "#+TAGS: PERFORCE(4)  BUGZILLA(b) SVN(v) SCMBUG(m) PROJECT(j)"
    "#+TAGS: CVS(i) PHONE(p) INTERNET(i)"
    ;; "#+SEQ_TODO: TODO STARTED DONE"
    )
  "Desc")

(defvar *task-projbuffs-base-dir* "~/Documents/org/tasks")

(defvar *task-type-config*
  '(("bug"
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
     (project         "works.pb"))
    ))

(defvar *taskdir-current-task* nil "Current task")

(add-to-list
 'desktop-globals-to-save
 '*taskdir-current-task*)
(add-to-list
 'session-globals-include
 '(*taskdir-current-task* 100))

;;;###autoload
(defun add-to-task-current-party-change-hook (fn &optional append local)
  (add-to-hook
   'task-current-party-change-hook
   fn
   append
   local))

;;;###autoload
(defun remove-task-current-party-change-hook (fn)
  (remove-hook
   'task-current-party-change-hook
   fn))

;; (progn ;; base-dir function
;;;{{{ task base
(defun task-party-base-org-master-file (base-dir)
  (let* ((base-dir              base-dir)
         (org-master-file       *task-party-base-org-master-file*)
         (org-master-file-path (expand-file-name org-master-file base-dir)))
    (if (file-directory-p base-dir)
        (progn
          (unless (file-exists-p org-master-file-path)
            (let ((nfile org-master-file-path)) ;find alternate of find-file-noselect to get non-existing file.
              (task-create-org-file nfile
                (insert "\n\n")
                (insert (format "* %s: %s\n\n\n" "start" "tasks."))
                (insert (format "* Reports\n\n\n"))
                ;; (insert (format "* %s\n" (task-party-org-heading party)))
                )))

          org-master-file)
        (error "directory %s not exists" base-dir))))

;;;###autoload
(defun task-make-party-base-dir (base-dir)
  (interactive
   (list (read-directory-name "Select task-party-base-dir: ")))
  (when base-dir
    (unless (file-directory-p base-dir)
      (make-directory base-dir t))
    (when (file-directory-p base-dir)
      (task-party-base-org-master-file base-dir))
    base-dir))

;;;###autoload
(defun task-party-base-dir (&optional base-dir)
  (interactive
   (list (read-directory-name "Select task-party-base-dir: ")))
  (when base-dir
    (unless (file-directory-p base-dir)
      (task-make-party-base-dir base-dir))
    (when (file-directory-p base-dir)
      (task-party-base-org-master-file base-dir)
      (setq
       *task-party-base-dir* base-dir)))
  (message
   "task-projbuffs-base-dir: changing supplied base-dir %s and task-projbuffs-base-dir to %s"
   base-dir
   *task-party-base-dir*)
  *task-party-base-dir*)

;;;###autoload
(defun task-scratch-dir (&optional scratch-dir)
  (interactive
   (list (read-directory-name "Select task-scratch-dir: ")))
  (when scratch-dir
    (unless (file-directory-p scratch-dir)
      (make-directory scratch-dir t))
    (when (file-directory-p scratch-dir)
      (setq
       task-scratch-dir scratch-dir)))
  task-scratch-dir)

;;;###autoload
(defun task-projbuffs-base-dir (&optional projbuffs-base-dir)
  (interactive
   (list (read-directory-name "Select task-projbuffs-base-dir: ")))
  (when projbuffs-base-dir
    (unless (file-directory-p projbuffs-base-dir)
      (make-directory projbuffs-base-dir t))
    (when (file-directory-p projbuffs-base-dir)
      (setq
       *task-projbuffs-base-dir* projbuffs-base-dir)))
  projbuffs-base-dir)
;;;}}}
;;  )

;; (progn ;; party functions
;;;{{{  party
;;;###autoload
(defun task-add-task-party (name org-master-file org-heading bugz-url)
  (push
   `(,name
     (org-master-file ,org-master-file)
     (org-heading     ,org-heading)
     (bugz-url        ,bugz-url))
   task-parties)

  (unless task-current-party
    (task-current-party name)))

;;;###autoload
(defun task-current-party (&optional party)
  (interactive
   (let ((party
          (ido-completing-read
           "select party: "
           (mapcar 'car task-parties))))
     (list party)))
  (progn
    (when (member party (mapcar 'car task-parties))
      (setq task-current-party party)
      (run-hooks 'task-current-party-change-hook))
    task-current-party))

;;;###autoload
(defun task-current-party-select-set (&optional prompt)
  (interactive)
  (let ((party (ido-completing-read
                (or prompt "select party: ")
                (mapcar 'car task-parties))))
    (if (member party (mapcar 'car task-parties))
        (task-current-party party)
        (error "task-current-party-select-set: party `%s' is not from task-parties" party))))

;;;###autoload
(defun task-select-party-dir ()
  (interactive)
  (ido-read-directory-name "dir: " *task-party-base-dir* nil t))

;;;###autoload
(defun find-task-dir (&optional force)
  (interactive "P")
  (if (or
       force
       (null *taskdir-current-task*))
      (setq *taskdir-current-task*
            (ido-read-directory-name "dir: " (task-party-dir) nil t))
      *taskdir-current-task*))

;;;###autoload
(defun task-select-task-type (&optional prompt)
  (ido-completing-read
   (or prompt "select task type: ")
   (mapcar 'car *task-type-config*) nil t))

(defun task-party-org-master-file (&optional party)
  (let* ((party                 (or party (task-current-party)))
         (party-dir             (expand-file-name party (task-party-base-dir)))
         (org-master-file       (cadr
                                 (assoc 'org-master-file
                                        (cdr (assoc party task-parties)))))
         (org-master-file-path (expand-file-name org-master-file party-dir)))
    (if (member party (mapcar 'car task-parties))
        (progn
          (if (file-directory-p party-dir)
              (unless (file-exists-p org-master-file-path)
                (let ((nfile org-master-file-path)) ;find alternate of find-file-noselect to get non-existing file.
                  (task-create-org-file nfile
                    (insert "\n\n")
                    (insert (format "* %s - %s: %s\n\n\n" (capitalize "party") (capitalize party) "tasks."))
                    (insert (format "* Reports\n\n\n"))
                    (insert (format "* %s\n" (task-party-org-heading party))))))
              (error "directory %s not exists" party-dir))
          org-master-file)
        (error "task-party-org-master-file: party `%s' is not from task-parties" party))))

(defun task-make-party-dir (party)
  (let ((party party))
    (if (member party (mapcar 'car task-parties))
        (let ((party-dir (expand-file-name party (task-party-base-dir))))
          (unless (file-directory-p party-dir)
            (make-directory party-dir t))
          (when (file-directory-p party-dir)
            (task-party-org-master-file party))


          ;; add org heading entry to (task-party-base-org-master-file)

          party-dir)
        (error "task-make-party-dir: party `%s' is not from task-parties" party))))

;;;###autoload
(defun task-party-dir (&optional party)
  "Task Party Directory"
  (let ((party (or party (task-current-party))))
    (if (member party (mapcar 'car task-parties))
        (let ((party-dir (expand-file-name party (task-party-base-dir))))
          (unless (file-directory-p party-dir)
            (task-make-party-dir party))
          (when (file-directory-p party-dir)
            (task-party-org-master-file party))
          party-dir)
        (error "task-party-dir: party `%s' is not from task-parties" party))))

;;;###autoload
(defun task-party-dir-files-recursive ()
  (directory-files-recursive (task-party-dir)
                             "\\.org$" 2 "\\(rip\\|stage\\)"))

;;;###autoload
(defun task-party-org-heading (&optional party)
  (let ((party (or party (task-current-party))))
    (if (member party (mapcar 'car task-parties))
        (cadr
         (assoc 'org-heading
                (cdr (assoc party task-parties))))
        (error "task-party-org-heading: party `%s' is not from task-parties" party))))

;;;###autoload
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

;;;###autoload
(defun task-party-bugz-url (&optional party)
  (let ((party (or party (task-current-party))))
    (if (member party (mapcar 'car task-parties))
        (cadr
         (assoc 'bugz-url
                (cdr (assoc party task-parties))))
        (error "task-party-bugz-url: party `%s' is not from task-parties" party))))

;;;###autoload
(defun task-party-url-base (&optional party)
  "task-party-url-base"
  (let ((party (or party (task-current-party))))
    (if (member party (mapcar 'car task-parties))
        (concat *task-url-path* party)
        (error "task-party-url-base: party `%s' is not from task-parties" party))))

;;;###autoload
(defun task-party-projbuffs-dir (&optional party)
  (let ((party (or party (task-current-party))))
    (if (member party (mapcar 'car task-parties))
        (expand-file-name party *task-projbuffs-base-dir*)
        (error "task-projbuffs-dir: party `%s' is not from task-parties" party))))

;;;}}}
;;  )

;;;{{{
;; (progn ;; task-type
;;;###autoload
(defun task-org-master-file (task-type)
  (if (member task-type (mapcar 'car *task-type-config*))
      (cdr (assoc 'org-master-file (cdr (assoc task-type *task-type-config*))))
      (error "task-type is not from *task-type-config*")))

;;;###autoload
(defun task-first-org-master-file (task-type)
  (if (member task-type (mapcar 'car *task-type-config*))
      (cadr (assoc 'org-master-file (cdr (assoc task-type *task-type-config*))))
      (error "task-type is not from *task-type-config*")))

;;;###autoload
(defun task-org-todo-file (task-type)
  (if (member task-type (mapcar 'car *task-type-config*))
      (cadr (assoc 'org-todo-file (cdr (assoc task-type *task-type-config*))))
      (error "task-type is not from *task-type-config*")))

;;;###autoload
(defun task-org-files (task-type)
  (if (member task-type (mapcar 'car *task-type-config*))
      (cdr (assoc 'org-files (cdr (assoc task-type *task-type-config*))))
      (error "task-type is not from *task-type-config*")))
;;  )
;;

;;;###autoload
(defun task-get-task-name (prompt party task-type &optional new)
  (let* ((party (or party (task-current-party)))
         (task-name (completing-read
                     prompt
                     (unless new (directory-files (concat (task-party-dir party) "/" (pluralize-string task-type) "/")))
                     nil
                     (not new))))
    (if new
        (let* ((predicate (cadr (assoc 'name (cdr (assoc task-type *task-type-config*)))))
               (task-name-match
                (cond
                  ((stringp predicate)
                   (string-match predicate task-name))
                  ((functionp predicate)
                   (funcall predicate task-name))
                  ((null predicate)
                   (> (length task-name) 0)))))
          (if task-name-match
              (if (file-exists-p (task-get-task-dir party task-type task-name))
                  (error "task-type %s name %s already exists." task-type task-name)
                  task-name)
              (error "task-type %s name %s not matching to %s" task-type task-name predicate)))
        task-name)))

;;;###autoload
(defun task-get-task-desc (party task-type name &optional new)
  (if new
      (let* ((bug (if (string-equal task-type "bug")
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
      (let* ((task-dir       (task-get-task-dir (task-current-party) task-type name))
             (task-desc-file (expand-file-name task-dir *task-desc-file-name*)))
        (if (file-exists-p task-desc-file)
            (lotus-read-file task-desc-file)
            (task-get-task-desc party task-type name t)))))

;;;###autoload
(defun task-get-task-dir (party task-type name)
  (concat (task-party-dir party) "/" (pluralize-string task-type) "/" name))

;;;###autoload
(defun task-get-task-data (&optional new)
  (let* ((task-type           (task-select-task-type "what: "))
         (name                (task-get-task-name "name: " (task-current-party) task-type new))
         (desc                (task-get-task-desc task-type (task-current-party) name))
         (task-dir            (task-get-task-dir (task-current-party) task-type name)))
    (list task-type name desc task-dir)))

;;;###autoload
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
;;;###autoload
(defun task-get-planner-description (task-type name desc)
  (cl-flet ((my-formattor (id summary url)
                          ;; BUG: w f b
                          (format "[[%s][%c%s]]: %s %s"
                                  (concat (task-party-url-base) "/" (pluralize-string task-type) "/" (number-to-string id))
                                  (aref task-type 0)
                                  (number-to-string id) summary (concat "[[" url "][url]]"))))
    (let* ((planner-bugz-formattor #'my-formattor)
           (hname
            (if (task-party-url-base)
                ;; BUG: w f b
                (format "[[%s][%c%s]]:" (concat (task-party-url-base) "/" (pluralize-string task-type) "/" name) (aref task-type 0) name)
                (format "%c%s:" (aref task-type 0) name)))
           (task-description
            (cond
              ((string-equal task-type "bug")     (planner-bugzilla-bug-to-task-name name (task-party-bugz-url)))
              ((string-equal task-type "feature") (format "%s %s" hname desc))
              ((string-equal task-type "work")    (format "%s %s" hname desc))
              (t                             (error "task-type is not bound.")))))
      task-description)))

;;;###autoload
(defun task-get-links-text (name task-type formatterfn)
  (let ((description ""))
    (dolist
        (f (task-org-files task-type) description)
      (setq description
            (concat
             description
             ;; "\n - "
             (funcall formatterfn name task-type f))))))

;;;###autoload
(defun task-get-org-description (task-type name desc)
  (cl-flet ((my-formattor (id summary url)
                          ;; BUG: w f b
                          (format
                           "[[%s][%s - %s]]: %s %s"
                           (concat (task-party-url-base) "/" (pluralize-string task-type) "/" (number-to-string id))
                           (pluralize-string task-type)
                           (number-to-string id)
                           summary
                           (concat "[[" url "][url]]"))))
    (let* ((planner-bugz-formattor #'my-formattor)
           (hname
            (if (task-party-url-base)
                ;; BUG: w f b
                ;; (format "[[%s][%s - %s]]:" (concat (task-party-url-base) "/" (pluralize-string task-type) "/" name) (capitalize task-type) name)
                (format "%s - %s:" (capitalize task-type) name)
                (format "%s - %s:" (capitalize task-type) name)))
           (task-description
            (cond
              ((string-equal task-type "bug")     (planner-bugzilla-bug-to-task-name name (task-party-bugz-url)))
              ((string-equal task-type "feature") (format "%s %s" hname desc))
              ((string-equal task-type "work")    (format "%s %s" hname desc))
              (t                             (error "task-type is not bound."))))
           (description ""))

      (setq
       description
       (concat
        description
        task-description
        (format "\n  [[file:%s/%s/%s][dir]]"
                (pluralize-string task-type)
                name
                (task-first-org-master-file task-type))
        (task-get-links-text name task-type
                             '(lambda (name task-type file)
                               (format "\n - [[file:%s/%s/%s][%s]]"
                                (pluralize-string task-type)
                                name
                                file
                                (capitalize (file-name-sans-extension file)))))
        (concat description (format "\nend %s\n" task-type)))))))

;;;#autoload
(defun task-create-plan-task (task-type name desc task-dir)
  (interactive (task-get-task-data t))
  (let* ((plan-page
          (planner-read-non-date-page (planner-file-alist))))

    (planner-create-task
     (task-get-planner-description task-type name desc)
     (let ((planner-expand-name-favor-future-p
            (or planner-expand-name-favor-future-p
                planner-task-dates-favor-future-p)))
       (planner-read-date))
     nil
     plan-page
     (task-status-of-sys 'planner 'inprogress))
    t))

;;;#autoload
(defun task-delete-plan-task (task-type name desc task-dir)
  (interactive (task-get-task-data))
  (message "Not doing anything."))

;;;#autoload
(defun task-create-org-task (task-type name desc task-dir project-main-file project-root-folder)
  (interactive
   (let* ((task-data (task-get-task-data t))
          (project-root-folder (iproject-choose-root-folder project-main-file)))
     (append
      task-data
      (list project-main-file project-root-folder))))

  (let ((file    (expand-file-name (task-party-org-master-file) (task-party-dir)))
        (heading (task-party-org-heading))
        (child-heading (task-get-org-description task-type name desc)))
    (org-with-file-headline file heading
      (org-insert-subheading-to-file-headline
       child-heading
       file
       (capitalize (pluralize-string task-type))))
    (org-with-file-headline file child-heading
                            (let ((buffer-read-only nil))
                              (org-entry-put nil "SubtreeFile"
                                             (format "%s/%s/%s"
                                                     (pluralize-string task-type)
                                                     name
                                                     (task-first-org-master-file task-type))

                                             ;; (file-relative-name
                                             ;;  (concat task-dir "/" (task-first-org-master-file task-type))
                                             ;;  (file-name-directory file))
                                             )
                              ;; (org-entry-put nil "Root" project-root-folder)
                              ))
    (with-current-buffer (find-file-noselect file)
      (let ((buffer-read-only nil))
        (save-buffer)))
    t))

;;;#autoload
(defun task-delete-org-task (task-type name desc task-dir)
  (interactive (task-get-task-data))
  (let ()
    (org-remove-heading-from-file-headline
     (task-get-org-description task-type name desc)
     (expand-file-name (task-party-org-master-file) (task-party-dir))
     (task-party-org-heading))
    t))

;; Project-Buffer
;;;#autoload
(defun task-create-pbm-task (task-type name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
  (interactive (task-get-task-data-all t))
  (let* ((project-name (concat task-type ":" name " - " desc)))
    (with-project-buffer (find-file-noselect
                          (expand-file-name
                           (concat
                            (pluralize-string task-type) ".pb") ;; (task-projbuffs-dir)
                           (task-party-projbuffs-dir)
                           ))
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
         doc-base-virtual-folder))

      (save-buffer))))

;; (defun task-delete-pbm-task (task name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
;;;#autoload
(defun task-delete-pbm-task (task-type name desc task-dir)
  (interactive (task-get-task-data-all))
  (message "Not doing anything."))

;;;#autoload
(defun task-create-task-dir (task-type name desc task-dir project-root-folder)
  (interactive (task-get-task-data t))
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (let* ()
    (make-directory task-dir t)
    (lotus-write-file (expand-file-name *task-desc-file-name* task-dir) desc)
    (if (file-directory-p task-scratch-dir)
        (progn
          (make-directory (concat task-scratch-dir (pluralize-string task-type) "/" name) t)
          (make-symbolic-link (concat task-scratch-dir (pluralize-string task-type) "/" name) (concat task-dir "/scratch")))
        (warn "Ensure to create task-scratch-dir %s directory" task-scratch-dir))

    (let ((org-heading (format "%s - %s: %s" (capitalize task-type) name desc)))
      ;; files
      (dolist (file
                (cons
                 (task-org-todo-file task-type)
                 (task-org-files task-type)))
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

      (let ((file (task-first-org-master-file task-type)))
        (when file

          (let ((nfile (expand-file-name file task-dir))
                (heading (format "%s %s"
                                 (capitalize (file-name-sans-extension file))
                                 org-heading))) ;find alternate of find-file-noselect to get non-existing file.
            (task-create-org-file nfile
                                  (insert (concat
                                           (format "\n\n* %s\n" heading)
                                           (task-get-links-text name task-type
                                                                '(lambda (name task-type f)
                                                                  (format "** %s\n- [[file:%s][%s]]\n"
                                                                   (capitalize (file-name-sans-extension f))
                                                                   f
                                                                   "here"))))))
            (dolist (sfile
                      (cons
                       (task-org-todo-file task-type)
                       (task-org-files task-type)))
              (org-with-file-headline nfile (capitalize (file-name-sans-extension sfile))
                                      (with-writable-buffer
                                          (message "adding property %s in %s" sfile nfile)
                                        (org-entry-put nil "SubtreeFile" sfile)))))))

      ;; links
      (dolist (lp (cdr (assoc 'links (cdr (assoc task-type *task-type-config*)))))
        (make-symbolic-link
         (car lp) ;; (expand-file-name (car lp) (concat task-dir "/"))
         (expand-file-name (cdr lp) (concat task-dir "/"))))
      ;; dirs
      (dolist
          (dname
            (cdr (assoc 'dirs (cdr (assoc task-type *task-type-config*))))
           t)
        (make-directory (concat task-dir "/" dname) t)))))

;;;#autoload
(defun task-delete-task-dir (task-type name desc task-dir)
  (interactive (task-get-task-data))
  ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
  (delete-directory task-dir t))

;;;#autoload
(defun task-create-task (task-type name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
  (interactive (task-get-task-data-all t))
  (let* ()
    (if (file-directory-p task-dir)
        (find-task task-dir)
        ;; (progn
        ;;   (task-create-plan-task task-type name desc task-dir)
        ;;   (task-create-org-task  task-type name desc task-dir project-main-file project-root-folder)
        ;;   (task-create-task-dir  task-type name desc task-dir project-root-folder)
        ;;   (task-create-pbm-task  task-type name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
        ;;   (org-context-clock-task-update-tasks t))

        (progn
          (task-create-task-dir  task-type name desc task-dir project-root-folder)
          (task-create-pbm-task  task-type name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
          (task-create-plan-task task-type name desc task-dir)
          (task-create-org-task  task-type name desc task-dir project-main-file project-root-folder)
          (org-context-clock-task-update-tasks t)))

    (when (y-or-n-p (format "Should set %s current task" task-dir))
      (setq *taskdir-current-task* task-dir)
      (find-file (expand-file-name (task-first-org-master-file task-type) task-dir)))))

;;;#autoload
(defun task-delete-task (task-type name desc task-dir)
  (interactive (task-get-task-data))
  (let* ()
    (progn
      (task-delete-plan-task task-type name desc task-dir)
      (task-delete-org-task  task-type name desc task-dir)
      (task-delete-task-dir  task-type name desc task-dir)
      ;; (task-delete-pbm-task  task-type name desc task-dir project-type project-main-file project-root-folder project-file-filter doc-file-filter doc-base-virtual-folder)
      (task-delete-pbm-task  task-type name desc task-dir))))

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

;;;###autoload
(defun task-org-task-files (&optional party)
  (let ((party (or party (task-current-party))))
    (directory-files-recursive
     (task-party-dir party) "\\.org$" 7)))

;;;###autoload
(defun task-org-all-task-files ()
  (let ()
    (directory-files-recursive
     (task-party-base-dir)
     "\\.org$" 7)))

;;;###autoload
(defun task-org-task-refile-target (party)
  (let* ((party (or party (task-current-party)))
         (task-files (task-org-task-files party)))
                                        ;all files returned by `task-org-task-files'
    `((,task-files :maxlevel . 3))))

;;;#autoload
(defun org-clock-in-refile-task (party)
  (interactive
   (list (ido-completing-read
          "Seletc Party: "
          (mapcar 'car task-parties)
          nil
          t
          (task-current-party))))
  (org-clock-in-refile (task-org-task-refile-target party)))
;; )






(define-minor-mode office-mode
    "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
  :initial-value nil
  :lighter " Office"
  :global nil
  (condition-case e
      (if office-mode
          (progn
            (message "calling enable office mode")
            (when (or
                   (eq major-mode 'c-mode)
                   (eq major-mode 'c++-mode))
              (setq tab-width 8)
              (c-set-style "stroustrup" 1))
            (set (make-local-variable 'before-save-hook) before-save-hook)
            (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
            (run-with-timer
             7 nil
             #'(lambda (buff)
                 (when (bufferp buff)
                   (with-current-buffer buff
                     (forgive/them))))
             (current-buffer))
            (message "called enable office mode"))

        (progn
          (message "calling disable office mode")
          (when (or
                 (eq major-mode 'c-mode)
                 (eq major-mode 'c++-mode))
            (setq tab-width (custom-reevaluate-setting 'tab-width))
            (c-set-style "gnu" 1))
          (set (make-local-variable 'before-save-hook) before-save-hook)
          (add-hook 'before-save-hook 'delete-trailing-whitespace t)
          (message "called disable office mode")))
    (error (message "Error: %s" e))))

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
;;;}}}

(provide 'task-manager)
;;; task-manager.el ends here
