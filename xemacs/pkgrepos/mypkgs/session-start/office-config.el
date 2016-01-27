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



(deh-require-mustbe (and
                     orgmode-config
                     publishing-config)

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

 (defvar *task-party-base-dir* (org-publish-get-attribute "tasks" "org" :base-directory) "Task Party Directory")

 (defvar task-scratch-dir "~/SCRATCH/" "task scratch directory")

 (defvar task-parties
   '(
     ("meru"
      (org-master-file "report.org")
      (org-heading "Office related work"))
     ("personal"
      (org-master-file "report.org")
      (org-heading "Office related work"))))

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

 (defvar *task-projbuffs-base-dir* (expand-file-name "misc/projbuffs" *created-content-dir*))

 (defvar task-config '(("bug"
                        (master-org-file "report.org")
                        (org-files "todo.org" "notes.org" "analysis.org")
                        (dirs "logs" "programs" "patches" "deliverables")
                        (links ("notes.html" . "index.html"))
                        (project "bugs.pb"))
                       ("feature"
                        (master-org-file "report.org")
                        (org-files "reqirement.org" "feasibility.org"
                         "design.org" "todo.org" "notes.org" "analysis.org")
                        (dirs "logs" "programs" "patches" "deliverables")
                        (links ("notes.html" . "index.html"))
                        (project "features.pb"))
                       ("work"
                        (master-org-file "report.org")
                        (org-files "reqirement.org" "feasibility.org"
                         "design.org" "todo.org" "notes.org" "analysis.org")
                        (dirs "logs" "programs" "patches" "deliverables")
                        (links ("notes.html" . "index.html"))
                        (project "works.pb"))))

 (defvar *taskdir-current-task* nil "Current task")

 (add-to-list
  'desktop-globals-to-save
  '*taskdir-current-task*)
 (add-to-list
  'session-globals-include
  '(*taskdir-current-task* 100))

 ;; (defvar taskdir
 ;;   (expand-file-name task-current-party
 ;;                     (org-publish-get-attribute "tasks" "org" :base-directory))
 ;;   "Task Directory")

 (defun task-party-dir (&optional party)
   "Task Directory"
   (let ((party (or party task-current-party)))
    (expand-file-name party
                      (org-publish-get-attribute "tasks" "org" :base-directory))))

 (defun task-party-org-heading ()
   (cadr
    (assoc 'org-heading
           (cdr (assoc task-current-party task-parties)))))

 (defun task-party-org-master-file ()
   (cadr
    (assoc 'org-master-file
           (cdr (assoc task-current-party task-parties)))))

 (defun task-party-url-base ()
   "task-party-url-base"
   (concat "/~s/tasks/" task-current-party))

 (defun task-org-master-file (task)
   (if (member task (mapcar 'car task-config))
       (cadr (assoc 'master-org-file (cdr (assoc task task-config))))
       (error "task is not from task-config")))

 (defun task-org-files (task)
   (if (member task (mapcar 'car task-config))
       (cdr (assoc 'org-files (cdr (assoc task task-config))))
       (error "task is not from task-config")))

 (defun task-projbuffs-dir ()
   (expand-file-name task-current-party *task-projbuffs-base-dir*))

 (defun task-select-party-dir (force)
   (interactive "P")
   (if (or force (null taskdir))
       (setq taskdir
             (ido-read-directory-name "dir: " *task-party-base-dir* nil t))
       taskdir))

 (defun find-task-dir (&optional force)
   (interactive "P")
   (if (or
        force
        (null *taskdir-current-task*))
       (setq *taskdir-current-task*
             (ido-read-directory-name "dir: " (task-party-dir) nil t))
       *taskdir-current-task*))

 (defun task-get-planner-description (task name desc)
   (flet ((my-formattor (id summary url)
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
                 ((string-equal task "bug")     (planner-bugzilla-bug-to-task-name name))
                 ((string-equal task "feature") (format "%s %s" hname desc))
                 ((string-equal task "work")    (format "%s %s" hname desc))
                 (t                             (error "task is not bound.")))))
         (format "%s [[../../../../../../../org/tasks/%s/%s/%s/%s][dir]]"
                 task-description
                 task-current-party
                 (pluralize-string task)
                 name
                 (task-org-master-file task)))))

 (defun task-get-org-description (task name desc)
   (flet ((my-formattor (id summary url)
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
                   (format "[[%s][%s - %s]]:" (concat (task-party-url-base) "/" (pluralize-string task) "/" name) (capitalize task) name)
                   (format "%s - %s:" (capitalize task) name)))
              (task-description
               (cond
                 ((string-equal task "bug")     (planner-bugzilla-bug-to-task-name name))
                 ((string-equal task "feature") (format "%s %s" hname desc))
                 ((string-equal task "work")    (format "%s %s" hname desc))
                 (t                             (error "task is not bound."))))
              (description ""))
         (setq
          description
          (concat
           description
           (format "%s [[file:%s/%s/%s][dir]]"
                   task-description
                   (pluralize-string task)
                   name
                   (task-org-master-file task))))

         (dolist
             (f (task-org-files task) description)
           (setq description
                 (concat
                  description
                  "\n - "
                  (format "[[file:%s/%s/%s][%s]]"
                          (pluralize-string task)
                          name
                          f
                          (capitalize (file-name-sans-extension f)))))))))

 (defun create-plan-task (dir task name desc)
   (let* ((plan-page
           (planner-read-non-date-page (planner-file-alist))))

         (planner-create-task
          (task-get-planner-description task name desc)
          (let ((planner-expand-name-favor-future-p
                 (or planner-expand-name-favor-future-p
                     planner-task-dates-favor-future-p)))
            (planner-read-date))
          nil plan-page
          (task-status-of-sys 'planner 'inprogress))))

 (defun create-org-task (dir task name desc)
   (let ()
    (org-insert-heading-to-file-headline
     (task-get-org-description task name desc)
     (expand-file-name (task-party-org-master-file) (task-party-dir))
     (task-party-org-heading))))

 ;; Project-Buffer
 (defun create-pbm-task (dir task name desc)
   (let ((project-name (concat task ":" name " - " desc)))
     (with-project-buffer (find-file-noselect
                           (expand-file-name
                            (concat (pluralize-string task) ".pb") (task-projbuffs-dir)))
       (iproject-add-project
        nil                          ;project-type
        nil                          ;project-main-file
        nil                          ;project-root-folder
        project-name                 ;project-name
        nil)                         ;file-filter
       (project-buffer-set-master-project-no-status (current-buffer) project-name)
       (iproject-add-files-to-current-project dir))))

 (defun create-task-dir (dir task name desc)
   ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
   (make-directory dir t)
   (make-directory (concat task-scratch-dir (pluralize-string task) "/" name) t)
   (make-symbolic-link (concat task-scratch-dir (pluralize-string task) "/" name) (concat dir "/scratch"))

   ;; files
   (dolist (f (task-org-files task))
     (let ((nfile (expand-file-name f (concat dir "/")))
           find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
       (with-current-buffer (or (find-buffer-visiting nfile)
                                (find-file-noselect nfile))
         ;; (if (goto-char (point-min))
         ;;     (insert "# -*-  -*-\n"))
         (dolist (pv task-file-properties)
           (add-file-local-variable-prop-line (car pv) (cdr pv)))
         (goto-char (point-max))
         (insert (reduce '(lambda (a b) (concat a "\n" b)) task-org-headers))
         (goto-char (point-max))
         (insert (format "\n\n* %s - %s: %s\n\n\n\n" (capitalize task) name desc))
         (set-buffer-file-coding-system
          (if (coding-system-p 'utf-8-emacs)
              'utf-8-emacs
              'emacs-mule))
         (write-file nfile)
         (org-html-export-to-html))
       (kill-buffer (find-buffer-visiting nfile))))


   ;; master file
   (let ((f (task-org-master-file task)))
     (let ((nfile (expand-file-name f (concat dir "/")))
           find-file-not-found-functions) ;find alternate of find-file-noselect to get non-existing file.
       (with-current-buffer (or (find-buffer-visiting nfile)
                                (find-file-noselect nfile))
         ;; (if (goto-char (point-min))
         ;;     (insert "# -*-  -*-\n"))
         (dolist (pv task-file-properties)
           (add-file-local-variable-prop-line (car pv) (cdr pv)))
         (goto-char (point-max))
         (insert (reduce '(lambda (a b) (concat a "\n" b)) task-org-headers))
         (goto-char (point-max))
         (insert (format "\n\n* %s - %s: %s\n\n\n\n" (capitalize task) name desc))

         (dolist
             (of (task-org-files task))
           (insert
            "\n - "
            (format "[[file:%s][%s]]"
                    of
                    (capitalize (file-name-sans-extension of)))))

         ;; create linked sub entry for design todo etc.

         (set-buffer-file-coding-system
          (if (coding-system-p 'utf-8-emacs)
              'utf-8-emacs
              'emacs-mule))
         (write-file nfile)
         (org-html-export-to-html))
       (kill-buffer (find-buffer-visiting nfile))))

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
           (name (completing-read "name: " (directory-files (concat (task-party-dir) "/" (pluralize-string task) "/")) nil))
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

   (let* ((dir (concat (task-party-dir) "/" (pluralize-string task) "/" name))
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
           (create-org-task dir task name desc)
           (create-pbm-task dir task name desc)
           ;; create task dir
           (create-task-dir dir task name desc)))

     (when (y-or-n-p (format "Should set %s current task" dir))
         (setq *taskdir-current-task* dir)
         (find-file (expand-file-name
                     (task-org-master-file task) (concat dir "/"))))))

 (defun delete-task-dir (dir task name desc)
   ;; (dolist (dname '("logs" "programs" "patches" "deliverables"))
   (make-directory dir t)
   (make-directory (concat task-scratch-dir (pluralize-string task) "/" name) t)
   (make-symbolic-link (concat task-scratch-dir (pluralize-string task) "/" name) (concat dir "/scratch"))

   ;; files
   (dolist (f (task-org-files task))
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
         (write-file nfile)
         (org-html-export-to-html))
       (kill-buffer (find-buffer-visiting nfile))))
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
           (name (completing-read "name: " (directory-files (concat (task-party-dir) "/" (pluralize-string task) "/")) nil))
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

   (let* ((dir (concat (task-party-dir) "/" (pluralize-string task) "/" name))
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
             ;; BUG -- it should delete not create
             (planner-create-task
              (task-get-planner-description task name desc)
                  (let ((planner-expand-name-favor-future-p
                         (or planner-expand-name-favor-future-p
                             planner-task-dates-favor-future-p)))
                    (planner-read-date))
                  nil plan-page
                  (task-status-of-sys 'planner 'inprogress)))
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
                       (task-org-master-file task)
                       (concat dir "/")))))

     (if (y-or-n-p (format "Should set %s current task" dir))
         (setq *taskdir-current-task* dir))))

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

 (deh-section "Forgive"
   (defun forgive/them ()
     (interactive)
     (if (and
          (featurep 'develock)
          (assq major-mode develock-keywords-alist))
         (develock-mode -1))
     (highlight-changes-visible-mode -1)))


 (deh-section "Org Task"

   (defun org-task-files (&optional party)
     (let ((party (or party task-current-party)))
       (directory-files-recursive
        (task-party-dir party)
        "\\.org$" 7 "\\(rip\\|stage\\)" t)))

   (defun org-task-refile-target (party)
     ;; (interactive)
     (let* ((party (or party task-current-party))
            (task-files (org-task-files party)))
        ;all files returned by `org-task-files'
       `((,task-files :maxlevel . 3))))

  (defun org-clock-in-refile-task (party)
    (interactive
     (list (ido-completing-read
            "Seletc Party: "
            (mapcar 'car task-parties)
            nil
            t
            task-current-party)))
    (let ((refile-targets (org-task-refile-target party))) ;all files returned by `org-task-files'
      (org-clock-in-refile refile-targets)))
 )


(provide 'office-config)
