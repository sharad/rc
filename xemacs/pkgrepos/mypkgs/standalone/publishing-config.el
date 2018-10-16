;;; publishing-config.el --- publishing configuration

;; Copyright (C) 2015  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords:convenience, data, hypermedia, wp

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

;; Publishing related configuration.

;;; Code:

(require 'macros-config)

(deh-require-maybe (or muse muse-project org org-compat ox-publish)

  (defvar *doc-root*            (expand-file-name "Documents" "~"))
  (defvar *created-content-dir* (expand-file-name "CreatedContent" *doc-root*))
  (defvar *website-address*     "http://sharad.ddns.net/~s/gen/")


  (deh-section "muse publishing utils"

    (defvar *muse-top-dir*           (expand-file-name "contents/muse" *created-content-dir*))
    (defvar *muse-top-style-dir*     (expand-file-name "generic/muse/style" *muse-top-dir*))
    (defvar *muse-generated-top-dir* (expand-file-name "gen/muse" *created-content-dir*))
    (defvar *muse-website-address*   (concat *website-address* "muse/"))

    (defun* read-muse-style-spec ()
      (let* ((muse-dir (read-directory-name "Muse Project Directory: " *muse-top-dir*))
             (publishing-path
              (read-directory-name
               "Muse Project Directory: "
               (concat *muse-generated-top-dir* (replace-regexp-in-string *muse-top-dir* "" muse-dir))))
             (publishing-style
              (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
             (publishing-url (read-from-minibuffer "Publishing Base URL: "))
             (publishing-options nil))
        (list muse-dir publishing-path publishing-style publishing-url publishing-options)))

    (defun* read-muse-project-spec ()
      (let* ((name (read-from-minibuffer "Project Name: "))
             (muse-dirs
              (read-directory-name "Muse Project Directory: " (concat *muse-top-dir* "/" name)))
             (publishing-path
              (read-directory-name
               "Muse Project Directory: "
               (concat *muse-generated-top-dir*
                       (replace-regexp-in-string *muse-top-dir* ""
                                                 (if (consp muse-dirs) (car muse-dirs) muse-dirs)))))
             (publishing-style
              (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
             (publishing-url (read-from-minibuffer "Publishing Base URL: "))
             (publishing-options nil))
        `(,name
          ,@(make-muse-style-spec
             (if (consp muse-dirs) (car muse-dirs) muse-dirs)
             publishing-path
             publishing-style
             publishing-url
             publishing-options))))

    (defun add-muse-project (project-spec)
      "Add muse project."
      (interactive
       (let ((project-spec
              (read-muse-project-spec)))))
      (if (member (car project-spec)
                  (mapcar 'car muse-project-alist))
          (if (or (not (called-interactively-p 'interactive))
                  (y-or-n-p (format "project %s already present, do you want to overwrite it?: " (car project-spec))))
              (progn
                (remove-muse-project project-spec)
                (add-muse-project project-spec)))
          (add-to-list 'muse-project-alist project-spec)))

    (defun remove-muse-project (project-spec)
      (interactive
       (let ((project (ido-completing-read "Project: "
                                           (mapcar 'car muse-project-alist))))
         (if project
             (list project)
             (error "No project %s present" project))))
      (let ((project
             (cond
               ((and (consp project-spec)
                     (stringp (car project-spec)))
                (car project-spec))
               ((stringp project-spec)
                project-spec)
               (t nil))))
        (if project
            (setq muse-project-alist
                  (delete* project muse-project-alist
                           :key 'car
                           :test 'string-equal)))))

    (defun* make-muse-style-spec (muse-dir publishing-path publishing-style publishing-url &rest publishing-options)
      (interactive
       (let* ((muse-dir (read-directory-name "Muse Project Directory: " *muse-top-dir*))
              (publishing-path
               (read-directory-name
                "Muse Project Directory: "
                (concat *muse-generated-top-dir* (replace-regexp-in-string *muse-top-dir* "" muse-dir))))
              (publishing-style
               (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
              (publishing-url (read-from-minibuffer "Publishing Base URL: "))
              (publishing-options nil))
         (list muse-dir publishing-path publishing-style publishing-url publishing-options)))
      (apply
       'muse-project-alist-styles
       (append
        (list
         muse-dir
         publishing-path)
        (list publishing-style)
        (if publishing-url
            (list :base-url publishing-url))
        publishing-options)))

    (defun* make-muse-project-spec (name muse-dirs publishing-path publishing-style publishing-url &rest publishing-options)
      (interactive
       (let* ((name (read-from-minibuffer "Project Name: "))
              (muse-dirs
               (read-directory-name "Muse Project Directory: " (concat *muse-top-dir* "/" name)))
              (publishing-path
               (read-directory-name
                "Muse Project Directory: "
                (concat *muse-generated-top-dir*
                        (replace-regexp-in-string *muse-top-dir* ""
                                                  (if (consp muse-dirs) (car muse-dirs) muse-dirs)))))
              (publishing-style
               (ido-completing-read "Muse Publishing Style: " (mapcar 'car muse-publishing-styles)))
              (publishing-url (read-from-minibuffer "Publishing Base URL: "))
              (publishing-options nil))
         (list name muse-dirs publishing-path publishing-style publishing-url publishing-options)))
      `(,name
        ,@(make-muse-style-spec
           (if (consp muse-dirs) (car muse-dirs) muse-dirs)
           publishing-path
           publishing-style
           publishing-url
           publishing-options)))

    (defun content-muse-dir (path)
        "thisandthat."
        (expand-file-name path *muse-top-dir*))

    (defun content-muse-publishing-dir (path)
        "thisandthat."
        (expand-file-name path *muse-generated-top-dir*))

    (defun content-muse-publishing-url (localpath)
      (concat *muse-website-address* "/" localpath))

    ;; (mapcar 'car muse-publishing-styles)
    ;; (muse-project-alist-styles
    ;;  (concat *muse-top-dir* "/doc/priv")
    ;;  (concat *muse-generated-top-dir* "/doc/pdf/doc/priv/pdf")
    ;;  "pdf")

    ;; (muse-project-alist-styles
    ;;  (concat *muse-top-dir* "/web/site/blog")
    ;;  (concat *muse-generated-top-dir* "/web/site/blog/pdf")
    ;;  "ikiwiki"
    ;;  :base-url (concat *website-address* "/blog/"))
    )

  (deh-require-maybe (and muse muse-project)

    (deh-section "Muse Projects"

      ;; Here is my master project listing.
      ;; (make-muse-style-spec
      ;;  (content-muse-dir "web/site/wiki/projects")
      ;;  (content-muse-publishing-dir "web/site/wiki/projects/my-xhtml")
      ;;  "my-xhtml"
      ;;  (content-muse-publishing-url "projects/"))

      (setq muse-project-alist nil)

      (add-muse-project
       `("Website"
         (
          ,(content-muse-dir "web/site/wiki/web")
           ,(content-muse-dir "web/site/wiki/web/testdir")
           :force-publish ("WikiIndex")
           :default "WelcomePage")
         (:base "my-xhtml"
                :base-url ,(content-muse-publishing-url "web/")
                :include "/web/[^/]+"
                :path ,(content-muse-publishing-dir "web/site/wiki/web/my-xhtml"))
         (:base "my-xhtml"
                :base-url ,(content-muse-publishing-url "web/")
                :include "/testdir/[^/]+"
                :path ,(content-muse-publishing-dir "web/site/wiki/web/testdir/my-xhtml"))
         (:base "my-pdf"
                :base-url ,(content-muse-publishing-url "web/")
                :path ,(content-muse-publishing-dir "doc/pdf/site/wiki/web/my-pdf")
                :include "/\\(CurriculumVitae\\|BriefResume\\)[^/]*$")))

      (if t
       (add-muse-project
        `("Projects" ( ,(content-muse-dir "web/site/wiki/projects")
                        :force-publish ("WikiIndex" "MuseQuickStart")
                        :default "WelcomePage")
                     (:base "my-xhtml"
                            :base-url ,(content-muse-publishing-url "web/site/wiki/projects/my-xhtml/")
                            :path ,(content-muse-publishing-dir "web/site/wiki/projects/my-xhtml"))))

       (add-muse-project
        `("Projects"
          ( ,(content-muse-dir "web/site/wiki/projects")
             :force-publish ("WikiIndex" "MuseQuickStart")
             :default "WelcomePage")

          ,(make-muse-style-spec
            (content-muse-dir "web/site/wiki/projects")
            (content-muse-publishing-dir "web/site/wiki/projects/my-xhtml/")
            "my-xhtml"
            (content-muse-publishing-url "web/site/wiki/projects/my-xhtml/"))))
       )

      (add-muse-project
       `("WikiWriting"
         (,(content-muse-dir "web/site/wiki/writing")
            ;; :force-publish ("WikiIndex" "MuseQuickStart")
            :major-mode muse-mode
            :default "index")
         (:base "my-xhtml"
                :base-url ,(content-muse-publishing-url "projects/")
                :path ,(content-muse-publishing-dir "web/site/wiki/writing/my-xhtml"))))


      (add-muse-project
       `("Blog" (,@(muse-project-alist-dirs (content-muse-dir "web/site/blog"))
                   :default "index"
                   :publish-project #'ignore)
                ;; Publish this directory and its subdirectories.  Arguments
                ;; are as follows.  The above `muse-project-alist-dirs' part
                ;; is also needed.
                ;;   1. Source directory
                ;;   2. Output directory
                ;;   3. Publishing style
                ;;   remainder: Other things to put in every generated style
                ,@(muse-project-alist-styles
                   (content-muse-dir "web/site/blog")
                   (content-muse-publishing-dir "web/site/blog/ikiwiki")
                   "ikiwiki"
                   :base-url (content-muse-publishing-url "blog/"))))

      ;; "http://grepfind.hello.org/blog/"
      (add-muse-project
       `("MyNotes" (,(content-muse-dir "web/site/wiki/notes")
                     :force-publish ("index")
                     :default "index")
                   (:base "xhtml"
                          :base-url ,(content-muse-publishing-url "notes/")
                          :path ,(content-muse-publishing-dir "web/site/wiki/notes/xhtml"))
                   (:base "my-pdf"
                          :base-url ,(content-muse-publishing-url "notes/")
                          :path ,(content-muse-publishing-dir "web/site/wiki/notes/my-pdf"))))

      (add-muse-project
       `("_Private" (,(content-muse-dir "doc/priv"))
                    ,@(muse-project-alist-styles (content-muse-dir "doc/priv")
                                                 (content-muse-publishing-dir "doc/pdf/doc/priv/pdf")
                                                 "pdf")))

      (add-muse-project
       `("_Classes" (,@(muse-project-alist-dirs (content-muse-dir "web/site/wiki/classes"))
                       :default "index")
                    ,@(muse-project-alist-styles (content-muse-dir "web/site/wiki/classes")
                                                 (content-muse-publishing-dir "web/site/wiki/classes/xhtml")
                                                 "xhtml")))

      (add-muse-project
       `("MA366" (,(content-muse-dir "doc/pdf/classes/ma366"))
                 (:base "pdf-uh"
                        :path ,(content-muse-publishing-dir "doc/pdf/classes/ma366/pdf-uh"))))

      (add-muse-project
       `("ENGL238" (,(content-muse-dir "doc/pdf/classes/eng238"))
                   (:base "pdf-uh"
                          :path ,(content-muse-publishing-dir "doc/pdf/classes/eng238/pdf-uh"))))

      (add-muse-project
       `("CS426" (,(content-muse-dir "web/site/wiki/classes/cs426"))
                 (:base "pdf-uh"
                        :path "~/proj/classes/cs426/pdf-uh")))

      (add-muse-project
       `("_Plans" (,(content-muse-dir "web/site/wiki/plans")
                    :default "TaskPool"
                    :major-mode planner-mode
                    :visit-link planner-visit-link)
                  (:base "planner-xhtml"
                         :path ,(content-muse-publishing-dir "web/site/wiki/plans/html")))))

    (deh-section "muse-meta"

      ;; (file-name-nondirectory "/sdf/sdfds//")

      (when nil
        ;; Do corrections
        (dir-final-slash "/sdfds/dfg/dfg/fdg")
        (pathname-delete-trailing-/ "/sdfds/dfg/dfg/fdg"))



      ;; (defun muse-meta-style-dirname (dir)
      ;;   (let* ((style (plist-get muse-publishing-current-style :base))
      ;;          (dir (if (stringp dir)
      ;;                      (concat
      ;;                       (if (consp dir) (car dir) dir)
      ;;                       "/styles/"

      ;;                       (unless (string-equal
      ;;                                style
      ;;                                (file-name-nondirectory
      ;;                                 (pathname-delete-trailing-/ dir)))
      ;;                           style)))))
      ;;         dir))

      (defun muse-meta-style-dirname (dir style)
        (let* ((dir (if (stringp dir)
                        (concat
                         (if (consp dir) (car dir) dir)
                         "/styles/"
                         (unless (string-equal
                                  style
                                  (file-name-nondirectory
                                   (pathname-delete-trailing-/ dir)))
                           style)))))
          dir))

      (defmacro muse-with-project-style (&rest body)
        `(let* ((muse-current-project (or (muse-project) (muse-read-project "Muse Project: " t t)))
                (muse-publishing-current-style (or muse-publishing-current-style
                                                   (cdr
                                                    (muse-publish-get-style
                                                     (mapcar (lambda (style)
                                                               (cons (muse-get-keyword :base style) style))
                                                             (cddr muse-current-project)))))))
           (message "muse-with-project-style: body:- %s muse-current-project:- %s" body muse-current-project)
           (message "muse-with-project-style: body:- %s muse-publishing-current-style:- %s" body muse-publishing-current-style)
           ,@body))


      (defmacro muse-with-project-style (&rest body)
        `(let ((muse-current-project
                (or (muse-project)
                    (muse-read-project "Muse Project: " t t))))
           (message "muse-with-project-style: muse-current-project %s" muse-current-project)
           (let ((muse-publishing-current-style
                  (or muse-publishing-current-style
                      (cdr
                       (muse-publish-get-style
                        (mapcar (lambda (style)
                                  (cons (muse-get-keyword :base style) style))
                                (cddr muse-current-project)))))))
             (message "muse-with-project-style: body:- %s muse-current-project:- %s" body muse-current-project)
             (message "muse-with-project-style: body:- %s muse-publishing-current-style:- %s" body muse-publishing-current-style)
             ,@body)))

      ;; (muse-publish-get-style
      ;;  (mapcar (lambda (style)
      ;;            (cons (muse-get-keyword :base style) style))
      ;;          (cddr (muse-read-project "Publish project: " t t))))

      ;; (ido-completing-read "sdfds" '(("aaa" "sdfds") ("ppp" "asfddsa")))


      ;; (mapcar (lambda (style)
      ;;            (cons (muse-get-keyword :base style) style))
      ;;          (cddr (muse-read-project "Publish project: " t t)))

      ;; (ido-completing-read "sdfads: "
      ;; '(("xhtml" :base "xhtml" :base-url (content-muse-publishing-url "notes/") :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/xhtml")
      ;;  ("my-pdf" :base "my-pdf" :base-url "http://hello.org//notes/" :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/my-pdf"))
      ;; )


      ;; muse-publishing-styles

      (defvar *muse-meta-style-dirname-fns*
        `(
          ("project-export"
           (:path-function (lambda ()
                             ;; (message "current-style %s " muse-publishing-current-style)
                             (if muse-publishing-current-style
                                 ;; (message "muse-publishing-current-style %s" muse-publishing-current-style)
                                 (or
                                  (muse-meta-style-dirname
                                   (or
                                    (plist-get muse-publishing-current-style :path)
                                    (when muse-current-output-style
                                      (plist-get muse-current-output-style :path)))
                                   (plist-get muse-publishing-current-style :base))
                                  'pass)
                                 'pass))))
          ("project"
           (:path-function (lambda ()
                             (if (and (muse-project)
                                      muse-publishing-current-style)
                                 (let* ((project-dir (cadr (muse-project)))
                                        (project-dir (if (consp project-dir) (car project-dir) project-dir)))
                                   ;; (message "(cadr (muse-project)) %s" project-dir)
                                   (muse-meta-style-dirname project-dir (plist-get muse-publishing-current-style :base)))
                                 'pass))))

          ("style"
           (:path-function (lambda ()
                             (if muse-publishing-current-style
                                 (muse-meta-style-dirname *muse-top-style-dir* (plist-get muse-publishing-current-style :base))
                                 'pass))))

          ("base"
           (:path-function *muse-top-style-dir*)))
        "*muse-meta-style-dirname-fns*")

      ;; *muse-top-style-dir*
      ;; "~/Documents/CreatedContent/contents/muse/generic/style"
      ;; (cadar *muse-meta-style-dirname-fns*)

      (defun mkdir-copy-file (src-file dst-file)
        (unless (file-exists-p dst-file)
          (progn
            (message "%s not exists" dst-file)
            (make-directory
             (expand-file-name
              (dirname-of-file dst-file)) t)
            (if (file-exists-p src-file)
                (copy-file src-file dst-file)
                (error "file %s not exists" src-file))))
        dst-file)

      ;; (defun lotus-muse-find-or-create-meta-file (filename &optional fnslist)
      ;;   "asfds"
      ;;   (let ((fnslist (or fnslist *muse-meta-style-dirname-fns*)))
      ;;     (muse-with-project-style
      ;;      (lotus-muse-find-or-create-meta-file-main filename fnslist))))

      (defun lotus-muse-find-or-create-meta-file (filename &optional dirfnslist)
        "asfds"
        (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
          (lotus-muse-find-or-create-meta-file-main filename dirfnslist)))

      (defun lotus-muse-find-or-create-meta-file-main (filename dirfnslist)
        "sdfds"
        ;; (message "calling lotus-muse-find-or-create-meta-file-main filename %s dirfnslist %s (cadar dirfnslist) %s" filename dirfnslist (cadar dirfnslist))
        (if dirfnslist
            (let* ((style-dirname-list (car dirfnslist))
                   (style-name (car style-dirname-list))
                   (strorfn (plist-get (cadr style-dirname-list) :path-function)))
              (let ((dirpath
                     (cond
                       ((functionp strorfn)
                        ;; (muse-with-project-style
                        (funcall strorfn))
                       ;; )
                       ((stringp strorfn)   strorfn)
                       ((and (symbolp strorfn)
                             (stringp (symbol-value strorfn)))
                        (symbol-value strorfn))
                       ((null strorfn) nil)
                       (t (error "wrong")))))

                (cond
                  ((and (stringp dirpath)
                        (file-directory-p dirpath))
                   (let ((filepath (progn
                                     ;; (message "dirfnslist %s" dirfnslist)
                                     ;; (message "fn %s list fns no %d retval %s" strorfn  (length dirfnslist) dirpath)
                                     (if (stringp dirpath) (expand-file-name filename dirpath)))))
                     ;; (message "filepath: %s" filepath)
                     ;; (unless dirpath
                     ;;   (error "can not get dirpath from style %s" style-name))
                     (if filepath
                         ;; (message "Xfilepath: %s" filepath)
                         (if (file-exists-p filepath)
                             filepath
                             (let ((parent-filepath (lotus-muse-find-or-create-meta-file-main filename (cdr dirfnslist))))
                               ;; (message "Have come here")
                               (if parent-filepath
                                   (if (file-exists-p parent-filepath)
                                       (progn
                                         (mkdir-copy-file parent-filepath filepath)
                                         filepath)
                                       (error "file %s did not got created for %s" parent-filepath filepath))
                                   (progn
                                     (message "You need to create %s file manually" filepath)
                                     (error "Can not file futher %s file now." filename))))))))
                  ((eq dirpath 'pass)
                   (lotus-muse-find-or-create-meta-file-main filename (cdr dirfnslist)))
                  (t (error "can not get dirpath %s from style %s" dirpath style-name)))))
            (error "can not get parent file for %s" filename)))

      (defun lotus-muse-get-meta-path-plist (&optional dirfnslist)
        "sdfds"
        (interactive)
        (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
          (remove-if-not '(lambda (c)
                           (stringp (cdr c)))
                         (mapcar
                          (lambda (fn-list)
                            (let ((name (car fn-list))
                                  (strfn (plist-get (cadr fn-list) :path-function)))
                              (cons name
                                    (cond
                                      ((if (symbolp strfn)
                                           (stringp (symbol-value strfn)))
                                       (symbol-value strfn))
                                      ((stringp strfn) strfn)
                                      ((functionp strfn) (funcall strfn))
                                      (t (error "error"))))))
                          dirfnslist))))

      (defun lotus-muse-edit-meta-file ()
        (interactive)
        (let* ((path-alist (lotus-muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
               (name
                (funcall muse-completing-read-function
                         "Get dir: " path-alist nil t))
               (path (cdr (assoc name path-alist))))
          (ido-find-file-in-dir path)))

      (defun lotus-muse-delete-meta-file ()
        (interactive)
        (let* ((path-alist (lotus-muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
               (name
                (funcall muse-completing-read-function
                         "Get dir: " path-alist nil t))
               (path (cdr (assoc name path-alist)))
               (delete-file (ido-read-file-name "delete muse meta file: " path)))
          (when (y-or-n-p (format "really delete %s :" delete-file))
            (delete-file delete-file)
            (message "file %s deleted." delete-file))))

      (defun muse-insert-css-link (media filename)
        (muse-make-css-link media
                            (file-relative-name
                             (lotus-muse-find-or-create-meta-file filename)
                             (plist-get muse-publishing-current-style :path))))

      (defun muse-insert-meta-file (filename)
        (get-string-from-file
         (lotus-muse-find-or-create-meta-file filename)))))

  (deh-section "org publishing utils"

    (defvar *org-top-dir*           (expand-file-name "contents/org" *created-content-dir*))
    (defvar *org-top-style-dir*     (expand-file-name "generic/org/style" *org-top-dir*))
    (defvar *org-generated-top-dir* (expand-file-name "gen/org" *created-content-dir*))
    (defvar *org-website-address*   (concat *website-address* "org/"))


    (defun* add-org-project (&rest project-spec)
      "Add org project."
      (interactive
       (let ((project-spec
              (read-org-project-spec)))))
      (if (member (car project-spec)
                  (mapcar 'car org-publish-project-alist))
          (if (or (not (called-interactively-p 'interactive))
                  (y-or-n-p (format "project %s already present, do you want to overwrite it?: " (car project-spec))))
              (progn
                (remove-org-project project-spec)
                (add-to-list 'org-publish-project-alist project-spec t)))
          (add-to-list 'org-publish-project-alist project-spec t)))

    (defun remove-org-project (&rest project-spec)
      (interactive
       (let ((project (ido-completing-read "Project: "
                                           (mapcar 'car org-publish-project-alist))))
         (if project
             (list project)
             (error "No project %s present" project))))
      (let ((project
             (cond
               ((and (consp project-spec)
                     (stringp (car project-spec)))
                (car project-spec))
               ((stringp project-spec)
                project-spec)
               (t nil))))
        (if project
            (setq org-publish-project-alist
                  (delete* project org-publish-project-alist
                           :key 'car
                           :test 'string-equal)))))

    ;; (defun* make-org-style-spec (org-dir publishing-path publishing-style publishing-url publishing-options)
    ;;   (interactive
    ;;    (let* ((org-dir (read-directory-name "Org Project Directory: " *org-top-dir*))
    ;;           (publishing-path
    ;;            (read-directory-name
    ;;             "Org Project Directory: "
    ;;             (concat *org-generated-top-dir* (replace-regexp-in-string *org-top-dir* "" org-dir))))
    ;;           (publishing-style
    ;;            (ido-completing-read "Org Publishing Style: " (mapcar 'car org-publishing-styles)))
    ;;           (publishing-url (read-from-minibuffer "Publishing Base URL: "))
    ;;           (publishing-options nil))
    ;;      (list org-dir publishing-path publishing-style publishing-url publishing-options)))
    ;;   (apply
    ;;    'org-publish-project-alist-styles
    ;;    (append
    ;;     (list
    ;;      org-dir
    ;;      publishing-path)
    ;;     (list publishing-style)
    ;;     (if publishing-url
    ;;         (list :base-url publishing-url))
    ;;     publishing-options)))

    (defun* make-org-project-spec (name &rest publishing-options)
      (interactive
       (let* ((name (read-from-minibuffer "Org Project Name: "))
              (org-dir
               (read-directory-name "Org Project Directory: " (concat *org-top-dir* "/" name)))
              (publishing-path
               (read-directory-name
                "Org Project Publishing Directory: "
                (concat *org-generated-top-dir*
                        (replace-regexp-in-string *org-top-dir* ""
                                                  (if (consp org-dir) (car org-dir) org-dir)))))
              (publishing-options nil))
         (list
          name
          :base-directory org-dir
          :publishing-directory publishing-path
          publishing-options)))
      `(
        ,name
        ,@publishing-options))

    ;; (defun* read-org-style-spec ()
    ;;   (let* ((org-dir (read-directory-name "Org Project Directory: " *org-top-dir*))
    ;;          (publishing-path
    ;;           (read-directory-name
    ;;            "Org Project Directory: "
    ;;            (concat *org-generated-top-dir* (replace-regexp-in-string *org-top-dir* "" org-dir))))
    ;;          (publishing-style
    ;;           (ido-completing-read "Org Publishing Style: " (mapcar 'car org-publishing-styles)))
    ;;          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
    ;;          (publishing-options nil))
    ;;     (list org-dir publishing-path publishing-style publishing-url publishing-options)))

    (defun* read-org-project-spec ()
      (let* ((name (read-from-minibuffer "Org Project Name: "))
             (org-dir
              (read-directory-name "Org Project Directory: " (concat *org-top-dir* "/" name)))
             (publishing-path
              (read-directory-name
               "Org Project Publishing Directory: "
               (concat *org-generated-top-dir*
                       (replace-regexp-in-string *org-top-dir* ""
                                                 (if (consp org-dir) (car org-dir) org-dir)))))
             (publishing-options nil))
        `(
          ,name
          :base-directory ,org-dir
          :publishing-directory ,publishing-path
          ,@publishing-options)))


    (defun content-org-dir (path)
        "thisandthat."
        (expand-file-name path *org-top-dir*))

    (defun content-org-publishing-dir (path)
        "thisandthat."
        (expand-file-name path *org-generated-top-dir*))

    (defun content-org-publishing-url (localpath)
      (concat *org-website-address* localpath))

    ;; (mapcar 'car org-publishing-styles)
    ;; (org-publish-project-alist-styles
    ;;  (concat *org-top-dir* "/doc/priv")
    ;;  (concat *org-generated-top-dir* "/doc/pdf/doc/priv/pdf")
    ;;  "pdf")

    ;; (org-publish-project-alist-styles
    ;;  (concat *org-top-dir* "/web/site/blog")
    ;;  (concat *org-generated-top-dir* "/web/site/blog/pdf")
    ;;  "ikiwiki"
    ;;  :base-url (concat *website-address* "/blog/"))
    )

  (deh-require-maybe (and org org-compat ox-publish)

    (defun org-publish-get-attribute (project extention attrib)
      (let ((proj-alist (assoc project org-publish-project-alist)))
        (or
         (plist-get (cdr proj-alist) attrib)
         (plist-get
          (cdar (remove-if-not
                 (lambda (p)
                   (string-match
                    (plist-get (cdr p) :base-extension)
                    extention))
                 (org-publish-expand-projects
                  (list proj-alist))))
          attrib))))
    ;;e.g.
    ;; (org-publish-get-attribute "journal" "org" :base-directory)

    (setq org-publish-project-alist nil)

    (add-org-project
     "inherit-org-info-js"
     ;; :base-directory "~/develop/org/Worg/code/org-info-js/"
     :base-directory (content-org-dir "org-info-js/")
     :recursive t
     :base-extension "js"
     :publishing-directory (content-org-publishing-dir "js/")
     :publishing-function 'org-publish-attachment)

    (add-org-project
     "inherit-org-info-js"
     ;; :base-directory "~/develop/org/Worg/code/org-info-js/"
     :base-directory (content-org-dir "org-info-js/")
     :recursive t
     :base-extension "js"
     :publishing-directory (content-org-publishing-dir "js/")
     :publishing-function 'org-publish-attachment)

    (add-org-project
     "org-notes"
     :base-directory (content-org-dir "org/")
     :base-extension "org"
     :publishing-directory (content-org-publishing-dir "org/html/")
     :recursive t
     :publishing-function 'org-html-publish-to-html
     :headline-levels 4             ; Just the default for this project.
     :auto-preamble t)

    (add-org-project
     "org-static"
     :base-directory (content-org-dir "org/")
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory (content-org-publishing-dir "org/html/")
     :recursive t
     :publishing-function 'org-publish-attachment)

    (add-org-project "org" :components '("org-notes" "org-static"))

    (add-org-project
     "generic-inherit"
     :base-directory (content-org-dir "generic/")
     :recursive t
     :base-extension "css\\|js"
     :publishing-directory (content-org-publishing-dir "generic/html/")
     :publishing-function 'org-publish-attachment)

    (add-org-project
     "generic-static"
     :base-directory (content-org-dir "generic/")
     :recursive t
     :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
     :publishing-directory (content-org-publishing-dir "generic/html/")
     :publishing-function 'org-publish-attachment)

    (add-org-project
     "generic-org"
     :base-directory (content-org-dir "generic/")
     :auto-index t
     :auto-sitemap t
     :sitemap-title "Sitemap"
     :index-filename "sitemap.org"
     :index-title "Sitemap"
     :recursive t
     :base-extension "org"
     :publishing-directory (content-org-publishing-dir "generic/html/")
     ;; :publishing-function org-publish-org-to-html
     :publishing-function 'org-html-publish-to-html
     :headline-levels 3
     :auto-preamble t)

    (add-org-project
     "journal-notes"
     :base-directory (content-org-dir "jorunal/")
     :auto-index t
     :auto-sitemap t
     :sitemap-title "Sitemap"
     :index-filename "sitemap.org"
     :index-title "Sitemap"
     :recursive t
     :base-extension "org"
     :publishing-directory (content-org-publishing-dir "journal/html/")
     ;; :publishing-function org-publish-org-to-html
     :publishing-function 'org-html-publish-to-html
     :headline-levels 3
     :with-section-numbers nil
     :table-of-contents nil
     :auto-preamble t
     :auto-postamble nil)

    (add-org-project
     "journal"
     :base-extension "org"
     :components '("inherit-org-info-js" "generic-inherit" "journal-notes" "generic-static"))

    (add-org-project
     "tasks-notes"
     :base-directory (content-org-dir "tasks/")
     :auto-index t
     :auto-sitemap t
     :sitemap-title "Sitemap"
     :index-filename "sitemap.org"
     :index-title "Sitemap"
     :recursive t
     :base-extension "org"
     :publishing-directory (content-org-publishing-dir "tasks/html/")
     ;; :publishing-function org-publish-org-to-html
     :publishing-function 'org-html-publish-to-html
     :headline-levels 3
     :with-section-numbers nil
     :table-of-contents nil
     :auto-preamble t
     :auto-postamble nil)

    (add-org-project
     "tasks"
     :base-extension "org"
     :components '("inherit-org-info-js" "generic-inherit" "tasks-notes" "generic-static"))

    (add-org-project
     "notes-notes"
     :base-directory (content-org-dir "notes/")
     :auto-index t
     :auto-sitemap t
     :sitemap-title "Sitemap"
     :index-filename "sitemap.org"
     :index-title "Sitemap"
     :recursive t
     :base-extension "org"
     :publishing-directory (content-org-publishing-dir "notes/html/")
     ;; :publishing-function org-publish-org-to-html
     :publishing-function 'org-html-publish-to-html
     :headline-levels 3
     :with-section-numbers nil
     :table-of-contents nil
     :auto-preamble t
     :auto-postamble nil)

    (add-org-project
     "notes"
     :base-extension "org"
     :components '("inherit-org-info-js" "generic-inherit" "notes-notes" "generic-static"))))

(provide 'publishing-config)
;;; publishing-config.el ends here
