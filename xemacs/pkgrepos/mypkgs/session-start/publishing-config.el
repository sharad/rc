;;; publishing-config.el --- publishing configuration

;; Copyright (C) 2015  sharad

;; Author: sharad <spratap@merunetworks.com>
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


(deh-require-maybe (and muse muse-project)
  (defvar *doc-root* "~/Documents")
  (defvar *created-content-dir* (concat *doc-root* "/CreatedContent"))
  (defvar *muse-top-dir* (concat *created-content-dir* "/contents/muse"))
  (defvar *muse-top-style-dir* (concat *muse-top-dir* "/generic/muse/style"))
  (defvar *muse-generated-top-dir* (concat *created-content-dir* "/gen/muse"))
  (defvar *org-top-dir* (concat *created-content-dir* "/contents/org"))
  (defvar *org-top-style-dir* (concat *org-top-dir* "/generic/org/style"))
  (defvar *org-generated-top-dir* (concat *created-content-dir* "/gen/org"))
  (defvar *website-address* "http://hello.org/")

  (deh-section "Muse Projects"

    (defun* make-muse-style-spec (muse-dir publishing-path publishing-style publishing-url publishing-options)
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

    (defun* make-muse-project-spec (name muse-dirs publishing-path publishing-style publishing-url publishing-options)
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





    ;; Here is my master project listing.

    (add-muse-project
     `("Website"
       ( ,(concat *muse-top-dir* "/web/site/wiki/web")
          ,(concat *muse-top-dir* "/web/site/wiki/web/testdir")
          :force-publish ("WikiIndex")
          :default "WelcomePage")
       (:base "my-xhtml"
              :base-url ,(concat *website-address* "/web/")
              :include "/web/[^/]+"
              :path ,(concat *muse-generated-top-dir* "/web/site/wiki/web/my-xhtml"))
       (:base "my-xhtml"
              :base-url ,(concat *website-address* "/web/")
              :include "/testdir/[^/]+"
              :path ,(concat *muse-generated-top-dir* "/web/site/wiki/web/testdir/my-xhtml"))
       (:base "my-pdf"
              :base-url ,(concat *website-address* "/web/")
              :path ,(concat *muse-generated-top-dir* "/doc/pdf/site/wiki/web/my-pdf")
              :include "/\\(CurriculumVitae\\|BriefResume\\)[^/]*$")))

    (add-muse-project
     `("Projects" ( ,(concat *muse-top-dir* "/web/site/wiki/projects")
                     :force-publish ("WikiIndex" "MuseQuickStart")
                     :default "WelcomePage")
                  (:base "my-xhtml"
                         :base-url ,(concat *website-address* "/projects/")
                         :path ,(concat *muse-generated-top-dir* "/web/site/wiki/projects/my-xhtml"))))

    (add-muse-project
     `("WikiWriting"
       ( ,(concat *muse-top-dir* "/web/site/wiki/writing")
          ;; :force-publish ("WikiIndex" "MuseQuickStart")
          :major-mode muse-mode
          :default "index")
       (:base "my-xhtml"
              :base-url ,(concat *website-address* "/projects/")
              :path ,(concat *muse-generated-top-dir* "/web/site/wiki/writing/my-xhtml"))))


    (add-muse-project
     `("Blog" (,@(muse-project-alist-dirs (concat *muse-top-dir* "/web/site/blog"))
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
                 (concat *muse-top-dir* "/web/site/blog")
                 (concat *muse-generated-top-dir* "/web/site/blog/ikiwiki")
                 "ikiwiki"
                 :base-url (concat *website-address* "/blog/"))))

    ;; "http://grepfind.hello.org/blog/"
    (add-muse-project
     `("MyNotes" (,(concat *muse-top-dir* "/web/site/wiki/notes")
                   :force-publish ("index")
                   :default "index")
                 (:base "xhtml"
                        :base-url (concat *website-address* "/notes/")
                        :path ,(concat *muse-generated-top-dir* "/web/site/wiki/notes/xhtml"))
                 (:base "my-pdf"
                        :base-url ,(concat *website-address* "/notes/")
                        :path ,(concat *muse-generated-top-dir* "/web/site/wiki/notes/my-pdf"))))

    (add-muse-project
     `("_Private" (,(concat *muse-top-dir* "/doc/priv"))
                  ,@(muse-project-alist-styles (concat *muse-top-dir* "/doc/priv")
                                               (concat *muse-generated-top-dir* "/doc/pdf/doc/priv/pdf")
                                               "pdf")))

    (add-muse-project
     `("_Classes" (,@(muse-project-alist-dirs (concat *muse-top-dir* "/web/site/wiki/classes"))
                     :default "index")
                  ,@(muse-project-alist-styles (concat *muse-top-dir* "/web/site/wiki/classes")
                                               (concat *muse-generated-top-dir* "/web/site/wiki/classes/xhtml")
                                               "xhtml")))

    (add-muse-project
     `("MA366" (,(concat *muse-top-dir* "/doc/pdf/classes/ma366"))
               (:base "pdf-uh"
                      :path ,(concat *muse-generated-top-dir* "/doc/pdf/classes/ma366/pdf-uh"))))

    (add-muse-project
     `("ENGL238" (,(concat *muse-top-dir* "/doc/pdf/classes/eng238"))
                 (:base "pdf-uh"
                        :path ,(concat *muse-generated-top-dir* "/doc/pdf/classes/eng238/pdf-uh"))))

    (add-muse-project
     `("CS426" (,(concat *muse-top-dir* "/web/site/wiki/classes/cs426"))
               (:base "pdf-uh"
                      :path "~/proj/classes/cs426/pdf-uh")))

    (add-muse-project
     `("_Plans" (,(concat *muse-top-dir* "/web/site/wiki/plans")
                  :default "TaskPool"
                  :major-mode planner-mode
                  :visit-link planner-visit-link)
                (:base "planner-xhtml"
                       :path ,(concat *muse-generated-top-dir* "/web/site/wiki/plans/html")))))

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
      `(let ((muse-current-project (or (muse-project) (muse-read-project "Muse Project: " t t))))
         (message "muse-with-project-style: muse-current-project %s" muse-current-project)
         (let ((muse-publishing-current-style (or muse-publishing-current-style
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
    ;; '(("xhtml" :base "xhtml" :base-url (concat *website-address* "/notes/") :path "~/Documents/CreatedContent/gen/web/site/wiki/notes/xhtml")
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

    ;; (defun sharad/muse-find-or-create-meta-file (filename &optional fnslist)
    ;;   "asfds"
    ;;   (let ((fnslist (or fnslist *muse-meta-style-dirname-fns*)))
    ;;     (muse-with-project-style
    ;;      (sharad/muse-find-or-create-meta-file-main filename fnslist))))

    (defun sharad/muse-find-or-create-meta-file (filename &optional dirfnslist)
      "asfds"
      (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
        (sharad/muse-find-or-create-meta-file-main filename dirfnslist)))

    (defun sharad/muse-find-or-create-meta-file-main (filename dirfnslist)
      "sdfds"
      ;; (message "calling sharad/muse-find-or-create-meta-file-main filename %s dirfnslist %s (cadar dirfnslist) %s" filename dirfnslist (cadar dirfnslist))
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
                           (let ((parent-filepath (sharad/muse-find-or-create-meta-file-main filename (cdr dirfnslist))))
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
                 (sharad/muse-find-or-create-meta-file-main filename (cdr dirfnslist)))
                (t (error "can not get dirpath %s from style %s" dirpath style-name)))))
          (error "can not get parent file for %s" filename)))

    (defun sharad/muse-get-meta-path-plist (&optional dirfnslist)
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

    (defun sharad/muse-edit-meta-file ()
      (interactive)
      (let* ((path-alist (sharad/muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
             (name
              (funcall muse-completing-read-function
                       "Get dir: " path-alist nil t))
             (path (cdr (assoc name path-alist))))
        (ido-find-file-in-dir path)))

    (defun sharad/muse-delete-meta-file ()
      (interactive)
      (let* ((path-alist (sharad/muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
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
                           (sharad/muse-find-or-create-meta-file filename)
                           (plist-get muse-publishing-current-style :path))))

    (defun muse-insert-meta-file (filename)
      (get-string-from-file
       (sharad/muse-find-or-create-meta-file filename)))))



(deh-require-maybe (and org org-compat ox-publish)

  (setq org-publish-project-alist
        `(("org-notes"
           :base-directory ,(expand-file-name "org/" *org-top-dir*)
           :base-extension "org"
           :publishing-directory ,(expand-file-name "org/html/" *org-generated-top-dir*)
           :recursive t
           :publishing-function org-html-publish-to-html
           :headline-levels 4             ; Just the default for this project.
           :auto-preamble t)

          ("org-static"
           :base-directory ,(expand-file-name "org/" *org-top-dir*)
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory ,(expand-file-name "org/html/" *org-generated-top-dir*)
           :recursive t
           :publishing-function org-publish-attachment)

          ("org" :components ("org-notes" "org-static"))

          ("inherit-org-info-js"
           ;; :base-directory "~/develop/org/Worg/code/org-info-js/"
           :base-directory ,(expand-file-name "org-info-js/" *org-top-dir*)
           :recursive t
           :base-extension "js"
           :publishing-directory ,(expand-file-name "js/" *org-generated-top-dir*)
           :publishing-function org-publish-attachment)

          ("B-inherit"
           :base-directory ,(expand-file-name "B/" *org-top-dir*)
           :recursive t
           :base-extension "css\\|js"
           :publishing-directory ,(expand-file-name "B/html/" *org-generated-top-dir*)
           :publishing-function org-publish-attachment
           )

          ("B-org"
           :base-directory ,(expand-file-name "B/" *org-top-dir*)
           :auto-index t
           :index-filename "sitemap.org"
           :index-title "Sitemap"
           :recursive t
           :base-extension "org"
           :publishing-directory ,(expand-file-name "B/html/" *org-generated-top-dir*)
           :publishing-function org-publish-org-to-html
           :headline-levels 3
           :auto-preamble t
           )
          ("B-static"
           :base-directory ,(expand-file-name "B/" *org-top-dir*)
           :recursive t
           :base-extension "css\\|js\\|png\\|jpg\\|gif\\|pdf\\|mp3\\|ogg\\|swf"
           :publishing-directory ,(expand-file-name "B/html/" *org-generated-top-dir*)
           :publishing-function org-publish-attachment)

          ("B" :components ("inherit-org-info-js" "B-inherit" "B-notes" "B-static")))))


(provide 'publishing-config)
;;; publishing-config.el ends here
