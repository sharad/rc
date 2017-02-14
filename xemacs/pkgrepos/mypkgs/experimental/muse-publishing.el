;;; muse publishing.el --- muse publishing

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

(provide 'muse-publishing)

(require 'cl)
(require 'publishing)
(require 'muse)
(require 'muse-project)


(defvar *muse-top-dir*           (expand-file-name "contents/muse" *created-content-dir*))
(defvar *muse-top-style-dir*     (expand-file-name "generic/muse/style" *muse-top-dir*))

(defvar *muse-generated-top-dir* (expand-file-name "gen/muse" *created-content-dir*))
(defvar *muse-website-address*   (concat *website-address* "muse/"))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun content-muse-dir (path)
  "thisandthat."
  (expand-file-name path *muse-top-dir*))

;;;###autoload
(defun content-muse-publishing-dir (path)
  "thisandthat."
  (expand-file-name path *muse-generated-top-dir*))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun sharad/muse-find-or-create-meta-file (filename &optional dirfnslist)
  "asfds"
  (let ((dirfnslist (or dirfnslist *muse-meta-style-dirname-fns*)))
    (sharad/muse-find-or-create-meta-file-main filename dirfnslist)))

;;;###autoload
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

;;;###autoload
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

;;;###autoload
(defun sharad/muse-edit-meta-file ()
  (interactive)
  (let* ((path-alist (sharad/muse-get-meta-path-plist *muse-meta-style-dirname-fns*))
         (name
          (funcall muse-completing-read-function
                   "Get dir: " path-alist nil t))
         (path (cdr (assoc name path-alist))))
    (ido-find-file-in-dir path)))

;;;###autoload
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

;;;###autoload
(defun muse-insert-css-link (media filename)
  (muse-make-css-link media
                      (file-relative-name
                       (sharad/muse-find-or-create-meta-file filename)
                       (plist-get muse-publishing-current-style :path))))

;;;###autoload
(defun muse-insert-meta-file (filename)
  (get-string-from-file
   (sharad/muse-find-or-create-meta-file filename)))

;;; muse-publishing.el ends here
