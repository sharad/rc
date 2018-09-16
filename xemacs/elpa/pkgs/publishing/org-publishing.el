;;; org-publishing.el --- org publishing

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

(provide 'org-publishing)

(require 'cl)
(require 'publishing)
(require 'org)
(when (featurep 'org-compact)
 (require 'org-compact))
(require 'ox-publish)

(defvar *org-top-dir*           (publishing-created-contents-dir 'org))
(defvar *org-top-style-dir*     (publishing-created-contents-dir 'org "generic/org/style"))
(defvar *org-generated-top-dir* (publishing-generated-contents-dir 'org))
*org-generated-top-dir*
"/home/s/hell/Documents/CreatedContent/gen/org"

"/home/s/hell/Documents/CreatedContent/gen/virtual/org/default"

(defvar *org-website-address*   (concat *website-address* "org/"))


;;;###autoload
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

;;;###autoload
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

;;;###autoload
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

;;;###autoload
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


;;;###autoload
(defun content-org-dir (path)
    "thisandthat."
    (expand-file-name path *org-top-dir*))

;;;###autoload
(defun content-org-publishing-dir (path)
    "thisandthat."
    (expand-file-name path *org-generated-top-dir*))

;;;###autoload
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

;;;###autoload
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

;;; org-publishing.el ends here
