;;; misc publishing.el --- misc publishing

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

(provide 'misc-publishing)

(require 'cl)
(require 'publishing)


(defvar *misc-top-dir*           (publishing-contents-top-dir 'misc))
(defvar *misc-top-style-dir*     (expand-file-name "generic/misc/style" (publishing-contents-top-dir 'misc)))

(defvar *misc-generated-top-dir* (publishing-generated-top-dir 'misc))
(defvar *misc-website-address*   (concat *website-address* "misc/"))

;;;###autoload
(defun* read-misc-style-spec ()
  (let* ((misc-dir (read-directory-name "Misc Project Directory: " *misc-top-dir*))
         (publishing-path
          (read-directory-name
           "Misc Project Directory: "
           (concat *misc-generated-top-dir* (replace-regexp-in-string *misc-top-dir* "" misc-dir))))
         (publishing-style
          (ido-completing-read "Misc Publishing Style: " (mapcar 'car misc-publishing-styles)))
         (publishing-url (read-from-minibuffer "Publishing Base URL: "))
         (publishing-options nil))
    (list misc-dir publishing-path publishing-style publishing-url publishing-options)))

;;;###autoload
(defun* read-misc-project-spec ()
  (let* ((name (read-from-minibuffer "Project Name: "))
         (misc-dirs
          (read-directory-name "Misc Project Directory: " (concat *misc-top-dir* "/" name)))
         (publishing-path
          (read-directory-name
           "Misc Project Directory: "
           (concat *misc-generated-top-dir*
                   (replace-regexp-in-string *misc-top-dir* ""
                                             (if (consp misc-dirs) (car misc-dirs) misc-dirs)))))
         (publishing-style
          (ido-completing-read "Misc Publishing Style: " (mapcar 'car misc-publishing-styles)))
         (publishing-url (read-from-minibuffer "Publishing Base URL: "))
         (publishing-options nil))
    `(,name
      ,@(make-misc-style-spec
         (if (consp misc-dirs) (car misc-dirs) misc-dirs)
         publishing-path
         publishing-style
         publishing-url
         publishing-options))))

;;;###autoload
(defun add-misc-project (project-spec)
  "Add misc project."
  (interactive
   (let ((project-spec
          (read-misc-project-spec)))))
  (if (member (car project-spec)
              (mapcar 'car misc-project-alist))
      (if (or (not (called-interactively-p 'interactive))
              (y-or-n-p (format "project %s already present, do you want to overwrite it?: " (car project-spec))))
          (progn
            (remove-misc-project project-spec)
            (add-misc-project project-spec)))
      (add-to-list 'misc-project-alist project-spec)))

;;;###autoload
(defun remove-misc-project (project-spec)
  (interactive
   (let ((project (ido-completing-read "Project: "
                                       (mapcar 'car misc-project-alist))))
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
        (setq misc-project-alist
              (delete* project misc-project-alist
                       :key 'car
                       :test 'string-equal)))))

;;;###autoload
(defun* make-misc-style-spec (misc-dir publishing-path publishing-style publishing-url &rest publishing-options)
  (interactive
   (let* ((misc-dir (read-directory-name "Misc Project Directory: " *misc-top-dir*))
          (publishing-path
           (read-directory-name
            "Misc Project Directory: "
            (concat *misc-generated-top-dir* (replace-regexp-in-string *misc-top-dir* "" misc-dir))))
          (publishing-style
           (ido-completing-read "Misc Publishing Style: " (mapcar 'car misc-publishing-styles)))
          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
          (publishing-options nil))
     (list misc-dir publishing-path publishing-style publishing-url publishing-options)))
  (apply
   'misc-project-alist-styles
   (append
    (list
     misc-dir
     publishing-path)
    (list publishing-style)
    (if publishing-url
        (list :base-url publishing-url))
    publishing-options)))

;;;###autoload
(defun* make-misc-project-spec (name misc-dirs publishing-path publishing-style publishing-url &rest publishing-options)
  (interactive
   (let* ((name (read-from-minibuffer "Project Name: "))
          (misc-dirs
           (read-directory-name "Misc Project Directory: " (concat *misc-top-dir* "/" name)))
          (publishing-path
           (read-directory-name
            "Misc Project Directory: "
            (concat *misc-generated-top-dir*
                    (replace-regexp-in-string *misc-top-dir* ""
                                              (if (consp misc-dirs) (car misc-dirs) misc-dirs)))))
          (publishing-style
           (ido-completing-read "Misc Publishing Style: " (mapcar 'car misc-publishing-styles)))
          (publishing-url (read-from-minibuffer "Publishing Base URL: "))
          (publishing-options nil))
     (list name misc-dirs publishing-path publishing-style publishing-url publishing-options)))
  `(,name
    ,@(make-misc-style-spec
       (if (consp misc-dirs) (car misc-dirs) misc-dirs)
       publishing-path
       publishing-style
       publishing-url
       publishing-options)))

;;;###autoload
(defun content-misc-dir (path)
  "thisandthat."
  (publishing-created-contents-dir 'misc path))

;;;###autoload
(defun content-misc-publishing-dir (path)
  "thisandthat."
  (publishing-generated-contents-dir 'misc path))

;;;###autoload
(defun content-misc-publishing-url (localpath)
  (publishing-website-address 'misc path)
  (concat *misc-website-address* "/" localpath))

;;; misc-publishing.el ends here
