;;; publishing.el --- publishing configuration

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

(provide 'publishing)

(defvar *doc-root*            (expand-file-name "Documents" "~"))
(defvar *created-content-dir* (expand-file-name "CreatedContent" *doc-root*))
(defvar *website-address*     "http://emacs-publishing.org/")


;; ;; Add function to set all four
;; (defvar *misc-top-dir*           (expand-file-name "contents/virtual/misc/default" *created-content-dir*))
;; (defvar *misc-top-style-dir*     (expand-file-name "generic/misc/style" *misc-top-dir*))
;; (defvar *misc-generated-top-dir* (expand-file-name "gen/misc" *created-content-dir*))
;; (defvar *misc-website-address*   (concat *website-address* "misc/"))
;; ;; variables

;;;###autoload
(defun default-publishing-document-root-dir ()
  *doc-root*)

;;;###autoload
(defun default-publishing-document-created-contents-dir ()
  (expand-file-name "CreatedContent/contents" (publishing-document-root-path)))

;;;###autoload
(defun default-publishing-document-generated-contents-dir ()
  (expand-file-name "CreatedContent/gen" (publishing-document-root-path)))

(defun default-publishing-document-website-address ()
  *website-address*)

(defalias 'publishing-document-root-path               #'default-publishing-document-root-path)
(defalias 'publishing-document-created-contents-dir   #'default-publishing-document-created-contents-path)
(defalias 'publishing-document-generated-contents-dir #'default-publishing-document-generated-contents-dir)
(defalias 'publishing-document-website-address        #'default-publishing-document-website-address)

;;;###autoload
(defun default-publishing-class-created-contents-path (class &optional path)
  (let ((path (or path "")))
    (expand-file-name
     path
     (expand-file-name
      "default"
      (expand-file-name
       (symbol-name class)
       (expand-file-name
        "virtual"
        (publishing-document-created-contents-path)))))))

;;;###autoload
(defun default-publishing-class-generated-contents-dir (class &optional path)
  (let ((path (or path "")))
    (expand-file-name
     path
     (expand-file-name
      "default"
      (expand-file-name
       (expand-file-name
        (symbol-name class)
        (expand-file-name
         "virtual"
         (publishing-document-generated-contents-dir))))))))

;;;###autoload
(defun default-publishing-class-website-address (class &optional path)
  (concat
   (publishing-document-website-address)
   "/"
   (symbol-name class)
   (if path (concat "/" path))))


(defalias 'publishing-class-created-contents-path   #'default-publishing-class-created-contents-path)
(defalias 'publishing-class-generated-contents-dir #'default-publishing-class-generated-contents-dir)
(defalias 'publishing-class-website-address        #'default-publishing-class-website-address)

;;;###autoload
(defun default-publishing-created-contents-path (&optional class path)
  (if class
      (publishing-class-created-contents-path class path)
    (publishing-document-created-contents-path)))

;;;###autoload
(defun default-publishing-generated-contents-dir (&optional class path)
  (if class
      (publishing-class-generated-contents-dir class path)
    (publishing-document-generated-contents-dir)))

;;;###autoload
(defun default-publishing-website-address (&optional class path)
  (if class
      (publishing-class-website-address class path)
    (publishing-document-website-address)))


(defalias 'publishing-created-contents-path   #'default-publishing-created-contents-path)
(defalias 'publishing-generated-contents-dir #'default-publishing-generated-contents-path)
(defalias 'publishing-website-address        #'default-publishing-website-address)





;;; publishing.el ends here
