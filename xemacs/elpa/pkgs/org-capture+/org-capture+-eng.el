;;; org-capture+-eng.el --- org capture plus eng     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <>
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

(provide 'org-capture+-eng)


(require 'org-capture+-helm-dynamic)


(defvar org-capture+-learned-templates nil)

;; checkout org-capture-templates variable
(defvar org-capture+-types   '(("Org Entry" . entry)
                               ("List Item" . item)
                               ("Checklist Item" . chckitem)
                               ("Table Line" . table-line)
                               ("Plain Text" . plain)
                               ("Log note" . log-note)))
(defvar org-capture+-target-names '(("File" . file)
                                    ("Org entry Id" . id)
                                    ("File and Headline Regular Expression". file+regexp)
                                    ("File Headline" . file+headline)
                                    ("File Outline path" . file+olp)
                                    ("File Outline path Date-tree" . file+olp+datetree)
                                    ("File and Function to find Headline" . file+function)
                                    ("Current Org Clock" . clock)
                                    ("Function" . function)
                                    ("Marker" . marker)))


(defun ptree-get (tree &rest keys)
  (let ((key (car keys)))
    (if (and key
             (consp tree))
        (apply #'ptree-get (plist-get tree key) (cdr keys))
      (unless (cdr keys) tree))))

(defun ptree-put (tree value &rest keys)
  (let ((key (car keys)))
    (if (and key
             (or (null tree)
                 (consp tree)))
        (setq tree
              (plist-put tree key
                         (if (cdr keys)
                             (apply #'ptree-put (plist-get tree key) value (cdr keys))
                           value))))))

(defun ptree-keys (tree)
  (mapcar #'(lambda (key)
              (let ((subtree (plist-get tree key)))
                (message "key: %s subtree: %s" key subtree)
                (if (and (consp subtree)
                         (keywordp (car subtree)))
                    (cons key (ptree-keys subtree))
                  (list key))))
          (plist-get-keys tree)))

(defun ptree-key-lists-keys (ptree-keys)
  (letrec ((dfs #'(lambda (tree)
                    (if (cdr tree)
                        (mapcar #'(lambda (l)
                                    (cons (car tree) (car (depth-first-search l))))
                                (cdr tree))
                      (list tree)))))
    (mapcan dfs ptree-keys)))

(defun ptree-key-lists (ptree-keys)
  (ptree-key-lists-keys (ptree-keys ptree-keys)))


(when nil
    (ptree-key-lists-keys
     '((:type)
       (:target (:file) (:name))
       (:target (:file (:x)) (:name (:y)))
       (:description)))

    (ptree-put '(:a (:b (:c (:d e)))) 'x :a :b :c :d)

    (ptree-keys '(:type entry :target (:file /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/personal/report.org :name Test :headings ("x" "y"))))

    (ptree-key-lists '(:type entry :target (:file /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/personal/report.org :name Test))))



(defun org-capture-helm-action (ptree value &rest keys)
  (apply #'ptree-put ptree value keys))


(defun org-select-targets (&rest targets)
  (remove-if-not #'(lambda (trg)
                     (memq (cdr trg) targets))
                 org-capture+-target-names))


(defun org-capture+-build-target-arg (ptree)
  (let ((name      (ptree-get ptree :target :name))
        (file      (ptree-get ptree :target :file))
        (headlines (ptree-get ptree :target :headlines))
        (function  (ptree-get ptree :target :function))
        (marker    (ptree-get ptree :target :marker)))
    (case name
      (file              (list name file))
      (id                (list name id))
      (file+headline     (list name file (car (last headlines))))
      (file+olp          (append (list name file) headlines))
      (file+olp+datetree (append (list name file) headlines))
      (file+function     (list name function))
      (clock)            (list name)
      (function          (list name function))
      (marher            (list name marker)))))

(defun org-capture+-build-arg (ptree)
  (let ((type     (ptree-get ptree :type))
        (template (ptree-get ptree :template)))
    (list type
          (org-capture+-build-target-arg ptree)
          template)))

(defun org-capture+-run-ptree (ptree)
  (apply #'org-capture-run
         (org-capture+-build-arg ptree)))


;; cons of name value
(defun org-capture+-get-file-headlines (file match &rest headlines)
  (when (file-exists-p file)
    (with-current-buffer (find-file-noselect file)
      (let ((m (org-find-olp (cons file headlines))))
        (if (markerp m)
            (goto-char m)
          (when headlines
            (error "Can not find headlines %s in file %s"
                   headlines
                   file)))
        (org-map-entries #'(lambda ()
                             ;; https://emacs.stackexchange.com/questions/41870/org-mode-retrieve-current-heading-and-parents-programmatically
                             (let* ((level   (org-element-property :level (org-element-at-point)))
                                    (prefix  (concat (make-string level ?\*) " "))
                                    (headline-tags   (org-get-heading))
                                    (headline-notags (substring-no-properties (org-get-heading 'no-tags))))
                               (cons (org-fontify-like-in-org-mode (concat prefix headline-tags))
                                     (org-get-outline-path t))))
                         match
                         (if m 'tree 'file))))))

(defun org-capture+-get-org-files ()
  org-agenda-files)

(defun org-capture+-get-markers ())

(defun org-capture+-get-org-entry-id ()
  ())

(defun org-capture+-get-file-functions ()
  ())

(defun org-capture+-get-functions ()
  ())


(defun org-capture+-filter-types (ptree)
  org-capture+-types)

(defun org-capture+-target-name-filter (ptree)
  (let* ((file      (ptree-get ptree :target :file))
         (headlines (ptree-get ptree :target :headlines)))
    (if file
        (apply #'org-select-targets
               (if headlines
                   (if (> (length headlines) 1)
                       '(file+olp file+olp+datetree)
                     '(file+headline file+olp file+olp+datetree))
                 '(file file+headline file+olp file+olp+datetree)))
      org-capture+-target-names)))

(defun org-capture+-target-files-filter (ptree)
  (let ((name (ptree-get ptree :target :name)))
    (when (memq name
                '(nil file file+headline file+olp file+olp+datetree))
      (org-capture+-get-org-files))))

(defun org-capture+-target-file+headlines-filter (ptree)
  (let* ((file      (ptree-get ptree :target :file))
         (headlines (ptree-get ptree :target :headlines)))
    (when (and file
               (null headlines))
      (apply #'org-capture+-get-file-headlines file t headlines))))


(defun org-capture+-type-source (ptree)
  (let ((types (org-capture+-filter-types ptree)))
    (helm-build-sync-source "Type"
      :candidates types
      :action     #'(lambda (type)
                      (org-capture+-guided (org-capture-helm-action ptree type :type))))))

(defun org-capture+-target-name-source (ptree)
  (let ((targets (org-capture+-target-name-filter ptree)))
    (helm-build-sync-source "Target"
      :candidates targets
      :action     #'(lambda (name)
                      (org-capture+-guided (org-capture-helm-action ptree name :target :name))))))

(defun org-capture+-target-file-source (ptree)
  (let ((files (org-capture+-target-files-filter ptree)))
    (helm-build-sync-source "Files"
      :candidates files
      :action     #'(lambda (file)
                      (org-capture+-guided (org-capture-helm-action ptree file :target :file))))))

(defun org-capture+-target-file+headlines-source (ptree)
  (let ((headlines       (org-capture+-target-file+headlines-filter ptree))
        (headline-action #'(lambda (headlines)
                             (org-capture+-guided (org-capture-helm-action ptree headlines :target :headlines)))))
    (helm-build-sync-source "Headline"
      :candidates headlines
      :action (list (cons "Select" headline-action)))))

(defun org-capture+-template-source (ptree)
  (helm-template-gen-source #'org-capture+-tree-predicate
                            '(t xx yy)
                            0
                            #'(lambda (template)
                                (org-capture+-guided (org-capture-helm-action ptree template :template)))))

(defun org-capture+-description-source (ptree)
  (let (description)
    (helm-build-dummy-source "Description"
      :action     #'(lambda (description)
                      (org-capture+-guided (org-capture-helm-action ptree description :description))))))


(defun org-capture+-reset-candidates (ptree)
  (let ((key-lists (ptree-key-lists ptree)))
    (message "key-lists: %s" key-lists)
    (message "ptree: %s" ptree)
    (mapcar #'(lambda (key-list)
                (cons (format "%s: %s" key-list (apply #'ptree-get ptree key-list))
                      key-list))
            key-lists)))

(defun org-capture+reset-source (ptree)
  (let ((candidates (org-capture+-reset-candidates ptree)))
    (helm-build-sync-source "Reset"
      :candidates candidates
      :multiline t
      :action     #'(lambda (keys)
                      (org-capture+-guided (apply #'ptree-put ptree nil keys))))))


;; TODO: Add resets which will help to edit existing
;;       take new as editing an anonymous

;;;###autoload
(defun org-capture+-guided (&optional ptree)
  (interactive)
  (let (sources
        (reset-source (list (org-capture+reset-source ptree))))

    (progn
      (progn
        (unless (ptree-get ptree :target :name)
          (push (org-capture+-target-name-source ptree)
                sources))
        (unless (ptree-get ptree :target :file)
          (push (org-capture+-target-file-source ptree)
                sources))
        (unless (ptree-get ptree :target :headlines)
          (push (org-capture+-target-file+headlines-source ptree)
                sources)))

      (unless (ptree-get ptree :type)
        (push (org-capture+-type-source        ptree)
              sources))
      (unless (ptree-get ptree :description)
        (push (org-capture+-description-source ptree)
              sources))
      (unless (ptree-get ptree :template)
        (setq sources
              (nconc sources (org-capture+-template-source ptree))))

      (if sources
          (helm :sources (append sources reset-source))
        (org-capture+-run-ptree ptree)
        (message "ptree %s" ptree)))))

;;;###autoload
(defalias 'org-capture+ #'org-capture+-guided)


(defun self-insert-test ()
  (interactive)
  (message "Hello")
  (self-insert-command 1))

;;; org-capture+-eng.el ends here
