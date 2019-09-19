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
(defvar org-capture+-meta-data nil)

(defvar org-capture+-plist '(:type nil
                             :target (:file     nil
                                      :name     nil
                                      :headings nil)
                             :template nil
                             :description nil))


(defun plist-keys (plist)
  "Return keys in a PLIST."
  (-slice plist 0 nil 2))

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
          (plist-keys tree)))

(defun ptree-key-lists-keys (ptree-keys)
  (letrec ((dfs #'(lambda (tree)
                    (if (cdr tree)
                        (mapcar #'(lambda (l)
                                    (cons (car tree) (car (funcall dfs l))))
                                (cdr tree))
                      (list tree)))))
    (mapcan dfs ptree-keys)))

(defun ptree-key-lists (ptree-keys)
  (ptree-key-lists-keys (ptree-keys ptree-keys)))


(defun org-capture+-meta--get (&rest keys)
  (apply #'ptree-get org-capture+-meta-data keys))

(defun org-capture+-meta--put (value &rest keys)
  (setq org-capture+-meta-data
        (apply #'ptree-put org-capture+-meta-data value keys)))

(defun org-capture+-meta-get (keys key)
  (apply #'ptree-get org-capture+-meta-data (append keys (list key))))

(defun org-capture+-meta-put (value keys key)
  (setq org-capture+-meta-data
        (apply #'ptree-put org-capture+-meta-data value (append keys (list key)))))


(defun org-capture+-helm-common-action (ptree &rest keys)
  #'(lambda (candidate)
      (org-capture+-guided (apply #'ptree-put ptree candidate keys))))


(defun org-capture+-helm-sync (ptree &rest keys)
  (let* ((prompt     (org-capture+-meta-get keys :prompt))
         (filter     (org-capture+-meta-get keys :filter))
         (candidates (when filter (funcall filter ptree))))
    (list
     (helm-build-sync-source prompt
       :candidates candidates
       :action     (apply #'org-capture+-helm-common-action ptree keys)))))

(defun org-capture+-helm-dummy (ptree &rest keys)
  (let* ((prompt     (org-capture+-meta-get keys :prompt))
         (filter     (org-capture+-meta-get keys :filter))
         (candidates (when filter (funcall filter ptree))))
    (list
     (helm-build-dummy-source prompt
       :action     (apply #'org-capture+-helm-common-action ptree keys)))))

(defun org-capture+-helm-gen (ptree &rest keys)
  (let* ((prompt     (org-capture+-meta-get keys :prompt))
         (filter     (org-capture+-meta-get keys :filter))
         (candidates (when filter (funcall filter ptree))))
    (helm-template-gen-source #'org-capture+-tree-predicate
                              '(t xx yy)
                              0
                              (apply #'org-capture+-helm-common-action ptree keys))))


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
      (marker            (list name marker)))))

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


(defmacro define-org-capture+-filter (tree-keys &rest body)
  (destructuring-bind (ptree . keys) tree-keys
   `(let ((fn #'(lambda (,ptree) ,@body)))
      (org-capture+-meta-put fn ',keys :filter))))
(put 'define-org-capture+-filter 'lisp-indent-function 1)


(org-capture+-meta--put "Types" :type :prompt)
(org-capture+-meta--put "Files" :target :file :prompt)
(org-capture+-meta--put "Targets" :target :name :prompt)
(org-capture+-meta--put "Headings" :target :headings :prompt)
(org-capture+-meta--put "Description" :description :prompt)
(org-capture+-meta--put "Templates" :template :prompt)

(org-capture+-meta--put #'org-capture+-helm-sync :type :source)
(org-capture+-meta--put #'org-capture+-helm-sync :target :file :source)
(org-capture+-meta--put #'org-capture+-helm-sync :target :name :source)
(org-capture+-meta--put #'org-capture+-helm-sync :target :headings :source)
(org-capture+-meta--put #'org-capture+-helm-dummy :description :source)
(org-capture+-meta--put #'org-capture+-helm-gen :template :source)

(org-capture+-meta--put 'single :type :alignment)
(org-capture+-meta--put 'multi :target :file :alignment)
(org-capture+-meta--put 'single :target :name :alignment)
(org-capture+-meta--put 'single :target :headings :alignment)
(org-capture+-meta--put 'single :description :alignment)
(org-capture+-meta--put 'multi :template :alignment)


(define-org-capture+-filter (ptree :type)
  org-capture+-types)

(define-org-capture+-filter (ptree :target :name)
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

(define-org-capture+-filter (ptree :target :file)
  (let ((name (ptree-get ptree :target :name)))
    (when (memq name
                '(nil file file+headline file+olp file+olp+datetree))
      (org-capture+-get-org-files))))

(define-org-capture+-filter (ptree :target :headlines)
  (let* ((file      (ptree-get ptree :target :file))
         (headlines (ptree-get ptree :target :headlines)))
    (when (and file
               (null headlines))
      (apply #'org-capture+-get-file-headlines file t headlines))))


(defun org-capture+-reset-candidates (ptree)
  (let ((key-lists (ptree-key-lists ptree)))
    (mapcar #'(lambda (keys)
                (cons (format "%s%s%s"
                              (org-capture+-meta-get keys :prompt)
                              (if (eq (org-capture+-meta-get keys :alignment) 'multi) "\n" ": ")
                              (apply #'ptree-get ptree keys))
                      keys))
            (remove-if-not #'(lambda (keys) (apply #'ptree-get ptree keys))
                           key-lists))))

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


    (dolist (keys (ptree-key-lists org-capture+-plist))
      (unless (apply #'ptree-get ptree keys)
        (setq sources
              (nconc sources
                     (apply (org-capture+-meta-get keys :source) ptree keys)))))

    (if sources
        (helm :sources (append sources reset-source))
      (org-capture+-run-ptree ptree)
      (message "ptree %s" ptree))))

;;;###autoload
(defalias 'org-capture+ #'org-capture+-guided)


(defun self-insert-test ()
  (interactive)
  (message "Hello")
  (self-insert-command 1))


(when nil
  (helm :sources (funcall (org-capture+-meta-get '(:type) :source) nil :type))

  (helm (org-capture+-helm-sync nil :type))

  (let (sources)
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:type) :source) nil :type)))
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:target :file) :source) nil :target :file)))
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:target :name) :source) nil :target :name)))
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:target :headings) :source) nil :description)))
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:description) :source) nil :target :file)))
    (setq sources (nconc sources (funcall (org-capture+-meta-get '(:template) :source) nil :target :template)))
    (helm :sources sources))


  (org-capture+-meta-get '(:type) :prompt)

  (org-capture+-meta-get '(:type) :filter))
;;; org-capture+-eng.el ends here
