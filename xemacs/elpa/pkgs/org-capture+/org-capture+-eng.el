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
  ())

(defun ptree-set (tree &rest keys)
  ())

(defun org-select-targets (&rest targets)
  (remove-if-not #'(lambda (trg)
                     (memq (cdr trg) targets))
                 org-capture+-target-names))


(defun org-capture+-build-target-arg (plist)
  (let ((name      (plist-get plist :name))
        (file      (plist-get plist :file))
        (headlines (plist-get plist :headlines))
        (function  (plist-get plist :function))
        (marker    (plist-get plist :marker)))
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

(defun org-capture+-build-arg (plist)
  (let ((type     (plist-get plist :type))
        (target   (plist-get plist :target))
        (template (plist-get plist :template)))
    (list type
          (org-capture+-build-target-arg target)
          template)))

(defun org-capture+-run-plist (plist)
  (apply #'org-capture-run
         (org-capture+-build-arg plist)))


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


(defun org-capture+-filter-types (plist)
  org-capture+-types)


(defun org-capture+-target-name-filter (plist)
  (let* ((trg-plist (plist-get plist     :target))
         (file      (plist-get trg-plist :file))
         (headlines (plist-get trg-plist :headlines)))
    (if file
        (apply #'org-select-targets
               (if headlines
                   (if (> (length headlines) 1)
                       '(file+olp file+olp+datetree)
                     '(file+headline file+olp file+olp+datetree))
                 '(file file+headline file+olp file+olp+datetree)))
      org-capture+-target-names)))

(defun org-capture+-target-files-filter (plist)
  (let* ((trg-plist (plist-get plist     :target))
         (name      (plist-get trg-plist :name)))
    (when (memq name
                '(nil file file+headline file+olp file+olp+datetree))
      (org-capture+-get-org-files))))

;; NEW
(defun org-capture+-target-file+headlines-filter (plist)
  (let* ((trg-plist (plist-get plist     :target))
         (file      (plist-get trg-plist :file))
         (headlines (plist-get trg-plist :headlines)))
    (when (and file
               (null headlines))
      (apply #'org-capture+-get-file-headlines file t headlines))))


(defun org-capture+-target-file-source (plist)
  (let ((files (org-capture+-target-files-filter plist)))
    (helm-build-sync-source "Files"
      :candidates files
      :action     #'(lambda (file)
                      (let ((trg-plist (plist-get plist :target)))
                        (setq trg-plist (plist-put trg-plist :file   file))
                        (setq plist     (plist-put plist     :target trg-plist))
                        (org-capture+-guided plist))))))

(defun org-capture+-target-file+headlines-source (plist)
  (let ((headlines       (org-capture+-target-file+headlines-filter plist))
        (headline-action #'(lambda (headlines)
                             (let* ((trg-plist (plist-get plist     :target)))
                               (setq trg-plist (plist-put trg-plist :headlines headlines))
                               (setq plist     (plist-put plist     :target    trg-plist))
                               (org-capture+-guided plist)))))
    (helm-build-sync-source "Headline"
      :candidates headlines
      :action (list (cons "Select" headline-action)))))

(defun org-capture+-target-name-source (plist)
  (let ((targets (org-capture+-target-name-filter plist)))
    (helm-build-sync-source "Target"
      :candidates targets
      :action     #'(lambda (name)
                      (let ((trg-plist (plist-get plist :target)))
                        (setq trg-plist (plist-put trg-plist :name   name))
                        (setq plist     (plist-put plist     :target trg-plist))
                        (org-capture+-guided plist))))))

(defun org-capture+-target-source (&optional plist)
  (let (sources
        (trg-plist (plist-get plist :target)))
    (progn
      (unless (plist-get trg-plist :name)
        (push (org-capture+-target-name-source plist)
              sources))
      (unless (plist-get trg-plist :file)
        (push (org-capture+-target-file-source plist)
              sources))
      (unless (plist-get trg-plist :headlines)
        (push (org-capture+-target-file+headlines-source plist)
              sources))
      sources)))


(defun org-capture+-type-source (plist)
  (let ((types (org-capture+-filter-types plist)))
    (helm-build-sync-source "Type"
      :candidates types
      :action     #'(lambda (type)
                      (setq plist (plist-put plist :type type))
                      (org-capture+-guided plist)))))

(defun org-capture+-description-source (plist)
  (let ((descriptions org-agenda-files))
    (helm-build-dummy-source "Description"
      ;; :candidates descriptions
      :action     #'(lambda (description)
                      (setq plist (plist-put plist :description description))
                      (org-capture+-guided plist)))))

(defun org-capture+-template-source (plist)
  ;; BUG TODO: Add action
  (helm-template-gen-source #'org-capture+-tree-predicate
                            '(t xx yy)
                            0
                            #'(lambda (template)
                                (setq plist (plist-put plist :template template))
                                (org-capture+-guided plist))))


(defun org-capture+-reset-candidates (plist &rest tree-keys)
  (let* ((target (plist-get plist :target))
         (keys   (plist-get-keys plist))
         (keys   (remove-if-not #'(lambda (k) (plist-get plist k))
                                keys)))
    (mapcar #'(lambda (key)
                (cons (format "%s: %s" key (plist-get plist key))
                      (append tree-keys (list key))))
            keys)))

(defun org-capture+reset-source (plist)
  (let ((candidates (org-capture+-reset-candidates plist)))
    (helm-build-sync-source "Reset"
      :candidates candidates
      :action     #'(lambda (key)
                      (setq plist (plist-put plist :key nil))
                      (org-capture+-guided plist)))))


;; TODO: Add resets which will help to edit existing
;;       take new as editing an anonymous

;;;###autoload
(defun org-capture+-guided (&optional plist)
  (interactive)
  (let (sources
        reset-source)
    (setq reset-source
          (list (org-capture+reset-source plist)))
    (progn
      (unless (plist-get plist :type)
        (push (org-capture+-type-source        plist)
              sources))
      (setq sources
            (nconc sources (org-capture+-target-source plist)))
      (unless (plist-get plist :description)
        (push (org-capture+-description-source plist)
              sources))
      (unless (plist-get plist :template)
        (setq sources
              (nconc sources (org-capture+-template-source plist))))

      (if sources
          (helm :sources (append sources reset-source))
        (org-capture+-run-plist plist)
        (message "plist %s" plist)))))

;;;###autoload
(defalias 'org-capture+ #'org-capture+-guided)


(defun self-insert-test ()
  (interactive)
  (message "Hello")
  (self-insert-command 1))

;;; org-capture+-eng.el ends here
