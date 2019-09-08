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


;; TODO: some kind of recommendation system, not rigid, but not fully free also.
;; THINK
;; you have tree api with node functions

;; org-capture-templates

;; (org-capture)

;; Value:
'(("w" "Default template" entry
   (file+headline "/home/s/paradise/capture/capture.org" "Notes")
   "* %^{Title}\n\n  Source: %u, %c\n\n  %i" :empty-lines 1)
  ("c" "Current Clock")
  ("ch" "Current Clock Heading" entry
   (clock))
  ("ci" "Current Clock Item" item
   (clock))
  ("cp" "Current Clock Plain" plain
   (clock))
  ("k" "Current Task" entry
   (file+headline
    (lambda nil
      (org-template-set-file-writable
       (expand-file-name "notes.org"
                         (find-task-dir)))))
   "* TODO %? %^g\n %i\n [%a]\n" :empty-lines 1)
  ("m" "Emacs" entry "/home/s/paradise/capture/emacs.org" "* TODO %? %^g\n %i\n [%a]\n" :empty-lines 1)
  ("t" "Todo" entry
   (file+headline "/home/s/paradise/capture/todo.org" "G T D")
   "* TODO %? %^g\n %i\n [%a]\n" :empty-lines 1)
  ("j" "Journal" entry
   (file+headline "/home/s/paradise/capture/journal.org" "j o u r n a l")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("n" "Plan" entry
   (file+headline "/home/s/paradise/capture/plan.org" "p l a n")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("l" "Learn" entry
   (file+headline "/home/s/paradise/capture/learn.org" "Learn")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("i" "Idea" entry
   (file+headline "/home/s/paradise/capture/idea.org" "Ideas")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("b" "Book" entry
   (file+headline "/home/s/paradise/capture/journal.org" "Books")
   "\n* %^{Book Title} %t :READING: \n%[~/Documents/CreatedContent/contents/virtual/org/default/remember/templates/book]\n [%a]\n" :empty-lines 1)
  ("p" "Private" entry
   (file+headline "/home/s/paradise/capture/privnotes.org")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("r" "Remember" entry
   (file+headline "/home/s/paradise/capture/remember.org" "Remember")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("s" "SomeDay" entry
   (file+headline "/home/s/paradise/capture/someday.org" "Some Day")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("w" "Waiting-For" entry
   (file+headline "/home/s/paradise/capture/waiting4.org" "Waiting")
   "\n* %^{topic} %T \n%i%?\n [%a]\n" :empty-lines 1)
  ("ac" "Contact" entry
   (file+headline "/home/s/paradise/capture/contacts.org" "Contacts")
   "\n* %^{Name} :CONTACT:\n%[~/Documents/CreatedContent/contents/virtual/org/default/remember/templates/contact]\n %i\n [%a]\n" :empty-lines 1)
  ("e" "Receipt" entry
   (file+headline "/home/s/paradise/capture/finances.org" "Receipts")
   "** %^{BriefDesc} %U %^g\n%?\n [%a]\n" :empty-lines 1)
  ("x" "Refile" entry #'org-goto-refile "* TODO %? %^g\n %i\n [%a]\n" :empty-lines 1))


(defun org-capture+-select-type ()
  (let ((types '(entry item chckitem table-line plain log-note)))
    (intern (completing-read "Type: " types))))

(defun org-capture+-select-target ()
  '(
    (file              "path/to/file")
    (id                "id of existing Org entry")
    (file+headline     "path/to/file" "node headline")
    (file+olp          "path/to/file" "Level 1 heading" "Level 2" ...)
    (file+olp+datetree "path/to/file" "Level 1 heading" ...)
    (file+function     "path/to/file" function-finding-location)
    (clock)
    (function function-finding-location)
    (marker marker)))

(defun org-capture+-select-target-name ()
  (let ((types
         '(file id file+headline file+olp file+olp+datetree clock function)))
    (intern (completing-read "Target Name: " types))))

(defun org-capture+-select-target (target-name)
  (let ((target-name target-name))
   (case target-name
     (file (list 'file (org-capture+-select-target-file)))
     (id   (list 'id   (org-capture+-select-target-id)))
     (file+heading (let* ((file (org-capture+-select-target-file))
                  (heading (org-capture+-select-target-heading file)))
             (list 'id  file heading)))
     (file+olp (list 'file+olp "path/to/file" "Level 1 heading" "Level 2"))
     (file+olp+datetree (list 'file+olp+datetree "path/to/file" "Level 1 heading"))
     (file+function (list 'file+function "path/to/file" 'function-finding-location))
     (clock 'clock)
     (function (list 'function 'function-finding-location))
     (marker marker))))


(defun org-capture+get-template (action)
  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t xx yy)
                               0
                               action)))


(defvar org-capture+-types   '(("Org Entry" . entry)
                               ("List Item" . item)
                               ("Checklist Item" . chckitem)
                               ("Table Line" . table-line)
                               ("Plain Text" . plain)
                               ("Log note" . log-note)))
(defvar org-capture+-targets '(("File" . file)
                               ("Org entry Id" . id)
                               ("File Headline" . file+headline)
                               ("File Outline path" . file+olp)
                               ("File Outline path Date-tree" . file+olp+datetree)
                               ("File function" . file+function)
                               ("Current Org Clock" . clock)
                               ("Function" . function)
                               ("Marker" . marker)))


(defun org-capture+-filter-types (plist)
  org-capture+-types)

(defun org-capture+-filter-target (plist)
  (let ((file (plist-get plist :file)))
    (if file
        (remove-if-not #'(lambda (trg)
                           (memq (cdr trg) '(file file+headline file+olp file+olp+datetree file+function)))
                       org-capture+-targets)
      org-capture+-targets)))

(defun org-capture+-filter-files (plist)
  (let ((target (plist-get plist :target)))
    (when (memq target
                '(nil file file+headline file+olp file+olp+datetree file+function))
      org-agenda-files)))


(defun org-capture+-type-source (plist)
  (let ((types (org-capture+-filter-types plist)))
    (helm-build-sync-source "Type"
      :candidates types
      :action     #'(lambda (type)
                      (setq plist (plist-put plist :type type))
                      (org-capture+-capture plist)))))

(defun org-capture+-target-source (plist)
  (let ((targets (org-capture+-filter-target plist)))
    (helm-build-sync-source "Target"
      :candidates targets
      :action     #'(lambda (target)
                      (setq plist (plist-put plist :target target))
                      (org-capture+-capture plist)))))

(defun org-capture+-file-source (plist)
  (let ((files (org-capture+-filter-files plist)))
    (helm-build-sync-source "File"
      :candidates files
      :action     #'(lambda (file)
                      ;; ()
                      (setq plist (plist-put plist :file file))
                      (org-capture+-capture plist)))))

(defun org-capture+-description-source (plist)
  (let ((descriptions org-agenda-files))
    (helm-build-dummy-source "Description"
      ;; :candidates descriptions
      :action     #'(lambda (description)
                      (setq plist (plist-put plist :description description))
                      (org-capture+-capture plist)))))

(defun org-capture+-template-source (plist)
  ;; BUG TODO: Add action
  (helm-template-gen-source #'org-capture+-tree-predicate
                            '(t xx yy)
                            0
                            #'(lambda (template)
                                (setq plist (plist-put plist :template template))
                                (org-capture+-capture plist))))

(defun org-capture+-capture (&optional plist)
  (interactive)
  (let (sources)
    (progn

      (unless (plist-get plist :type)
        (push (org-capture+-type-source        plist)
              sources))
      (unless (plist-get plist :target)
        (push (org-capture+-target-source      plist)
              sources))
      (unless (plist-get plist :file)
        (push (org-capture+-file-source        plist)
              sources))
      (unless (plist-get plist :description)
        (push (org-capture+-description-source plist)
              sources))
      (unless (plist-get plist :template)
        (setq sources
              (nconc sources (org-capture+-template-source plist))))

      (if sources
          (helm
           :sources sources)
        (message "plist %s" plist)))))

(defun org-capture+-run-template-list (plist)
  ())
;; (org-capture+ TYPE TARGET TEMPLATE &rest PLIST)

(defun org-capture+-build-arg (plist)
  (let ((type (plist-get plist :type))
        (target (list (plist-get plist :target))))
    (cond
     ((memq (car target) '(file file+headline file+olp file+olp+datetree file+function)
            (setq target (nconc target (plist-get plist :file)))
            (case (car target)
              (file+headline )))))))

;;; org-capture+-eng.el ends here
