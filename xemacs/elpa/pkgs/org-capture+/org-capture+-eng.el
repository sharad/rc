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
    (file "path/to/file")
    (id "id of existing Org entry")
    (file+headline "path/to/file" "node headline")
    (file+olp "path/to/file" "Level 1 heading" "Level 2" ...)
    (file+olp+datetree "path/to/file" "Level 1 heading" ...)
    (file+function "path/to/file" function-finding-location)
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
     (file (let* ((file (org-capture+-select-target-file))
                  (heading (org-capture+-select-target-heading file)))
             (list 'id  file heading)))
     (file+olp (list 'file+olp "path/to/file" "Level 1 heading" "Level 2"))
     (file+olp+datetree (list 'file+olp+datetree "path/to/file" "Level 1 heading"))
     (file+function (list 'file+function "path/to/file" 'function-finding-location))
     (clock 'clock)
     (function (list 'function 'function-finding-location))
     (marker marker))))


(defun org-capture+-select-target-file ())

(defun org-capture+-select-target-id ())

(defun org-capture+-select-target-heading (file))

;;; org-capture+-eng.el ends here
