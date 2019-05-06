;;; lotus-note.el --- Note taking unified            -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d _at_ Gmail>
;; Keywords: convenience, docs, tools

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




(defun* add-org-capture-templates (&rest project-spec)
            "Add org project."
            (add-to-list 'org-capture-templates project-spec t))

          ;; (setq org-protocol-default-template-key "l")

(defun org-capture-template-gen (&optional org-parent-dir)
  (setq org-capture-templates nil)
  (let ((org-parent-dir (or org-parent-dir
                            (expand-file-name "capture" (org-publish-get-attribute "notes" "org" :base-directory)))))
    (progn

      (add-org-capture-templates
       "w" "Default template"
       'entry
       `(file+headline ,(expand-file-name "capture.org" org-parent-dir) "Notes")
       "* %^{Title}\n\n  Source: %u, %c\n\n  %i"
       :empty-lines 1)

      ;; (add-org-capture-templates
      ;;  ?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" nil "Notes")

      (add-org-capture-templates
       "c" "Current Clock")

      (add-org-capture-templates
       "ch" "Current Clock Heading"
       'entry
       '(clock))

      (add-org-capture-templates
       "ci" "Current Clock Item"
       'item
       '(clock))

      (add-org-capture-templates
       "cp" "Current Clock Plain"
       'plain
       '(clock))

      (add-org-capture-templates
       "k" "Current Task"
       'entry
       `(file+headline
         (lambda ()
           (org-template-set-file-writable (expand-file-name "notes.org" (find-task-dir)))))
       "* TODO %? %^g\n %i\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "m" "Emacs"
       'entry
       (expand-file-name "emacs.org" org-parent-dir)
       "* TODO %? %^g\n %i\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "t" "Todo"
       'entry
       `(file+headline ,(expand-file-name "todo.org" org-parent-dir) "G T D")
       "* TODO %? %^g\n %i\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "j" "Journal" ;; any kind of note
       'entry
       `(file+headline ,(expand-file-name "journal.org" org-parent-dir) "j o u r n a l")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "n" "Plan" ;; any kind of note
       'entry
       `(file+headline ,(expand-file-name "plan.org" org-parent-dir) "p l a n")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "l" "Learn" ;; any kind of note
       'entry
       `(file+headline ,(expand-file-name "learn.org" org-parent-dir) "Learn")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "i" "Idea" ;; any kind of note
       'entry
       `(file+headline ,(expand-file-name "idea.org" org-parent-dir) "Ideas")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "b" "Book" ;; book descp
       'entry
       `(file+headline ,(expand-file-name "journal.org" org-parent-dir) "Books")
       "\n* %^{Book Title} %t :READING: \n%[~/Documents/CreatedContent/contents/virtual/org/default/remember/templates/book]\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "p" "Private" ;; private note
       'entry
       `(file+headline ,(expand-file-name "privnotes.org" org-parent-dir))
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "r" "Remember" ;; private note
       'entry
       `(file+headline ,(expand-file-name "remember.org" org-parent-dir) "Remember")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "s" "SomeDay" ;; private note
       'entry
       `(file+headline ,(expand-file-name "someday.org" org-parent-dir) "Some Day")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "w" "Waiting-For" ;; private note
       'entry
       `(file+headline ,(expand-file-name "waiting4.org" org-parent-dir) "Waiting")
       "\n* %^{topic} %T \n%i%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "ac" "Contact" ;; contact
       'entry
       `(file+headline ,(expand-file-name "contacts.org" org-parent-dir) "Contacts")
       "\n* %^{Name} :CONTACT:\n%[~/Documents/CreatedContent/contents/virtual/org/default/remember/templates/contact]\n %i\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "e" "Receipt" ;; receipt
       'entry
       `(file+headline ,(expand-file-name "finances.org" org-parent-dir) "Receipts")
       "** %^{BriefDesc} %U %^g\n%?\n [%a]\n"
       :empty-lines 1)

      (add-org-capture-templates
       "x" "Refile"
       'entry
       '(function org-goto-refile)
       "* TODO %? %^g\n %i\n [%a]\n"
       :empty-lines 1)

      )))

(org-capture-template-gen)


(provide 'lotus-note)
;;; lotus-note.el ends here
