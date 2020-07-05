;;; packages.el --- lotus-remember layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-remember-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-remember/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-remember/pre-init-PACKAGE' and/or
;;   `lotus-remember/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-rememberS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-remember-packages
  '(remember
    org
    (org-remember :location local)
    (org-capture :location local)
    org-capture+
    planner
    (remember-planner :location local)
    (read-file-name :location local)
    (remember-blosxom :location local)
    (remember-experimental :location local) ;; will start mail at daemon startup time.
    (remember-autoloads :location local)
    (remember-diary :location local) ; merged in remember.el
    (remember-bbdb :location local)
    loadhist
    (remember-bibl :location local)
     (macs-wiki-journal :location local)
    (remember-unified :location local)
    (remember-idle :location local))

  "The list of Lisp packages required by the lotus-remember layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;; start

;; Setting up for OrgMode
;;Basically you should follow the instructions in the org-mode info
;;file. However this currently forgets that you also need something like
;;this in your .emacs:

(defun lotus-remember/init-remember ()
  (use-package remember
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/post-init-org ()
  (use-package org
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-org-remember ()
  (use-package org-remember
      :defer t
      :config
      (progn
        (progn ;; "org-remember"
          (defun* add-org-remember-templates (&rest project-spec)
            "Add org project."
            (add-to-list 'org-remember-templates project-spec t))

          (setq org-remember-templates nil)

          (defun org-remember-template-gen (&optional org-parent-dir)
            (let ((org-parent-dir (or org-parent-dir
                                      (expand-file-name "remember" (org-publish-get-attribute "notes" "org" :base-directory)))))
              (progn
                (add-org-remember-templates
                 ?w "* %^{Title}\n\n  Source: %u, %c\n\n  %i" nil "Notes")

                (add-org-remember-templates
                 "Current Task" ?k "* TODO %? %^g\n %i\n [%a]\n"
                 (lambda ()
                   (org-template-set-file-writable (expand-file-name "notes.org" (find-task-dir)))))

                (add-org-remember-templates
                 "Emacs" ?m "* TODO %? %^g\n %i\n [%a]\n" (expand-file-name "emacs.org" org-parent-dir))

                (add-org-remember-templates "Todo" ?t "* TODO %? %^g\n %i\n [%a]\n" (expand-file-name "todo.org" org-parent-dir) "G T D")

                (add-org-remember-templates
                 "Journal" ;; any kind of note
                 ?j "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "journal.org" org-parent-dir) "j o u r n a l")

                (add-org-remember-templates "Plan" ;; any kind of note
                                            ?n "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "plan.org" org-parent-dir) "p l a n")

                (add-org-remember-templates "Learn" ;; any kind of note
                                            ?l "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "learn.org" org-parent-dir) "Learn")

                (add-org-remember-templates "Idea" ;; any kind of note
                                            ?i "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "idea.org" org-parent-dir) "Ideas")

                (add-org-remember-templates "Book" ;; book descp
                                            ?b "\n* %^{Book Title} %t :READING: \n%[~/Documents/online/CreatedContent/contents/virtual/org/default/remember/templates/book]\n [%a]\n" (expand-file-name "journal.org" org-parent-dir) "Books")

                (add-org-remember-templates "Private" ;; private note
                                            ?p "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "privnotes.org" org-parent-dir))

                (add-org-remember-templates "Remember" ;; private note
                                            ?r "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "remember.org" org-parent-dir))

                (add-org-remember-templates "SomeDay" ;; private note
                                            ?s "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "someday.org" org-parent-dir))

                (add-org-remember-templates "Waiting-For" ;; private note
                                            ?w "\n* %^{topic} %T \n%i%?\n [%a]\n" (expand-file-name "waiting4.org" org-parent-dir))

                (add-org-remember-templates "Contact" ;; contact
                                            ?c "\n* %^{Name} :CONTACT:\n%[~/Documents/online/CreatedContent/contents/virtual/org/default/remember/templates/contact]\n %i\n [%a]\n" (expand-file-name "contacts.org" org-parent-dir))

                (add-org-remember-templates "Receipt" ;; receipt
                                            ?e "** %^{BriefDesc} %U %^g\n%?\n [%a]\n" (expand-file-name "finances.org" org-parent-dir)))))

          ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html
          ;; (defvar org-remember-templates nil "templates for org.")

          (org-remember-template-gen)
          ))))

(defun lotus-remember/init-org-capture ()
  (use-package org-capture
      :defer t
      :config
      (progn
        (progn ;; "org-capture"

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
                 "\n* %^{Book Title} %t :READING: \n%[~/Documents/online/CreatedContent/contents/virtual/org/default/remember/templates/book]\n [%a]\n"
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
                 "\n* %^{Name} :CONTACT:\n%[~/Documents/online/CreatedContent/contents/virtual/org/default/remember/templates/contact]\n %i\n [%a]\n"
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

          (org-capture-template-gen)))))

(defun lotus-remember/init-org-capture+ ()
  (use-package org-capture+
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/post-init-planner ()
  (use-package planner
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-blosxom ()
  (use-package remember-blosxom
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-experimental ()
  (use-package remember-experimental
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-autoloads ()
  (use-package remember-autoloads
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-diary ()
  (use-package remember-diary
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-planner ()
  (use-package remember-planner
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-read-file-name ()
  (use-package read-file-name
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-bbdb ()
  (use-package remember-bbdb
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-loadhist ()
  (use-package loadhist
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-bibl ()
  (use-package remember-bibl
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-macs-wiki-journal ()
  (use-package macs-wiki-journal
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-unified ()
  (use-package remember-unified
      :defer t
      :config
      (progn
        )))

(defun lotus-remember/init-remember-idle ()
  (use-package remember-idle
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
