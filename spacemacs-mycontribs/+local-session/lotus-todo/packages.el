;;; packages.el --- lotus-todo layer packages file for Spacemacs.
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
;; added to `lotus-todo-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-todo/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-todo/pre-init-PACKAGE' and/or
;;   `lotus-todo/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-todoS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-todo-packages
  '(
    ;; (PACKAGE :location local)
    (taskjuggler-mode :location local)
    (todoo :location local)
    (sidebrain :location local)
    todo-mode
    (todostack :location local)
    wcheck-mode
    org-jira
    ;; soap-client
    (rfringe :location local)
    (myfixme :location local)
    (ficme-mode :location local)
    (fixme-mode :location local)

    )
  "The list of Lisp packages required by the lotus-todo layer.

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

(defun lotus-todo/init-taskjuggler-mode ()
  (use-package taskjuggler-mode
    :defer t
    :config
    (progn
      ;; http://www.skamphausen.de/cgi-bin/ska/taskjuggler-mode
      ;; http://www.emacswiki.org/emacs/Taskjuggler
      )))

(defun lotus-todo/init-todoo ()
  (use-package todoo
    :defer t
    :config
    (progn
      ;; part of emacs-goodies-el
      ;; http://www.emacswiki.org/emacs/ToDoo
      (setq todoo-file-name (lotus-cache-file "todoo/todo"))
      (defun todoo-or-close-todoo()
        (interactive)
        (if (eq major-mode 'todoo-mode)
            (call-interactively 'todoo-save-and-exit)
          (call-interactively 'todoo))))))

(defun lotus-todo/init-sidebrain ()
  (use-package sidebrain
    :defer t
    :config
    (progn
      ;;http://www.emacswiki.org/emacs/SideBrain
      ;;http://sidebrain.sourceforge.net/manual/index.html
      ;; (trace-function #'sidebrain-read-todo-from-comments)
      ;; it is in dev-config.el
      )))

(defun lotus-todo/init-todo-mode ()
  (use-package todo-mode
    :defer t
    :config
    (progn
      ;; famous
      ;; https://groups.google.com/forum/?fromgroups=#!msg/gnu.emacs.sources/7v7Wlnocr8o/bSUKTMEdL4QJ

      (setq
       todo-file-do   (lotus-cache-file "todo-mode/todo-do")
       todo-file-done (lotus-cache-file "todo-mode/todo-done")))))

(defun lotus-todo/init-todostack ()
  (use-package todostack
    :defer t
    :config
    (progn
      ;; beautiful
      (setq todostack-save-file (lotus-cache-file "todostacksave/todostacksave.el"))
      (add-hook 'kill-emacs-hook        'todostack-save)
      (add-hook 'emacs-startup-hook     'todostack-load)
      (add-hook 'todostack-post-op-hook 'todostack-save))))

(defun lotus-todo/init-wcheck-mode ()
  (use-package wcheck-mode
    :defer t
    :config
    (progn
      ;; Ensure that the variable exists.
      (defvar wcheck-language-data nil)

      (push '("FIXME"
              (program . (lambda (strings)
                           (let (found)
                             (dolist (word my-highlight-words found)
                               (when (member word strings)
                                 (push word found))))))
              (face . highlight)
              (read-or-skip-faces
               (nil)))
            wcheck-language-data)

      ;; Now ‘wcheck-change-language’ to FIXME and turn on ‘wcheck-mode’
      ;; (a minor mode).
      )))

(defun lotus-todo/post-init-org-jira ()
  (use-package org-jira
    :defer t
    :config
    (progn
      )))

(defun lotus-todo/init-rfringe ()
  (use-package rfringe
    :defer t
    :config
    (progn
      )))

(defun lotus-todo/init-myfixme ()
  (use-package myfixme
    :defer t
    :config
    (progn
      (defvar my-highlight-words
        '("FIXME" "TODO" "BUG"))
      )))

(defun lotus-todo/init-ficme-mode ()
  (use-package ficme-mode
    :defer t
    :config
    (progn
      )))

(defun lotus-todo/init-fixme-mode ()
  (use-package fixme-mode
    :defer t
    :config
    (progn
      ;; Just do as hi-lock; add this to the end of fic-mode:

      (font-lock-fontify-buffer)

      ;; And I suggest removing the lighter. It is not very important to
      ;; show it and there is a lack of room in the modeline.

      ;; This highlighting of FIXMEs etc. can be done with
      ;; WcheckMode. Here’s an example configuration:
      ))

  ;; (fixme-mode 1)
  )

;; (defun lotus-todo/init-PACKAGE ()
;;   (use-package PACKAGE
;;     :defer t
;;     :config
;;     (progn
;;       )))

;;; packages.el ends here
