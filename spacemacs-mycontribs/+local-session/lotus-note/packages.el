;;; packages.el --- lotus-note layer packages file for Spacemacs.
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
;; added to `lotus-note-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-note/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-note/pre-init-PACKAGE' and/or
;;   `lotus-note/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-note-packages
  '(
    deft
    evernote-mode
    dbus
    ein)
  "The list of Lisp packages required by the lotus-note layer.

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

(defun lotus-note/post-init-deft ()
  (use-package deft
      :defer t
      :config
      (progn
        (setq
         deft-directory (auto-config-dir "deft/" t)
         deft-use-filename-as-title nil
         deft-extension "org"
         deft-text-mode 'org-mode)
        (add-to-enable-startup-interrupting-feature-hook 'deft t))))

(defun lotus-note/init-evernote-mode ()
  (use-package evernote-mode
    :defer t
    :config
    (progn
      ;; http://emacs-evernote-mode.googlecode.com/svn-history/r190/trunk/doc/readme_en.html#sec-7
      ;; (setq evernote-username "") ; optional: you can use this username as default.
      (setq evernote-enml-formatter-command '("w3m" "-dump" "-I" "UTF8" "-O" "UTF8")
            evernote-password-cache t) ; option

      ;; (global-set-key "\C-cec" 'evernote-create-note)
      ;; (global-set-key "\C-ceo" 'evernote-open-note)
      ;; (global-set-key "\C-ces" 'evernote-search-notes)
      ;; (global-set-key "\C-ceS" 'evernote-do-saved-search)
      ;; (global-set-key "\C-cew" 'evernote-write-note)
      ;; (global-set-key "\C-cep" 'evernote-post-region)
      ;; (global-set-key "\C-ceb" 'evernote-browser)
      (when (boundp 'anything-sources)
       (add-to-list 'anything-sources anything-c-source-evernote-title)))))

(defun lotus-note/init-dbus ()
  (use-package dbus
    :defer t
    :config
    (progn
      (progn ;; site:
        ;; from: http://emacs-fu.blogspot.in/2009/01/using-d-bus-example.html
        (defun djcb-call-tomboy (method &rest args)
          "call the tomboy method METHOD with ARGS over dbus"
          (apply 'dbus-call-method
                 :session                            ; use the session (not system) bus
                 "org.gnome.Tomboy"                  ; service name
                 "/org/gnome/Tomboy/RemoteControl"   ; path name
                 "org.gnome.Tomboy.RemoteControl"    ; interface name
                 method args))

        ;; Then, djcb-tomboy-create-note-region creates a new note from the
        ;; region (selection) use CreateNamedNote and SetNoteContents; it even
        ;; does some rudimentary error checking:

        (defun djcb-tomboy-create-note-region (b e name)
          "Create a new note with in the Tomboy notetaker from region"
          (interactive "r\nsName for new Tomboy note:")
          (let ((note-uri (djcb-call-tomboy "CreateNamedNote" name)))
            (if (and note-uri (> (length note-uri) 0))
                (djcb-call-tomboy "SetNoteContents" note-uri
                                  (concat name "\n" (buffer-substring b e)))
              (message "hmmm... it did not work. maybe try a different name"))))

        ;; With djcb-tomboy-insert-note-contents we can insert the contents of
        ;; some tomboy note into the current buffer, using
        ;; FindNote/GetNoteContents There's auto-completion available for the
        ;; name of the note, using ListAllNotes:

        (defun djcb-tomboy-insert-note-contents (name)
          "Insert Tomboy note with NAME"
          (interactive
           (list (let ((lst))
                   (dolist (uri (djcb-call-tomboy "ListAllNotes"))
                     (add-to-list 'lst (djcb-call-tomboy "GetNoteTitle" uri)))
                   (completing-read "Name of Tomboy Note:" lst))))
          (let ((note-uri (djcb-call-tomboy "FindNote" name)))
            (when note-uri
              (insert (djcb-call-tomboy "GetNoteContents" note-uri)))))

        (use-package org
          :defer t
          :config
          (progn
            (use-package "org"
              :defer t
              :config
              ;; from: http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00253.html
              (org-add-link-type "tomboy" 'org-tomboy-open)

              (defun org-tomboy-open (note)
                (let ((outbuf (get-buffer-create "*Org Shell Output*"))
                      (cmd (concat "tomboy --open-note " (shell-quote-argument note) " &")))
                  (with-current-buffer outbuf (erase-buffer))
                  (shell-command cmd outbuf outbuf))))))))))

(defun lotus-note/post-init-ein ()
  (use-package ein-notification
    :defer t
    :commands (ein:header-line-setup-maybe)
    :config
    (progn
      ))

  (use-package ein-ac
    :defer t
    :commands (ein:ac-setup ein:ac-setup-maybe)
    :config
    (progn))

  (use-package ein-notebook
    :defer t
    :commands (ein:notebook-mode)
    :config
    (progn)))

;;; packages.el ends here
