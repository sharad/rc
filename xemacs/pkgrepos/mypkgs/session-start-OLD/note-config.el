;;; note-config.el --- sdfgsdfg

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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


(deh-require-maybe deft
  (setq
   deft-directory (auto-config-dir "deft/" t)
   deft-use-filename-as-title nil
   deft-extension "org"
   deft-text-mode 'org-mode)
  (add-hook 'lotus-enable-startup-interrupting-feature-hook 'deft t))

(when (xrequire 'evernote-mode)
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

  (add-to-list 'anything-sources anything-c-source-evernote-title))




(deh-require-maybe dbus
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

  (deh-require-maybe 'org
    ;; from: http://lists.gnu.org/archive/html/emacs-orgmode/2009-05/msg00253.html
    (org-add-link-type "tomboy" 'org-tomboy-open)

    (defun org-tomboy-open (note)
      (let ((outbuf (get-buffer-create "*Org Shell Output*"))
            (cmd (concat "tomboy --open-note " (shell-quote-argument note) " &")))
        (with-current-buffer outbuf (erase-buffer))
        (shell-command cmd outbuf outbuf)))))



(provide 'note-config)
;;; note-config.el ends here
