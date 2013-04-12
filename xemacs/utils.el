;;
;; utils.el
;; Login : <s@taj>
;; Started on  Thu Sep  2 02:33:29 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

(defun global-set-key-if-unbind (key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (message "*emacs-in-init* %s load-lib-with-errors %s (or (not *emacs-in-init*) (not load-lib-with-errors)) %s"
           *emacs-in-init*
           load-lib-with-errors
           (or (not *emacs-in-init*) (not load-lib-with-errors)))
  (if (or (not *emacs-in-init*) (not reloading-libraries))
     (message "k %s c %s" key cmd))
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (when (or (not *emacs-in-init*) (not reloading-libraries))
          (message "key %s already have binded with command %s, can't bind to %s."
                   key bindedcmd cmd))
        (global-set-key key cmd))))

(defun keymap-set-key-if-unbind (map key cmd)
  "Set binding for key if there is no  existing binding for key."
  ;; (interactive)
  (message "*emacs-in-init* %s reloading-libraries %s (or (not *emacs-in-init*) (not reloading-libraries)) %s"
           *emacs-in-init*
           reloading-libraries
           (or (not *emacs-in-init*) (not reloading-libraries)))
  (if (or (not *emacs-in-init*) (not reloading-libraries))
     (message "k %s c %s" key cmd))
  (let ((bindedcmd (key-binding key t)))
    (if bindedcmd
        (if (or (not *emacs-in-init*) (not *binding-config-loaded*))
         (message "key %s already have binded with command %s, can't bind to %s."
                  key bindedcmd cmd))
        (define-key map key cmd))))



(defun fprint (dir)
  "Print the current buffer with same file name."
  (interactive "DDirqectory to put: ")
  (let* ((fname (file-name-nondirectory
                (buffer-file-name)))
         (pname (concat (or dir default-directory) fname ".ps")))
    (ps-print-in-file pname)))



;;{{{ define xrequire

(defun xrequire (feature)
  (unless (member feature exclude-lib)
      (if (not running-xemacs)
          (require feature nil t)
          (require feature nil))))

(defun irequire (feature)
  (ignore-errors
    (unless (member feature exclude-lib)
      (if (not running-xemacs)
          (require feature nil t)
	(require feature nil)))))


;;}}}

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))
