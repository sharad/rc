;;; occ-config.el --- occ config                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-config)


(require 'occ-util-common)
(require 'occ-macros)


;; check
;; https://emacs.stackexchange.com/questions/12111/why-is-defgroup-useful

(defgroup occ nil
  "Org Context clock."
  :prefix "occ-"
  :group 'extensions
  :group 'convenience
  :version "0.1"
  :link '(emacs-commentary-link :tag "Commentary" "occ.el")
  :link '(emacs-library-link :tag "Lisp File" "occ.el")
  :link '(custom-manual "(occ) Top")
  :link '(info-link "(occ) Customization"))


(defcustom occ-unnamed t
  "occ-unnamed")

(defcustom occ-clockout-unassociable-to-unnamed 'ask
  "occ-clockout-unassociable-to-unnamed") ;; TODO: or could ask to continue for TIME(m/h) with current task.


;; mozilla config
;; name type default-value custom-value possible types

;; emacs defcustom
;; https://emacs.stackexchange.com/questions/15064/how-to-properly-use-defcustom
;; https://stackoverflow.com/questions/15309548/how-to-set-defcustom-variable
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Customization-Types.html
;; https://www.gnu.org/software/emacs/manual/html_node/eintr/defcustom.html

(defun occ-confirm (config
                    msg
                    timeout)
  (cond
   ((null config) nil)
   ((function config) (funcall config))
   ((eq config 'ask)  (y-or-n-p msg))
   ((eq config t) t)
   (t nil)))


;; org-agenda-category-icon-alist
;; https://orgmode.org/manual/Categories.html
;; org-todo-keywords
;; ((sequence "TODO(t)" "STARTED" "NEXT(n)" "|" "DONE(d@/!)" "|" "CLOSED(c@/!)")
;;  (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(C@/!)" "PHONE" "MEETING"))


(cl-defstruct occ-entry-types
  :sequence
  :catogery)


(defvar occ-config-clock-in t)

;;;###autoload
(defun occ-config-enable-clock-in ()
  (interactive)
  (setq occ-config-clock-in t))

;;;###autoload
(defun occ-config-disable-clock-in ()
  (interactive)
  (setq occ-config-clock-in nil))

;;;###autoload
(defun occ-config-clock-in ()
  occ-config-clock-in)


;; option for keeping quiet
(occ-gen-binary-option-commands occ-config- quiet -p nil)

;;; occ-config.el ends here
