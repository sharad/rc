;;; occ-config.el --- occ config                     -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(defun occ-config-enable-clock-in ()
  (interactive)
  (setq occ-config-clock-in t))

(defun occ-config-disable-clock-in ()
  (interactive)
  (setq occ-config-clock-in nil))

(defun occ-config-clock-in ()
  occ-config-clock-in)


;; Debugger entered--Lisp error: (error "Attempt to delete minibuffer or sole ordinary window")
;; signal(error ("Attempt to delete minibuffer or sole ordinary window"))
;; error("Attempt to delete minibuffer or sole ordinary window")
;; delete-window()
;; funcall-interactively(delete-window)
;; call-interactively(delete-window nil nil)
;; command-execute(delete-window)

;; Running occ-list-select-internal
;; occ-list-select: selected = nil
;; occ-select((obj occ-ctx)): occ-list-select returned Unknown: [cl-struct-occ-return :occ-nocandidate nil]
;; occ-props-window-edit((obj occ-ctx)): selected original: [cl-struct-occ-return :occ-nocandidate nil], retval: Null: nil with label :occ-nocandidate
;; occ-props-window-edit((obj occ-ctx)): returning original: [cl-struct-occ-return :occ-nocandidate nil], retval: Null: nil with label :occ-nocandidate operate: nil
;; finished occ-safe-props-window-edit
;; 2019-08-09 01:58:09 s: end: occ-delayed-select-obj-prop-edit-when-idle
;; occ-clock-in-if-not: operate (:occ-nocandidate :occ-timeout) retval nil
;; occ-clock-marker-is-unnamed-p: begin
;; occ-clock-in(obj occ-ctx): clock-in not allowed. [2 times]
;; occ-clock-in-if-not: Now really clock done.
;; occ-try-clock-schedule-next-timeout: begin
;; occ-try-clock-in-next-timeout: begin
;; Quit [4 times]
;; Updating buffer list...done
;; Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help
;; Updating buffer list...done
;; Commands: m, u, t, RET, g, k, S, D, Q; q to quit; h for help
;; Mark set
;; Entering debugger...
;; occ-clock-in-curr-ctx-if-not-timer-function: begin
;; occ-run-curr-ctx-chg-timer: begin
;; occ-clock-in-if-not((obj occ-ctx)): begin
;; occ-clock-marker-is-unnamed-p: begin
;; occ-clock-in-if-not: Now really going to clock with this-command=nil
;; TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only ’confirm
;; occ-clock-in(occ-ctx=[cl-struct-occ-ctx *Backtrace* nil *Backtrace* nil])
;; occ-clock-in((obj occ-ctx)): begin
;; occ-select((obj occ-ctx)): begin

;;; occ-config.el ends here
