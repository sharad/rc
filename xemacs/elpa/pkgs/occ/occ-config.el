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


;; Wrote /home/s/hell/.xemacs/elpa/pkgs/lotus-utils/lotus-misc-utils.{el,elc}
;; delete-window: Attempt to delete last non-side window
;; Quit [12 times]
;; delete-window: Attempt to delete last non-side window
;; Busy user input 48

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

;; Search for
;; No recursive edit in progress
;; Symbol's value as variable is nil ido-cur-list
;; Window nil is a minibuffer window


;; occ-insinuate: begin
;; Restoring clock data
;; Loading /home/s/hell/.emacs.d/.cache/autoconfig/org/clock/org-clock-save.el (source)...done
;; Resume clock (Unnamed task 1036) n
;; occ-insinuate: finish
;; Busy user input return
;; occ-clock-in: Edit properties of a tsk to make associable to current context.
;; Busy user input return
;; 2019-08-09 02:09:21 s: org-clock-in-if-not: not running as minibuffer is already active.
;; occ-clock-in(obj occ-ctx): clock-in not allowed.
;; lotus-with-marker: selecting buf Unnamed.org [8 times]
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; Busy user input (dbus-event :session 2 47 org.freedesktop.DBus nil nil nil dbus-call-method-handler :1.3)
;; occ-clock-in: Edit properties of a tsk to make associable to current context.
;; Busy user input (dbus-event :session 2 47 org.freedesktop.DBus nil nil nil dbus-call-method-handler :1.3)
;; helm-timed: triggered timer for new-win #<window 20 on *helm*>
;; occ-clock-in(obj occ-ctx): clock-in not allowed.
;; Busy user input (dbus-event :session 2 47 org.freedesktop.DBus nil nil nil dbus-call-method-handler :1.3)
;; occ-clock-in: Edit properties of a tsk to make associable to current context.
;; Busy user input (dbus-event :session 2 47 org.freedesktop.DBus nil nil nil dbus-call-method-handler :1.3)
;; Quit
;; delete-window: Attempt to delete minibuffer or sole ordinary window
;; Quit
;; occ-clock-in(obj occ-ctx): clock-in not allowed.
;; Quit [4 times]
;; occ-clock-in-if-chg: ctx [cl-struct-occ-ctx occ-rank.el nil occ-rank.el /home/s/hell/.xemacs/elpa/pkgs/occ/occ-rank.el] not suitable to associate as context is not changed.
;; Mark set
;; Saving file /home/s/hell/.xemacs/elpa/pkgs/occ/occ-rank.el...
;; Wrote /home/s/hell/.xemacs/elpa/pkgs/occ/.occ-rank.el.~undo-tree~
;; Wrote /home/s/hell/.xemacs/elpa/pkgs/occ/occ-rank.el
;; Error callling git diff:

;; scroll-down-command: Beginning of buffer [4 times]
;; scroll-up-command: End of buffer [4 times]
;; Mark activated
;; Quit [2 times]
;; Saving file /home/s/hell/.xemacs/elpa/pkgs/occ/occ-rank.el...
;; Wrote /home/s/hell/.xemacs/elpa/pkgs/occ/.occ-rank.el.~undo-tree~
;; Wrote /home/s/hell/.xemacs/elpa/pkgs/occ/occ-rank.el
;; Error callling git diff:

;; Mark set [2 times]
;; Mark saved where search started
;; Busy user input (dbus-event :session 2 58 :1.3 nil nil nil dbus-call-method-handler 28)
;; occ-clock-in: Edit properties of a tsk to make associable to current context.
;; Busy user input (dbus-event :session 2 58 :1.3 nil nil nil dbus-call-method-handler 28)
;; Aborting an helm session running in background
;; Quit
;; delete-window: Attempt to delete minibuffer or sole ordinary window [2 times]
;; occ-clock-in(obj occ-ctx): clock-in not allowed.
;; Quit [4 times]
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list [2 times]
;; minibuffer-keyboard-quit: No recursive edit is in progress [2 times]
;; switch-to-prev-buffer: Window nil is a minibuffer window [3 times]
;; exit-minibuffer: No catch for tag: exit, nil [2 times]
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list [2 times]
;; minibuffer-keyboard-quit: No recursive edit is in progress [3 times]
;; exit-minibuffer: No catch for tag: exit, nil
;; minibuffer-keyboard-quit: No recursive edit is in progress
;; occ-clock-in-if-chg: ctx [cl-struct-occ-ctx  *Minibuf-2* nil  *Minibuf-2* nil] not suitable to associate as context buffer is minibuffer.
;; minibuffer-keyboard-quit: No recursive edit is in progress [6 times]
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list
;; minibuffer-keyboard-quit: No recursive edit is in progress
;; Error callling git diff:

;; Warning: symbolic link to Git-controlled source file
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list [2 times]
;; Error callling git diff:

;; Warning: symbolic link to Git-controlled source file
;; funcall-interactively: Symbol’s value as variable is void: ido-require-match
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list [2 times]
;; minibuffer-keyboard-quit: No recursive edit is in progress [4 times]
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list
;; switch-to-prev-buffer: Window nil is a minibuffer window
;; occ-clock-in-if-chg: ctx [cl-struct-occ-ctx  *Minibuf-2* nil  *Minibuf-2* nil] not suitable to associate as context buffer is minibuffer.
;; switch-to-prev-buffer: Window nil is a minibuffer window [3 times]
;; Error callling git diff:

;; Warning: symbolic link to Git-controlled source file
;; funcall-interactively: Symbol’s value as variable is void: ido-cur-list [7 times]
;; Mark set [2 times]
;; Saved text from "occ-insinuate: begin
;; Restoring clock dat"
;; Mark saved where search started
;; Mark set


;;; occ-config.el ends here
