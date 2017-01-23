;;; timeclock-config.el ---

;; Copyright 2009 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: timeclock-setup.el,v 0.0 2009/05/13 00:46:48 doom Exp $
;; Keywords:
;; X-URL: http://obsidianrook.com/codelife/timetrack.html
;; X-URL: http://obsidianrook.com/codelife/code/timeclock-setup.el

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;  Additions to timeclock.el and timeclock-x.el (timeclock-x-squared?).
;;  This does some simple set-up tasks, and includes some elisp glue
;;  to run perl utilities.

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'timeclock-config)

;;; Code:

;; (provide 'timeclock-config)

(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################


;; old-style, for just using "timeclock":

;; (require 'timeclock)

;; (define-key ctl-x-map "ti" 'timeclock-in)
;; (define-key ctl-x-map "to" 'timeclock-out)
;; (define-key ctl-x-map "tc" 'timeclock-change)
;; (define-key ctl-x-map "tr" 'timeclock-reread-log)
;; (define-key ctl-x-map "tu" 'timeclock-update-modeline)
;; (define-key ctl-x-map "tw" 'timeclock-when-to-leave-string)

;; (timeclock-modeline-display)

(require 'timeclock)
(require 'timeclock-x)

;; define this before we use it below...
(defun timeclock-additional-setup ()
  "Some additional set-up (timeclock-x squared?).
Creates a ~/.timeclock/default.log if it doesn't exist already."
  (interactive)
  (let* ((location (substitute-in-file-name "$HOME/.timeclock"))
         (log-file-base "default.log")
         (log-file (concat location "/" log-file-base)))

    (unless (file-exists-p location)
      (make-directory location t))

    (unless (file-exists-p log-file)
      (save-excursion
        (switch-to-buffer log-file t)
        (write-file log-file t) ;; additional safety: ask for confirmation on over-write
        (kill-buffer log-file-base)
        ))))

(defun timeclock-x-initialize ()
  (interactive)
  (timeclock-additional-setup)
  (timeclock-initialize)
  (timeclock-setup-keys)
 ;; (timeclock-modeline-display 1) ;; if you want modline display
 (timeclock-mode-line-display 1))




(defun timeclock-insert-project-hours-report  ()
  "Insert a daily report of hours spent on each project.
Summarizes the data in the timeclock log."
  (interactive)
  (let* ((command "~/bin/timeclock_project_hours_report")
         (report (shell-command-to-string command) ))
    (insert report)))

;; TODO
(unless (and (boundp 'sup-t-map)
             (keymapp sup-t-map))
  ;; (setq sup-t-map (make-sparse-keymap))
  (define-prefix-command 'sup-t-map)
  ;; (local-set-key (kbd "s-t") 'sup-t-map)
  (global-set-key (kbd "s-t") 'sup-t-map))

;; (global-set-key "\C-xtR" 'timeclock-insert-project-hours-report)
;; (global-set-key "\C-ctR" 'timeclock-insert-project-hours-report)
(define-key sup-t-map "tR" 'timeclock-insert-project-hours-report)

(defun timeclock-display-project-names  ()
  "Displays project names in use in the timeclock log."
  (interactive)
  (let* ((command "~/bin/timeclock_project_names")
         (report (shell-command-to-string command)))
    (delete-other-windows)
    (split-window-vertically)
    (other-window 1)
    (switch-to-buffer "*timeclock projects*")
    (mark-whole-buffer)
    (delete-region (mark) (point))
    (insert report)
    (set-mark-command (point)) ; just want to deactivate region

    ;; TODO switch to an appropriate mode
    ;;  (mh-folder-list-mode)
    ))

;; (global-set-key "\C-xtN" 'timeclock-display-project-names)
;; (global-set-key "\C-ctN" 'timeclock-display-project-names)
(define-key sup-t-map "tR" 'timeclock-display-project-names)



(provide 'timeclock-config)
;;; timeclock-config.el ends here
