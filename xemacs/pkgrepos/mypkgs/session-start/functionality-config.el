;;; functionality-config.el ---

;; Copyright 2011 Sharad Pratap
;;
;; Author: sh4r4d _at_ _G-mail_
;; Author: sh4r4d _at_ _G-mail_
;; Version: $Id: minor-modes-act.el,v 0.0 2011/04/13 05:33:48 spratap Exp $
;; Keywords:
;; X-URL: not distributed yet

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

(deh-section "sdfsd"
  (defvar *my-desktop-save-max-error-count* 6 "")
  (defvar *my-desktop-save-error-count* 0 "")
  (defcustom with-idle-time-idle-time-interval 7 "save all sessions auto save idle time interval")
  (defvar with-idle-time-idle-time-interval-dynamic 7 "save all sessions auto save idle time interval dynamic.")
  (defcustom with-idle-time-time-interval (* 7 60) "save all sessions auto save time interval")
  (defvar with-idle-time-time (current-time) "save all sessions auto save time")

  (defun with-idle-time-fun (&optional force)
    "Save elscreen frame, desktop, and session time to time
to restore in case of sudden emacs crash."
    (interactive "P")
    (let ((idle-time (current-idle-time))
          (time-format "%a %H:%M:%S"))
      (when (or
             force
             (let ((time-since-last-save (float-time (time-since with-idle-time-time))))
               (and
                (> time-since-last-save (float-time idle-time))
                (> time-since-last-save with-idle-time-time-interval))))
        (if (or
             force
             (and idle-time
                  ;; http://www.gnu.org/software/emacs/manual/html_node/emacs/Auto-Save-Control.html#Auto-Save-Control
                  (> (float-time idle-time) with-idle-time-idle-time-interval-dynamic)))
            (progn
              (message "current time %s, idle time %d idle-time-interval left %d"
                       (format-time-string time-format with-idle-time-time)
                       (float-time idle-time)
                       with-idle-time-idle-time-interval-dynamic)
              (setq with-idle-time-time (current-time)
                    with-idle-time-idle-time-interval-dynamic with-idle-time-idle-time-interval)
              (condition-case e
                                        ; .....
                  ()
                ('error
                 (progn
                   ;; make after 2 errors.
                   (message "with-idle-time: Error: %s" e)
                   (1+ *my-desktop-save-error-count* )
                   (unless(< *my-desktop-save-error-count* *my-desktop-save-max-error-count*)
                     (setq *my-desktop-save-error-count* 0)
                     (message "with-idle-time(): %s" e)

                     (progn
                       ;; fail action
                       )

                     )))))
            (setq with-idle-time-idle-time-interval-dynamic
                  (1- with-idle-time-idle-time-interval-dynamic)))))))






(deh-section "ext-cmd passwd"
  (require 'host-info)
  (defvar program--program (concat "timeout -k 16 10 ssh-add " ssh-key-file) "ssh-add command")

  (defvar program--prompt "Enter passphrase for \\([^:]+\\):"
    "ssh-add prompt for passphrases")

  (defvar program--invalid-prompt "Bad passphrase, try again:"
    "ssh-add prompt indicating an invalid passphrase")

  (defun getpass-ssh-send-passwd (process prompt)
    "read a passphrase with `read-passwd` and pass it to the ssh-add process"
    (let ((passwd (read-passwd prompt)))
      (process-send-string process passwd)
      (process-send-string process "\n")
      (clear-string passwd)))

  (defun program--process-filter (process input)
    "filter for ssh-add input"
    (cond ((string-match ssh-add-prompt input)
           (getpass-ssh-send-passwd process input))
          ((string-match ssh-add-invalid-prompt input)
           (getpass-ssh-send-passwd process input))
          ;; (t (with-current-buffer (get-buffer-create ssh-agent-buffer)
          ;;      (insert input)))
          ))

  (defun program-fun (&optional cmd)
    "run ssh-add"
    (interactive (list (if current-prefix-arg
                           (read-string "Run ssh-add: " program--program)
                           program--program)))
    (unless cmd
      (setq cmd program--program))
    (let ()
      (if cmd
          (set-process-filter
           (apply #'start-process "ssh-add" nil shell-file-name "-c" (list cmd))
           #'program--process-filter)
          (error "No command given")))))


(provide 'functionality-config)
;;; functionality-config.el ends here


