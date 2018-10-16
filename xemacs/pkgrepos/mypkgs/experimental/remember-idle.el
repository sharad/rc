;;; remember-idle.el --- remember-idle               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(require 'tree)
(require 'loadhist)

(require 'remember)
(require 'org)
(if (featurep 'org-remember)
    (require 'org-remember))
(require 'planner)
(require 'remember-planner)

;; (require 'read-file-name)
;; (require 'remember-blosxom)
;; (require 'remember-experimental) ;; will start mail at daemon startup time.


(require 'remember-autoloads)
;; (require 'remember-diary) ; merged in remember.el
(require 'remember-planner)
(require 'remember-bbdb)



;; (require 'remember-bibl) ; - check it
;; (require 'macs-wiki-journal)



(defvar idle-reminder-register nil "Idle reminder register")
(defvar idle-reminder-buffer nil "buffer")
(defvar idle-reminder-interval (* 7 60) "Idle reminder register")
(defvar idle-reminder-timer nil "buffer")


(defvar idle-reminder-mode-map (make-sparse-keymap)
  "Keymap for org-remember-mode, a minor mode.
Use this map to set additional keybindings for when Org-mode is used
for a Remember buffer.")

(defvar idle-reminder-mode-hook nil
  "Hook for the minor `idle-reminder-mode'.")

(define-minor-mode idle-reminder-mode
    "Minor mode for special key bindings in a remember buffer."
  nil " Rem" idle-reminder-mode-map
  (run-hooks 'org-remember-mode-hook))


(defun leave-show-reminder ()
  (interactive)
  (when (equal idle-reminder-buffer
               (current-buffer))
    (with-current-buffer idle-reminder-buffer
      (reader-mode nil)
      (bury-buffer)))
  (if idle-reminder-register
      (jump-to-register idle-reminder-register)))

(define-key idle-reminder-mode-map "q" 'leave-show-reminder)
;; (define-key idle-reminder-mode-map "\C-c\C-k" 'org-remember-kill)

(defun show-reminder (fn &optional time-to-show)
  (window-configuration-to-register idle-reminder-register)
  (setq idle-reminder-buffer (funcall fn))
  ;; (view-mode 1)
  (when idle-reminder-buffer
    (with-current-buffer idle-reminder-buffer
      (reader-mode 1)
      (idle-reminder-mode 1))
    (switch-to-buffer idle-reminder-buffer)))


(defun show-some-orgfile ()
  (let* ((file "~/.Organize/emacs/org/myself/emacs.org")
         (buf (or (find-buffer-visiting file)
                  (find-file-noselect file))))
    (switch-to-buffer buf)
    buf))

(defun idle-reminder-start ()
  (interactive)
  (setq idle-reminder-timer
        (run-with-idle-timer idle-reminder-interval t 'show-reminder 'show-some-orgfile)))


(defun idle-reminder-cancel ()
  (interactive)
  (when idle-reminder-timer
    (cancel-timer idle-reminder-timer)
    (when idle-reminder-buffer
      (with-current-buffer idle-reminder-buffer
        (reader-mode nil)
        (bury-buffer)))))



(provide 'remember-idle)
;;; remember-idle.el ends here
