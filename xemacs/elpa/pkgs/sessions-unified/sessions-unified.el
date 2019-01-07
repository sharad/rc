;;; sessions-unified.el --- session setting

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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




;; set session-mgr-utils-notify
;; run frame-session-restore-hook-func
;; run lotus-desktop-session-restore
;; in some startup-hook


;; (defun lotus-desktop-session-config (funn-notify)
;;   (progn
;;     (setq session-mgr-utils-notify funn-notify)
;;     (add-hook
;;      'lotus-enable-startup-interrupting-feature-hook
;;      'frame-session-restore-hook-func
;;      t)
;;     (add-hook ;; 'after-init-hook
;;      'lotus-enable-startup-interrupting-feature-hook
;;      '(lambda ()
;;         (run-at-time-or-now 7 'lotus-desktop-session-restore)))))

;; function frame-session-restore-hook-func
;; (add-hook 'lotus-enable-startup-interrupting-feature-hook
;;           'frame-session-restore-hook-func
;;           t)

;; (add-hook ;; 'after-init-hook
;;  'lotus-enable-startup-interrupting-feature-hook
;;  '(lambda ()
;;    (run-at-time-or-now 7 'lotus-desktop-session-restore)))

;;; Code:

(provide 'sessions-unified)


;; (require 'dot-emacs-helper)

(require 'general-testing)
;; testing
(require 'rcs-backup)

(require 'cl)

(eval-when-compile
  '(require 'cl))

(require 'wrappers)
(require 'basic-utils)
;; run-at-time-or-now
(require 'utils-custom)
;; lotus-read-sexp
(require 'misc-utils)

(require 'desktop)
(require 'session)
(require 'elscreen)
(require 'emacs-panel)

;; ;; BUG TODO
;; (require 'vc-config)

(defvar *session-unified-desktop-enabled* t "Enable desktop restoration.")
(defvar *session-unified-session-enabled* t "Enable session restoration.")


(defvar lotus-disable-desktop-restore-interrupting-feature-hook nil
  "feature that need to be disabled for proper restoring of desktop.")

(defvar lotus-enable-desktop-restore-interrupting-feature-hook nil
  "feature that were disabled for proper restoring of desktop will get re-enabled here.")

(defvar session-unified-save-all-sessions-before-hook nil "Hook run before saving all session")
(defvar session-unified-save-all-sessions-after-hook nil "Hook run after saving all session")

(eval-when-compile
 (defvar sessions-unified-utils-notify nil)
 (unless (null 'sessions-unified-utils-notify)
   (setq sessions-unified-utils-notify
         (lambda (title fmt &rest args)
           (concat title ": "
                   (apply 'message fmt args))))))

(defvar sessions-unified-utils-notify nil)

(unless (null 'sessions-unified-utils-notify)
  (setq sessions-unified-utils-notify
        (lambda (title fmt &rest args)
          (concat title ": "
                  (apply 'message fmt args)))))

;;;###autoload
(defun add-to-enable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-enable-desktop-restore-interrupting-feature-hook
   fn
   append
   local))
;;;###autoload
(defun remove-from-enable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-enable-desktop-restore-interrupting-feature-hook
   fn
   local))

;;;###autoload
(defun add-to-disable-desktop-restore-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-disable-desktop-restore-interrupting-feature-hook
   fn
   append
   local))
;;;###autoload
(defun remove-from-disable-desktop-restore-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-disable-desktop-restore-interrupting-feature-hook
   fn
   local))



(require 'fmsession)
(require 'session-unified)
(require 'desktop-unified)
(require 'misc-unified)




;; ;;;###autoload
;; (defun lotus-desktop-session-config (funn-notify)
;;   (progn
;;     (setq session-mgr-utils-notify funn-notify)
;;     (add-hook
;;      'lotus-enable-startup-interrupting-feature-hook
;;      'frame-session-restore-hook-func
;;      t)
;;     (add-hook ;; 'after-init-hook
;;      'lotus-enable-startup-interrupting-feature-hook
;;      '(lambda ()
;;         (run-at-time-or-now 7 'lotus-desktop-session-restore)))))

;; (toggle-debug-on-error)

;;; session-config.el ends here
