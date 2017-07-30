;;; startup-hooks.el --- startup hooks               -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

;;{{ Run after init

(defvar *sharad/after-init-hook* nil "sharad/after-init-hook")
(defvar sharad/enable-startup-interrupting-feature-hook nil
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.")
(defvar sharad/disable-startup-interrupting-feature-hook nil
           "Run only when emacs start from this file only, it
           basically run when this ful get loaded at emacs start
           time")
(defvar sharad/enable-login-session-interrupting-feature-hook nil
      "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.")
(defvar sharad/disable-login-session-interrupting-feature nil
     "called before when last frame deleted, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.")
(defvar *minimum-disable-login-session-frames* 2 "Minimum disable login session frames")


(require 'basic-utils)
(require 'basic-macros)
(require 'perforce-test)

(add-hook 'after-init-hook
          #'(lambda ()
              (run-each-hooks '*sharad/after-init-hook*)))

;;}}

;; (defvar startup-select-frame-fn #'select-frame "startup-select-frame-fn")
(defvar startup-select-frame-fn #'select-frame-set-input-focus "startup-select-frame-fn")

(progn ;; "disable startup interrupting feature till first frame created."
  (defvar enable-p4-login nil "test")

  (progn "disable-startup-interrupting-feature"
         (when nil
          (defvar sharad/disable-startup-interrupting-feature-hook nil
      "Run only when emacs start from this file only, it basically run when this ful get loaded at emacs start time"))
    (progn ;;  "xxdis"
      (defun general-disable-startup-setting ()
        (interactive)
        (setq pabbrev-read-only-error nil)
        (setq
         enable-p4-login nil
         tramp-mode nil
         ido-mode nil)
        (when (featurep 'epa)
          (if (fboundp 'epa-file-disable)
              (epa-file-disable))))
      (add-hook 'sharad/disable-startup-interrupting-feature-hook 'general-disable-startup-setting))

    (defun sharad/disable-startup-interrupting-feature ()
      "Run only when emacs start from this file only,
it basically run when this ful get loaded at emacs start time,
its purpose to disable all interrupting feature that may cause
problem while emacs startup in daemon mode, non-interactively."
      (interactive)
      (with-report-error "check"
          (when nil
            (unless debug-on-error                  ;I am running in --debug-init
              (setq debug-on-error nil)))
          ;; (setq
          ;;  enable-p4-login nil
          ;;  tramp-mode nil
          ;;  ido-mode nil)
          ;; (deh-featurep epa
          ;;   (if (fboundp 'epa-file-disable)
          ;;       (epa-file-disable)))
          ;; (global-pabbrev-mode -1)
          ;; (run-hooks 'sharad/disable-startup-interrupting-feature-hook)
          (run-each-hooks 'sharad/disable-startup-interrupting-feature-hook)
          (message "sharad/disable-startup-interrupting-feature() completed Seen.")))

    ;; run now
    ;; (sharad/disable-startup-interrupting-feature)



    (defun sharad/disable-startup-interrupting-feature-in-frame-once (&optional frame)
      ;; NOTE: Can not be called in hook.
      ;; (funcall startup-select-frame-fn frame)
      ;; (with-report-error "check"
      ;;                    (sharad/enable-startup-interrupting-feature))
      (sharad/disable-startup-interrupting-feature)
      (remove-hook 'after-init-hook 'sharad/disable-startup-interrupting-feature-in-frame-once))

    ;;(add-hook 'after-init-hook 'sharad/disable-startup-interrupting-feature-in-frame-once)
    ;; cause problems at the end of deamon strtup, consider implementing after, before lib loads.

    ;; run now
    (sharad/disable-startup-interrupting-feature-in-frame-once))



  (progn ;; "enable-startup-interrupting-feature"
   (when nil
     (defvar sharad/enable-startup-interrupting-feature-hook nil
      "Run only once when when very frame got created after emacs startup. Feature that were disabled for proper startup of emacs will get re-enabled here."))

    (progn ;; "xxen"
      (defun general-enable-startup-setting ()
        (interactive)
        (setq pabbrev-read-only-error nil)
        (setq
         enable-p4-login t
         tramp-mode t
         ido-mode t)
        (when (featurep 'epa)
          (if (fboundp 'epa-file-enable)
              (epa-file-enable)))
        ;; do in add-hook
        ;; (when (fboundp 'login-to-perforce)
        ;;  (login-to-perforce))
        )
      (add-hook 'sharad/enable-startup-interrupting-feature-hook 'general-enable-startup-setting t))

    (defun sharad/enable-startup-interrupting-feature ()
      "Run only once when when very frame got created after emacs startup.
its purpose to re/enable all feature that may have cuused problem in emacs
startup in daemon mode."
      (interactive)
      ;; test
      (with-report-error "check"
          ;; why desktop-restore not running.
          (progn
            ;; (setq enable-p4-login t
            ;;       tramp-mode t
            ;;       ido-mode 'both)
            ;; (login-to-perforce)
            ;; ;; (update-ssh-agent t) ;; should be called when tramp file accessed. - see how it will work in case sharad/desktop-session-restore.
            ;;test
            ;; (deh-featurep epa
            ;;   (if (fboundp 'epa-file-enable)
            ;;       (epa-file-enable)))
            (general-enable-startup-setting) ;could not run from sharad/enable-startup-interrupting-feature-hook
            ;as needed before the function in sharad/enable-startup-interrupting-feature-hook.
            (when (and
                   (featurep 'light-symbol)
                   (featurep 'hilit-chg))
              (add-element-to-lists '(lambda ()
                                      (light-symbol-mode 1)
                                      (highlight-changes-visible-mode t)
                                      (highlight-changes-mode t)) pgm-langs)))
          (run-each-hooks 'sharad/enable-startup-interrupting-feature-hook)
          ;; (sharad/desktop-session-restore)
          (message "sharad/enable-startup-interrupting-feature() completed Seen.")
        (setq debug-on-error t )))

    (defvar sharad/enable-startup-interrupting-feature-in-frame-once-lock nil "Lock for sharad/enable-startup-interrupting-feature-in-frame-once")
    (defun sharad/enable-startup-interrupting-feature-in-frame-once (frame)
      (if sharad/enable-startup-interrupting-feature-in-frame-once-lock
          (message-notify "sharad/enable-startup-interrupting-feature-in-frame-once" "locked due to sharad/enable-startup-interrupting-feature-in-frame-once-lock is t")
          (progn
            (setq sharad/enable-startup-interrupting-feature-in-frame-once-lock t)
            (funcall startup-select-frame-fn frame)
            ;; (with-report-error "check"
            ;;                    (sharad/enable-startup-interrupting-feature))
            (sharad/enable-startup-interrupting-feature)
            (remove-hook 'after-make-frame-functions 'sharad/enable-startup-interrupting-feature-in-frame-once)
            (setq sharad/enable-startup-interrupting-feature-in-frame-once-lock nil))))

    (add-hook 'after-make-frame-functions 'sharad/enable-startup-interrupting-feature-in-frame-once)))

;;;###autoload
(defun add-to-enable-startup-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-hook
   'sharad/enable-startup-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-startup-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'sharad/enable-startup-interrupting-feature-hook
   fn
   local))

  ;; (sharad/enable-startup-interrupting-feature-in-frame-once (selected-frame))
;;}}

;;{{
(progn ;; "login-session-interrupting-feature"
  (when nil
   (defvar *minimum-disable-login-session-frames* 2 "Minimum disable login session frames"))

  (defun any-frame-opened-p ()
    (>= (length (frame-list)) *minimum-disable-login-session-frames*))


  ;; don't mislead by login it is when no frame or 1 or more frame hook
  ;; basiclly used accross login where emacs daemon outlive.
  ;; can be used for other purpose.
  (progn ;; "disable-login-session-interrupting-feature"

    (when nil
      (defvar sharad/disable-login-session-interrupting-feature nil
     "called before when last frame deleted, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."))

   (defun sharad/disable-login-session-interrupting-feature ()
     (interactive)
     ;; (login-to-perforce)
     ;; (update-ssh-agent t)
     (setq debug-on-error nil)           ;for planner
     (with-report-error "check"
         (run-each-hooks 'sharad/disable-login-session-interrupting-feature)))

   (defun sharad/disable-login-session-interrupting-feature-in-frame-once (f)
     (when (any-frame-opened-p) ;last
                                                                            ;frame
                                                                            ;then
                                                                            ;add.
       (with-report-error "check"
           (sharad/disable-login-session-interrupting-feature)
           (add-hook 'after-make-frame-functions 'sharad/enable-login-session-interrupting-feature-in-frame-once t)
           (message "added sharad/enable-login-session-interrupting-feature-in-frame-once"))))

   (add-hook 'delete-frame-functions 'sharad/disable-login-session-interrupting-feature-in-frame-once))

  (progn ;; "enable-login-session-interrupting-feature"

    (when nil
      (defvar sharad/enable-login-session-interrupting-feature-hook nil
      "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."))

    (defun sharad/enable-login-session-interrupting-feature ()
      (interactive)
      ;; (setenv "DISPLAY" ":1")
      (with-report-error "check"
          ;; do in add-hook
          ;; (when (fboundp 'login-to-perforce)
          ;;   (login-to-perforce))
          ;; (update-ssh-agent t)  ; test
          ;; (update-ssh-agent) ;; should be called when tramp file accessed. - see how it will work in case sharad/desktop-session-restore.
          (setq debug-on-error t)           ;for planner
          (run-each-hooks 'sharad/enable-login-session-interrupting-feature-hook)))

    (defun sharad/enable-login-session-interrupting-feature-in-frame-once (frame)
      (funcall startup-select-frame-fn frame)
      ;; run and disable.
      (with-report-error "check"
          (when (any-frame-opened-p)
            (sharad/enable-login-session-interrupting-feature))
          (remove-hook 'after-make-frame-functions 'sharad/enable-login-session-interrupting-feature-in-frame-once)
          (when t
            (message "removed sharad/enable-login-session-interrupting-feature-in-frame-once"))))

    ;; (sharad/enable-login-session-interrupting-feature-in-frame-once (selected-frame))
    (add-hook 'after-make-frame-functions 'sharad/enable-login-session-interrupting-feature-in-frame-once t)))

;;;###autoload
(defun add-to-enable-login-session-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-hook
   'sharad/enable-login-session-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-login-session-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'sharad/enable-login-session-interrupting-feature-hook
   fn
   local))

;;}}

(defalias 'make-local-hook 'ignore)

(provide 'startup-hooks)
;;; startup-hooks.el ends here
