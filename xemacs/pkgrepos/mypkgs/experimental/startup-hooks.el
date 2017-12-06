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

(defvar *lotus-after-init-hook* nil "lotus-after-init-hook")
(defvar lotus-enable-startup-interrupting-feature-hook nil
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.")
(defvar lotus-disable-startup-interrupting-feature-hook nil
           "Run only when emacs start from this file only, it
           basically run when this ful get loaded at emacs start
           time")
(defvar lotus-enable-login-session-interrupting-feature-hook nil
      "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.")
(defvar lotus-disable-login-session-interrupting-feature nil
     "called before when last frame deleted, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive.")
(defvar *minimum-disable-login-session-frames* 2 "Minimum disable login session frames")


(require 'basic-utils)
(require 'basic-macros)
(require 'perforce-test)

(add-hook 'after-init-hook
          #'(lambda ()
              (run-each-hooks '*lotus-after-init-hook*)))

;;}}

;; (defvar startup-select-frame-fn #'select-frame "startup-select-frame-fn")
(defvar startup-select-frame-fn #'select-frame-set-input-focus "startup-select-frame-fn")

(progn ;; "disable startup interrupting feature till first frame created."
  (defvar enable-p4-login nil "test")

  (progn "disable-startup-interrupting-feature"
         (when nil
          (defvar lotus-disable-startup-interrupting-feature-hook nil
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
      (add-hook 'lotus-disable-startup-interrupting-feature-hook 'general-disable-startup-setting))

    (defun lotus-disable-startup-interrupting-feature ()
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
          ;; (run-hooks 'lotus-disable-startup-interrupting-feature-hook)
          (run-each-hooks 'lotus-disable-startup-interrupting-feature-hook)
          (message "lotus-disable-startup-interrupting-feature() completed Seen.")))

    ;; run now
    ;; (lotus-disable-startup-interrupting-feature)



    (defun lotus-disable-startup-interrupting-feature-in-frame-once (&optional frame)
      ;; NOTE: Can not be called in hook.
      ;; (funcall startup-select-frame-fn frame)
      ;; (with-report-error "check"
      ;;                    (lotus-enable-startup-interrupting-feature))
      (lotus-disable-startup-interrupting-feature)
      (remove-hook 'after-init-hook 'lotus-disable-startup-interrupting-feature-in-frame-once))

    ;;(add-hook 'after-init-hook 'lotus-disable-startup-interrupting-feature-in-frame-once)
    ;; cause problems at the end of deamon strtup, consider implementing after, before lib loads.

    ;; run now
    (lotus-disable-startup-interrupting-feature-in-frame-once))



  (progn ;; "enable-startup-interrupting-feature"
   (when nil
     (defvar lotus-enable-startup-interrupting-feature-hook nil
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
      (add-hook 'lotus-enable-startup-interrupting-feature-hook 'general-enable-startup-setting t))

    (defun lotus-enable-startup-interrupting-feature ()
      "Run only once when when very frame got created after emacs startup.
its purpose to re/enable all feature that may have cuused problem in emacs
startup in daemon mode."
      (interactive)
      ;; test
      (with-report-error "check"
          ;; why desktop-restore not running.
          (progn
            (general-enable-startup-setting) ;could not run from lotus-enable-startup-interrupting-feature-hook
            ;as needed before the function in lotus-enable-startup-interrupting-feature-hook.
            (when (and
                   (featurep 'light-symbol)
                   (featurep 'hilit-chg))
              (add-element-to-lists '(lambda ()
                                      (light-symbol-mode 1)
                                      (highlight-changes-visible-mode t)
                                      (highlight-changes-mode t)) pgm-langs)))
          (run-each-hooks 'lotus-enable-startup-interrupting-feature-hook)
          ;; (lotus-desktop-session-restore)
          (message "lotus-enable-startup-interrupting-feature() completed Seen.")
        (setq debug-on-error t )))

    (defvar lotus-enable-startup-interrupting-feature-in-frame-once-lock nil "Lock for lotus-enable-startup-interrupting-feature-in-frame-once")
    (defun lotus-enable-startup-interrupting-feature-in-frame-once (frame)
      (if lotus-enable-startup-interrupting-feature-in-frame-once-lock
          (message-notify "lotus-enable-startup-interrupting-feature-in-frame-once" "locked due to lotus-enable-startup-interrupting-feature-in-frame-once-lock is t")
          (progn
            (setq lotus-enable-startup-interrupting-feature-in-frame-once-lock t)
            (funcall startup-select-frame-fn frame)
            ;; (with-report-error "check"
            ;;                    (lotus-enable-startup-interrupting-feature))
            (lotus-enable-startup-interrupting-feature)
            (remove-hook 'after-make-frame-functions 'lotus-enable-startup-interrupting-feature-in-frame-once)
            (setq lotus-enable-startup-interrupting-feature-in-frame-once-lock nil))))

    (add-hook 'after-make-frame-functions 'lotus-enable-startup-interrupting-feature-in-frame-once)))

;;;###autoload
(defun add-to-enable-startup-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-enable-startup-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-startup-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-enable-startup-interrupting-feature-hook
   fn
   local))

;;;###autoload
(defun add-to-disable-startup-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-disable-startup-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-disable-startup-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-disable-startup-interrupting-feature-hook
   fn
   local))
  ;; (lotus-enable-startup-interrupting-feature-in-frame-once (selected-frame))
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
      (defvar lotus-disable-login-session-interrupting-feature nil
     "called before when last frame deleted, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."))

   (defun lotus-disable-login-session-interrupting-feature ()
     (interactive)
     ;; (login-to-perforce)
     ;; (update-ssh-agent t)
     (setq debug-on-error nil)           ;for planner
     (with-report-error "check"
         (run-each-hooks 'lotus-disable-login-session-interrupting-feature)))

   (defun lotus-disable-login-session-interrupting-feature-in-frame-once (f)
     (if (any-frame-opened-p) ;last
                                                                            ;frame
                                                                            ;then
                                                                            ;add.
       (with-report-error "check"
           (lotus-disable-login-session-interrupting-feature)
           (add-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once t)
           (message "added lotus-enable-login-session-interrupting-feature-in-frame-once"))
       (message "lotus-disable-login-session-interrupting-feature-in-frame-once: not running.")))

   (add-hook 'delete-frame-functions 'lotus-disable-login-session-interrupting-feature-in-frame-once))

  (progn ;; "enable-login-session-interrupting-feature"

    (when nil
      (defvar lotus-enable-login-session-interrupting-feature-hook nil
      "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."))

    (defun lotus-enable-login-session-interrupting-feature ()
      (interactive)
      ;; (setenv "DISPLAY" ":1")
      (with-report-error "check"
          ;; do in add-hook
          ;; (when (fboundp 'login-to-perforce)
          ;;   (login-to-perforce))
          ;; (update-ssh-agent t)  ; test
          ;; (update-ssh-agent) ;; should be called when tramp file accessed. - see how it will work in case lotus-desktop-session-restore.
          (setq debug-on-error t)           ;for planner
          (run-each-hooks 'lotus-enable-login-session-interrupting-feature-hook)))

    (defun lotus-enable-login-session-interrupting-feature-in-frame-once (frame)
      (funcall startup-select-frame-fn frame)
      ;; run and disable.
      (with-report-error "check"
          (when (any-frame-opened-p)
            (lotus-enable-login-session-interrupting-feature))
          (remove-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once)
          (when t
            (message "removed lotus-enable-login-session-interrupting-feature-in-frame-once"))))

    ;; (lotus-enable-login-session-interrupting-feature-in-frame-once (selected-frame))
    (add-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once t)))

;;;###autoload
(defun add-to-enable-login-session-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-enable-login-session-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-login-session-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-enable-login-session-interrupting-feature-hook
   fn
   local))

(defun add-to-disable-login-session-interrupting-feature-hook (fn &optional append local)
  (interactive)
  (add-to-hook
   'lotus-disable-login-session-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-disable-login-session-interrupting-feature-hook (fn &optional local)
  (interactive)
  (remove-hook
   'lotus-disable-login-session-interrupting-feature-hook
   fn
   local))

;;}}

(defalias 'make-local-hook 'ignore)

(provide 'startup-hooks)
;;; startup-hooks.el ends here
