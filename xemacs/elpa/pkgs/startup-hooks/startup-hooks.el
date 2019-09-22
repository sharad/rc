;;; startup-hooks.el --- startup hooks               -*- lexical-binding: t; -*-

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

;; (add-hook 'after-init-hook #'startup-hooks-insinuate)
;;

;;; Code:


(provide 'startup-hooks)


;;{{ Run after init

(defvar *lotus-after-init-hook* nil "lotus-after-init-hook")
(defvar lotus-enable-startup-interrupting-feature-hook nil
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here.")
(defvar lotus-disable-startup-interrupting-feature-hook nil
  "Run only when emacs start from this file only, it basically
  run when this ful get loaded at emacs start time")
(defvar lotus-enable-login-session-interrupting-feature-hook nil
  "called before when first frame created, don't mislead by
   login it is for no frame or 1 or more frame hook basiclly
   used accross login where emacs daemon outlive.")
(defvar lotus-disable-login-session-interrupting-feature nil
  "called before when last frame deleted, don't mislead by
   login it is for no frame or 1 or more frame hook basiclly
   used accross login where emacs daemon outlive.")
(defvar *minimum-disable-login-session-frames* 2 "Minimum disable login session frames")



(defvar lotus-disable-startup-begin-debug-on-error        nil)
(defvar lotus-disable-startup-finish-debug-on-error       t)
(defvar lotus-enable-startup-begin-debug-on-error         nil)
(defvar lotus-enable-startup-finish-debug-on-error        t)

(defvar lotus-disable-login-session-begin-debug-on-error  nil)
(defvar lotus-disable-login-session-finish-debug-on-error t)
(defvar lotus-enable-login-session-begin-debug-on-error   t)
(defvar lotus-enable-login-session-finish-debug-on-error  t)

(defvar lotus-disable-startup-begin-debug-on-quit         nil)
(defvar lotus-disable-startup-finish-debug-on-quit        t)
(defvar lotus-enable-startup-begin-debug-on-quit          nil)
(defvar lotus-enable-startup-finish-debug-on-quit         t)

(defvar lotus-disable-login-session-begin-debug-on-quit   nil)
(defvar lotus-disable-login-session-finish-debug-on-quit  t)
(defvar lotus-enable-login-session-begin-debug-on-quit    t)
(defvar lotus-enable-login-session-finish-debug-on-quit   t)


(require 'time-stamp)

(require 'basic-utils)
(require 'basic-macros)
(require 'perforce-test)

(add-hook 'after-init-hook
          #'(lambda ()
              (run-each-hooks '*lotus-after-init-hook*)))



;; (defvar startup-select-frame-fn #'select-frame "startup-select-frame-fn")
(defvar startup-select-frame-fn #'select-frame-set-input-focus "startup-select-frame-fn")
;;(setq startup-select-frame-fn #'select-frame)

;;;###autoload
(defun any-frame-opened-p ()
  (>=
   (length (frame-list))
   *minimum-disable-login-session-frames*))

(defvar enable-p4-login nil "test")
;;}}










;;;{{{ disable startup interrupting feature till first frame created.
;; disable startup interrupting feature till first frame created.
;;;###autoload
(defun lotus-general-disable-startup-setting-begin ()
  (interactive)
  (when nil
    (unless debug-on-error                  ;I am running in --debug-init
      (setq
       debug-on-error lotus-disable-startup-begin-debug-on-error
       debug-on-quit lotus-disable-startup-begin-debug-on-quit)))
  (setq pabbrev-read-only-error nil)
  (setq
   enable-p4-login nil
   tramp-mode nil
   ido-mode nil)
  (when (featurep 'epa)
    (if (fboundp 'epa-file-disable)
        (epa-file-disable))))
(add-hook 'lotus-disable-startup-interrupting-feature-hook 'lotus-general-disable-startup-setting-begin t)

;;;###autoload
(defun lotus-general-disable-startup-setting-finish ()
  (interactive)
  (setq
   debug-on-error lotus-disable-startup-finish-debug-on-error
   debug-on-quit lotus-disable-startup-finish-debug-on-quit))

(add-hook 'lotus-disable-startup-interrupting-feature-hook 'lotus-general-disable-startup-setting-finish)

;;;###autoload
(defun lotus-disable-startup-interrupting-feature ()
  "Run only when emacs start from this file only,
it basically run when this ful get loaded at emacs start time,
its purpose to disable all interrupting feature that may cause
problem while emacs startup in daemon mode, non-interactively."
  (interactive)
  (with-report-error "check"
      (lotus-general-disable-startup-setting-begin)
      (run-each-hooks 'lotus-disable-startup-interrupting-feature-hook)
    (message "lotus-disable-startup-interrupting-feature() completed Seen.")
    (lotus-general-disable-startup-setting-finish)))

;; run now
;; (lotus-disable-startup-interrupting-feature)


;;;###autoload
(defun lotus-disable-startup-interrupting-feature-in-frame-once (&optional frame)
  ;; NOTE: Can not be called in hook.
  (lotus-disable-startup-interrupting-feature)
  (remove-hook 'after-init-hook 'lotus-disable-startup-interrupting-feature-in-frame-once))

;;(add-hook 'after-init-hook 'lotus-disable-startup-interrupting-feature-in-frame-once)
;; cause problems at the end of deamon strtup, consider implementing after, before lib loads.
;; run now
(lotus-disable-startup-interrupting-feature-in-frame-once)



;; "enable-startup-interrupting-feature"

;;;###autoload
(defun lotus-general-enable-startup-setting-begin ()
  (interactive)
  (setq
   debug-on-error lotus-enable-startup-begin-debug-on-error
   debug-on-quit lotus-enable-startup-begin-debug-on-quit)
  (setq pabbrev-read-only-error nil)
  (setq
   enable-p4-login t
   tramp-mode t
   ido-mode t)
  (when (featurep 'epa)
    (if (fboundp 'epa-file-enable)
        (epa-file-enable)))
  (when (and
         (featurep 'light-symbol)
         (featurep 'hilit-chg))
    (add-element-to-lists '(lambda ()
                            (light-symbol-mode 1)
                            (highlight-changes-visible-mode t)
                            (highlight-changes-mode t))
                          pgm-langs)))
(add-hook 'lotus-enable-startup-interrupting-feature-hook 'lotus-general-enable-startup-setting-begin t)
;;;###autoload
(defun lotus-general-enable-startup-setting-finish ()
  (interactive)
  (setq
   debug-on-error lotus-enable-startup-finish-debug-on-error
   debug-on-quit lotus-enable-startup-finish-debug-on-quit))
(add-hook 'lotus-enable-startup-interrupting-feature-hook 'lotus-general-enable-startup-setting-finish)

;;;###autoload
(defun lotus-enable-startup-interrupting-feature ()
  "Run only once when when very frame got created after emacs startup.
its purpose to re/enable all feature that may have cuused problem in emacs
startup in daemon mode."
  (interactive)
  ;; test
  (with-report-error "check"
      ;; why desktop-restore not running.

      ;;;could not run from lotus-enable-startup-interrupting-feature-hook
      ;;;as needed before the function in lotus-enable-startup-interrupting-feature-hook.
      (lotus-general-enable-startup-setting-begin)
      (run-each-hooks 'lotus-enable-startup-interrupting-feature-hook)
      (message "lotus-enable-startup-interrupting-feature() completed Seen.")
    (lotus-general-enable-startup-setting-finish)))

(defvar lotus-enable-startup-interrupting-feature-in-frame-once-lock nil "Lock for lotus-enable-startup-interrupting-feature-in-frame-once")

;;;###autoload
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

(add-hook 'after-make-frame-functions 'lotus-enable-startup-interrupting-feature-in-frame-once)
;;;}}}


;;;###autoload
(defun add-to-enable-startup-interrupting-feature-hook (fn &optional append local)
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here."
  (interactive)
  (message "%s: add-to-enable-startup-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (add-to-hook
   'lotus-enable-startup-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-startup-interrupting-feature-hook (fn &optional local)
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here."
  (interactive)
  (message "%s: remove-from-enable-startup-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (remove-hook
   'lotus-enable-startup-interrupting-feature-hook
   fn
   local))

;;;###autoload
(defun add-to-disable-startup-interrupting-feature-hook (fn &optional append local)
  "Run only once when when very frame got created after emacs
  startup. Feature that were disabled for proper startup of emacs
  will get re-enabled here."
  (interactive)
  (message "%s: add-to-disable-startup-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (add-to-hook
   'lotus-disable-startup-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-disable-startup-interrupting-feature-hook (fn &optional local)
  (interactive)
  (message "%s: remove-from-disable-startup-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (remove-hook
   'lotus-disable-startup-interrupting-feature-hook
   fn
   local))
  ;; (lotus-enable-startup-interrupting-feature-in-frame-once (selected-frame))
;;}}





;;{{ login-session-interrupting-feature
;; don't mislead by login it is when no frame or 1 or more frame hook
;; basiclly used accross login where emacs daemon outlive.
;; can be used for other purpose.

;; "disable-login-session-interrupting-feature"
;;;###autoload
(defun lotus-general-disable-login-session-setting-begin ()
  (interactive)
  (setq
   debug-on-error lotus-disable-login-session-begin-debug-on-error
   debug-on-quit lotus-disable-login-session-begin-debug-on-quit))
(add-hook 'lotus-disable-startup-interrupting-feature-hook 'lotus-general-disable-login-session-setting-begin t)

;;;###autoload
(defun lotus-general-disable-login-session-setting-finish ()
  (interactive)
  (setq
   debug-on-error lotus-disable-login-session-finish-debug-on-error
   debug-on-quit lotus-disable-login-session-finish-debug-on-quit))
(add-hook 'lotus-disable-login-session-interrupting-feature-hook 'lotus-general-disable-login-session-setting-finish)

;;;###autoload
(defun lotus-disable-login-session-interrupting-feature ()
  (interactive)
  (with-report-error "check"
      (lotus-general-disable-login-session-setting-begin)
      (run-each-hooks 'lotus-disable-login-session-interrupting-feature)
      (lotus-general-disable-login-session-setting-finish)))

;;;###autoload
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

(add-hook 'delete-frame-functions 'lotus-disable-login-session-interrupting-feature-in-frame-once)


;; enable-login-session-interrupting-feature

;;;###autoload
(defun lotus-general-enable-login-session-setting-begin ()
  (interactive)
  (setq
                                        ;for planner
   debug-on-error lotus-enable-login-session-begin-debug-on-error))
(add-hook 'lotus-enable-startup-interrupting-feature-hook 'lotus-general-enable-login-session-setting-begin t)

;;;###autoload
(defun lotus-general-enable-login-session-setting-finish ()
  (interactive)
  (setq debug-on-error lotus-enable-login-session-finish-debug-on-error))
(add-hook 'lotus-enable-login-session-interrupting-feature-hook 'lotus-general-enable-login-session-setting-finish)

;;;###autoload
(defun lotus-enable-login-session-interrupting-feature ()
  (interactive)
  (with-report-error "check"
      ;; do in add-hook
      (lotus-general-enable-login-session-setting-begin)
      (run-each-hooks 'lotus-enable-login-session-interrupting-feature-hook)
      (lotus-general-enable-login-session-setting-finish)))

;;;###autoload
(defun lotus-enable-login-session-interrupting-feature-in-frame-once (frame)
  (funcall startup-select-frame-fn frame)
  ;; run and disable.
  (with-report-error "check"
      (when (any-frame-opened-p)
        (lotus-enable-login-session-interrupting-feature))
      (remove-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once)
      (when t
        (message "removed lotus-enable-login-session-interrupting-feature-in-frame-once"))))

(add-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once t)




















;;;###autoload
(defun add-to-enable-login-session-interrupting-feature-hook (fn &optional append local)
  "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."
  (interactive)
  (message "%s: add-to-enable-login-session-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (add-to-hook
   'lotus-enable-login-session-interrupting-feature-hook
   fn
   append
   local))

;;;###autoload
(defun remove-from-enable-login-session-interrupting-feature-hook (fn &optional local)
  "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."
  (interactive)
  (message "%s: remove-from-enable-login-session-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
  (remove-hook
   'lotus-enable-login-session-interrupting-feature-hook
   fn
   local))

(defun add-to-disable-login-session-interrupting-feature-hook (fn &optional append local)
  "called before when first frame created, don't mislead by
    login it is for no frame or 1 or more frame hook basiclly
    used accross login where emacs daemon outlive."
  (interactive)
  (message "%s: add-to-disable-login-session-interrupting-feature-hook: \n%s"
           (time-stamp-string)
           (pp-to-string fn))
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


;;;###autoload
(defun startup-hooks-insinuate ()
  (interactive)
  (add-hook 'after-make-frame-functions 'lotus-enable-startup-interrupting-feature-in-frame-once)
  (add-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once t))

(defun startup-hooks-uninsinuate ()
  (interactive)
  (remove-hook 'after-make-frame-functions 'lotus-enable-startup-interrupting-feature-in-frame-once)
  (remove-hook 'after-make-frame-functions 'lotus-enable-login-session-interrupting-feature-in-frame-once))

;; (setq after-make-frame-functions '(persp-init-new-frame elscreen-make-frame-confs muse-make-faces eyebrowse-init evil-init-esc x-dnd-init-frame))
;; (setq after-make-frame-functions nil)


(when nil
  after-make-frame-functions

 '(w3m-add-w3m-initial-frames persp-init-new-frame elscreen-make-frame-confs muse-make-faces eyebrowse-init evil-init-esc x-dnd-init-frame (closure (t) (frame) (run-with-idle-timer 3 nil #'set-default-face-height-by-resolution) (set-default-face-height-by-resolution)) (lambda (f) (run-at-time "1 sec" nil 'emacs-uptime)) (lambda (nframe) (run-at-time-or-now 100 '(lambda nil (if (any-frame-opened-p) (org-clock-in-if-not))))) call-org-clock-in-if-not-at-time-delay-frame-fn)

 (setq after-make-frame-functions
       '( persp-init-new-frame elscreen-make-frame-confs muse-make-faces eyebrowse-init evil-init-esc x-dnd-init-frame call-org-clock-in-if-not-at-time-delay-frame-fn))

 (setq after-make-frame-functions nil))

(defalias 'make-local-hook 'ignore)

;;; startup-hooks.el ends here
