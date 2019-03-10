;;; lotus-idle-utils.el --- Lotus Idle utils         -*- lexical-binding: t; -*-

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

(provide 'lotus-idle-utils)

(require 'frame)
(require 'timer)
(require 'timer-utils-lotus)

(defun quiet--select-frame (frame &optional norecord)
  ;; (select-frame frame norecord)
  ;; select-frame-set-input-focus should not be used as it will pull window if hidden.
  (select-frame frame norecord)
  ;; (raise-frame frame)
  ;; Ensure, if possible, that FRAME gets input focus.
  ;; (when (memq (window-system frame) '(x w32 ns))
  ;;   (x-focus-frame frame))
  ;; Move mouse cursor if necessary.
  (cond
   (mouse-autoselect-window
    (let ((edges (window-inside-edges (frame-selected-window frame))))
      ;; Move mouse cursor into FRAME's selected window to avoid that
      ;; Emacs mouse-autoselects another window.
      (set-mouse-position frame (nth 2 edges) (nth 1 edges))))
   (focus-follows-mouse
    ;; Move mouse cursor into FRAME to avoid that another frame gets
    ;; selected by the window manager.
    (set-mouse-position frame (1- (frame-width frame)) 0))))

(defun select-frame-set-input-focus-raise-p ()
  "Check if function select-frame-set-input-focus used by helm can raise frame?"
  (and
   (advice--p (advice--symbol-function 'select-frame-set-input-focus))
   (advice-function-member-p
    #'quiet--select-frame
    (advice--symbol-function 'select-frame-set-input-focus))))

(defun select-frame-set-input-focus-raise-disable ()
  "Disable raising of frame by function select-frame-set-input-focus used by helm."
  (when (fboundp 'remove-function)
    (remove-function
     (symbol-function 'select-frame-set-input-focus)
     #'quiet--select-frame)))

(defun select-frame-set-input-focus-raise-enable ()
  "Enable raising of frame by function select-frame-set-input-focus used by helm."
  (when (fboundp 'add-function)
    (add-function
     :override (symbol-function  'select-frame-set-input-focus)
     #'quiet--select-frame)))


(defmacro with-pre-command (&rest body)
  "Run BODY before next activity or with pre command hook"
  `(letrec ((fun #'(lambda ()
                    (unwind-protect
                        (progn
                          ,@body)
                      (remove-hook 'pre-command-hook (lambda () (funcall fun)))))))
     (add-hook
      'pre-command-hook
      (lambda () (funcall fun)))))
(put 'with-pre-command 'lisp-indent-function 0)

(defmacro with-post-command (&rest body)
  "Run BODY after next activity or with post command hook"
  `(letrec ((fun #'(lambda ()
                     (unwind-protect
                         (progn
                           ,@body)
                       (remove-hook 'post-command-hook (lambda () (funcall fun)))))))
     (add-hook
      'post-command-hook
      (lambda () (funcall fun)))))
(put 'with-post-command 'lisp-indent-function 0)

(defmacro with-pre-command-local (&rest body)
  "Run BODY before next activity or with local pre command hook"
  `(letrec ((fun #'(lambda ()
                     (unwind-protect
                         (progn
                           ,@body)
                       (remove-hook 'pre-command-hook (lambda () (funcall fun)) t)))))
     (add-hook
      'pre-command-hook
      (lambda () (funcall fun)) nil t)))
(put 'with-pre-command-local 'lisp-indent-function 0)

(defmacro with-post-command-local (&rest body)
  "Run BODY after next activity or with local post command hook"
  `(letrec ((fun #'(lambda ()
                     (unwind-protect
                         (progn
                           ,@body)
                       (remove-hook 'post-command-hook (lambda () (funcall fun)) t)))))
     (add-hook
      'post-command-hook
      (lambda () (funcall fun)) nil t)))
(put 'with-post-command-local 'lisp-indent-function 0)


(defmacro lotus-with-idle-timed-transient-window (timeout window &rest body)
  "Will destroy WINDOW after continued idle TIMEOUT
this macro intended to be used with or in idle timer functions."
  ;; TODO: org-fit-window-to-buffer
  ;; TODO: as clean up reset newwin configuration
  `(let* ((cleanup-fun #'(lambda (w)
                           (when (and w
                                      (windowp w)
                                      (window-valid-p w))
                             (delete-window w)
                             (lwarn 'lotus-idle-timed-window :debug "triggered timer for new-win %s" w)
                             (with-no-active-minibuffer
                               (select-frame-set-input-focus-raise-disable)))))
          (timer      (run-with-idle-plus-timer timeout nil cleanup-fun window)))
     (unwind-protect
         (progn
           (with-post-command
             (cancel-timer timer)
             (setq timer nil))
           (progn
             (select-frame-set-input-focus-raise-enable)
             (progn
               ,@body)))
       (progn
         (select-frame-set-input-focus-raise-disable)
         (when timer
           (cancel-timer timer)
           (setq timer nil))))))
(put 'lotus-idle-timed-window 'lisp-indent-function 2)

(defmacro lotus-with-idle-timed-transient-buffer-window (timeout buffer &rest body)
  "Will destroy WINDOW after continued idle TIMEOUT
this macro intended to be used with or in idle timer functions."
  ;; TODO: org-fit-window-to-buffer
  ;; TODO: as clean up reset newwin configuration
  `(let* ((cleanup-fun #'(lambda (buffer)
                           (let* ((buff   (get-buffer buffer))
                                  (window (if buff (get-buffer-window buff))))
                             (when (and window
                                       (windowp window)
                                       (window-valid-p window))
                              (delete-window window)
                              (lwarn 'lotus-idle-timed-window :debug "triggered timer for new-win %s" window)
                              (with-no-active-minibuffer
                                (select-frame-set-input-focus-raise-disable))))))
          (timer       (run-with-idle-plus-timer timeout nil cleanup-fun buffer)))
     (unwind-protect
         (progn
           (with-post-command
             (cancel-timer timer)
             (setq timer nil))
           (progn
             (select-frame-set-input-focus-raise-enable)
             (progn
               ,@body)))
       (progn
         (select-frame-set-input-focus-raise-disable)
         (when timer
           (cancel-timer timer)
           (setq timer nil))))))
(put 'lotus-idle-timed-transient-buffer-window 'lisp-indent-function 2)


(defmacro lotus-with-idle-timed-transient-win (timeout
                                               timer
                                               cleanupfn-newwin
                                               cleanupfn-local
                                               newwin
                                               &rest body)
  (let ((temp-win-config (make-symbol "test-lotus-with-timed-new-win-config")))
    `(let* ((,temp-win-config (current-window-configuration))
            (,cleanupfn-newwin #'(lambda (w localfn)
                                   ;; (message "cleaning up newwin and triggered timer for newwin %s" w)
                                   (when localfn (funcall localfn))
                                   ;; (when (active-minibuffer-window) ;not required here. it is just creating timed new-win
                                   ;;   (abort-recursive-edit))
                                   (when (and w (windowp w) (window-valid-p w))
                                     (delete-window w))
                                   (when ,temp-win-config
                                     (set-window-configuration ,temp-win-config)
                                     (setq ,temp-win-config nil)))))
       (lotus-with-new-win ,newwin
         (let* ((,timer (run-with-idle-plus-timer ,timeout
                                                  nil
                                                  ,cleanupfn-newwin
                                                  ,newwin
                                                  ,cleanupfn-local)))
           (condition-case err
               (progn
                 ,@body)
             ((quit)
              (funcall ,cleanupfn-newwin ,newwin ,cleanupfn-local))))))))
(put 'lotus-with-idle-timed-transient-win 'lisp-indent-function 1)


;;; lotus-idle-utils.el ends here