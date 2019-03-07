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


(defun select-frame-set-input-focus-raise-p ()
  "Check if function select-frame-set-input-focus used by helm can raise frame?"
  (and
   (advice--p (advice--symbol-function 'select-frame-set-input-focus))
   (advice-function-member-p #'quiet--select-frame (advice--symbol-function 'select-frame-set-input-focus))))

(defun select-frame-set-input-focus-raise-disable ()
  "Disable raising of frame by function select-frame-set-input-focus used by helm."
  (when (fboundp 'remove-function)
    (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)))

(defun select-frame-set-input-focus-raise-enable ()
  "Enable raising of frame by function select-frame-set-input-focus used by helm."
  (when (fboundp 'add-function)
    (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)))


(defmacro lotus-idle-timed-transient-window (timeout window &rest body)
  "Will destroy WINDOW after continued idle TIMEOUT
this macro intended to be used with or in idle timer functions."
   ;; TODO: org-fit-window-to-buffer
   ;; TODO: as clean up reset newwin configuration
  `(let* ((cleanup-fun #'(lambda (w)
                           (when (and w
                                      (windowp w)
                                      (window-valid-p w))
                             (delete-window w)
                             (lwarn 'lotus-timed-window :debug "triggered timer for new-win %s" w)
                             (with-no-active-minibuffer
                               (select-frame-set-input-focus-raise-disable)))))
          (timer      (run-with-idle-plus-timer timeout nil cleanup-fun window)))
     (unwind-protect
         (add-hook 'post-command-hook
                   #'(lambda () (cancel-timer timer)))
         (progn
           (select-frame-set-input-focus-raise-enable)
           ,@body)
       (progn
         (select-frame-set-input-focus-raise-disable)
         (when timer
           (cancel-timer timer)
           (setq timer nil))))))
(put 'lotus-idle-timed-window 'lisp-indent-function 2)

(defmacro lotus-idle-timed-transient-buffer-window (timeout buffer &rest body)
  "Will destroy BUFFER window after continued idle TIMEOUT
this macro intended to be used with or in idle timer functions."
  `(let ((buff (get-buffer buffer))
         (window (if buff (get-buffer-window buff))))
     (when window
       (lotus-idle-timed-transient-window ,timeout window
         ,@body))))
(put 'lotus-idle-timed-transient-buffer-window 'lisp-indent-function 2)



;;; lotus-idle-utils.el ends here
