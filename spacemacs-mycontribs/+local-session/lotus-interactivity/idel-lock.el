;;; idle-lock.el --- config                             -*- lexical-binding: t; -*-

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



(progn ;; "idle timeout lock"

  (defvar lock-passwd-keymap nil "lock passwd keymap")
  (setq lock-passwd-keymap (make-sparse-keymap))
  (define-key lock-passwd-keymap (kbd "C-j") 'newline)
  (define-key lock-passwd-keymap (kbd "RET") 'exit-minibuffer)
  (define-key lock-passwd-keymap (kbd "p") 'self-insert-command)

  (when nil
   (let ()
    (defun reset-global-map ()
      (interactive)
      (use-global-map global-map)
      (message "Done"))
    (run-at-time "1 min" nil 'reset-global-map)
    (use-global-map lock-passwd-keymap)
    (message (read-from-minibuffer "ABC: " "p" lock-passwd-keymap))
    (use-global-map global-map)))



  (defvar lock-emacs-master-password "abcd123" "lock emacs mster password")
  (defvar lock-emacs-password nil "lock emacs password")
  (defun set-emacs-lock-passwd ()
    (interactive)
    (let ((passwd (read-from-minibuffer "emacs lock passwd: " lock-emacs-password)))
      (setq lock-emacs-password passwd)
      (message "emacs lock password set to: %s" passwd)))

  (defun lock-emacs (&optional timeout)
    (interactive)
    (let ((timeout (or timeout 7)))
      (cond ((= (shell-command "pgrep xtrlock") 0)
             (message "Screen is already locked."))
            ((null lock-emacs-password)
             (message "password is not set, set it using set M-x set-emacs-lock-passwd"))
            (t
             (if (or
               (< (length (frame-list)) 2)
               (ad-find-advice 'call-interactively 'around 'lock))
              (message "Not locking emacs.")
              (progn
                (define-key (current-global-map) [remap self-insert-command] 'ignore)
                (eval
                 `(defadvice call-interactively (around lock activate)
                    ;; ,(help-function-interactive 'fun)
                    (catch 'foo
                      (condition-case e
                          (let ()
                            (define-key (current-global-map) [remap self-insert-command] 'ignore)
                            (when (or (< (length (frame-list)) 2) ;; *minimum-disable-login-session-frames*)
                                      (let ((passwd (with-timeout (,timeout (progn (message nil) (throw 'foo nil)))
                                                     (read-passwd "unlock password: "))))
                                        (or
                                         (string-equal passwd lock-emacs-password)
                                         (string-equal passwd lock-emacs-master-password))))
                              (unwind-protect
                                   ad-do-it
                                (progn
                                  (define-key (current-global-map) [remap self-insert-command] nil)
                                  (when (ad-find-advice 'call-interactively 'around 'lock)
                                    (ad-remove-advice 'call-interactively 'around 'lock)
                                    (ad-activate 'call-interactively)
                                    (ad-update  'call-interactively))))))
                        ('error
                         (progn
                           (message "Error: %s" e)
                           (define-key (current-global-map) [remap self-insert-command] nil)
                           (when (ad-find-advice 'call-interactively 'around 'lock)
                             (ad-remove-advice 'call-interactively 'around 'lock)
                             (ad-activate 'call-interactively)
                             (ad-update  'call-interactively))))))))
                (ad-enable-advice 'call-interactively 'around 'lock)
                (ad-activate 'call-interactively)
                (ad-update 'call-interactively)))))))

  ;; (defalias 'lock-emacs 'set-emacs-lock)

  (defvar zone-timer nil "Zone timer.")

  (defun zone-with-lock ()
    (unless (ad-find-advice 'call-interactively 'around 'lock)
      (lock-emacs 7))
    (zone))

  (defun enable-zone-lock ()
    (interactive)
    (setq zone-timer (run-with-idle-timer 600 t 'zone-with-lock)))

  (defun disable-zone-lock ()
    (interactive)
    (when zone-timer
      (cancel-timer zone-timer)
      (setq zone-timer nil)))

  ;; To enable Zone Mode for al


  ;; minutes, add the following Lisp code to you InitFile or try it out
  ;; by EvaluatingExpressions.

  ;; (require 'zone)
  ;; (zone-when-idle 120)

  ;; This will also enable Zone Mode after 2 minutes of idle.

  ;; (setq zone-timer (run-with-idle-timer 120 t
  ;;                                       '(lambda ()
  ;;                                         (zone)
  ;;                                         (set-emacs-lock))))

  ;; (cancel-timer zone-timer)

  )

(provide 'idle-lock)
;;; idle-lock.el ends here
