;;; reader-config.el --- Reader Config

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(deh-require-maybe centered-cursor-mode
;; To activate do:
;;     M-x centered-cursor-mode
;; for buffer local or
;;     M-x global-centered-cursor-mode
;; for global minor mode.
  (setq ccm-vpos-init 0))


;;{{ from: http://www.gnu.org/software/emacs/manual/html_node/cl/
(require 'cl)







(defvar reader-mode-smooth-read-start-hook nil "")
(defvar reader-mode-smooth-read-end-hook nil "")
(defvar reader-mode-resume-hook nil "")
(defvar reader-mode-pause-hook nil "")



(defvar reader-idle-timer nil "")
(defvar smooth-step-timer nil "")

(defvar reader-idle-time 7 "Reader idle time")
(defvar  reader-idle-repeat-time 7 "Reader idle repeat time")
(defvar reader-cmd #'forward-char "command")
(defvar reader-repeat 0.25 "repeat interval")

(add-hook 'reader-mode-smooth-read-start-hook
          #'(lambda ()

              ))

(add-hook 'reader-mode-smooth-read-end-hook
          #'(lambda ()
              ))

(add-hook 'reader-mode-pause-hook
          #'(lambda ()
              (if (and (boundp 'old-cursor-type)
                       old-cursor-type)
                  (set (make-local-variable 'cursor-type) old-cursor-type))
              (if (and (boundp 'old-centered-cursor-mode)
                       old-centered-cursor-mode)
                  (centered-cursor-mode old-centered-cursor-mode))
              (if (and (boundp 'old-hl-line-when-idle-p)
                       old-hl-line-when-idle-p)
                  (hl-line-toggle-when-idle old-hl-line-when-idle-p))
              (if (and (boundp 'old-view-mode)
                       old-view-mode)
                  (view-mode old-view-mode))))

(add-hook 'reader-mode-resume-hook
          #'(lambda ()
              (set (make-local-variable 'old-cursor-type) cursor-type)
              (set (make-local-variable 'cursor-type) nil)
              (set (make-local-variable 'old-hl-line-when-idle-p) hl-line-when-idle-p)
              (hl-line-toggle-when-idle -1)
              (set (make-local-variable 'old-centered-cursor-mode) centered-cursor-mode)
              (centered-cursor-mode t)
              (set (make-local-variable 'old-view-mode) view-mode)
              (view-mode t)))


(define-minor-mode reader-mode
    "Prepare for working with collarative office project."
  :initial-value nil
  :lighter " Reader"
  :global nil
  (if reader-mode
      (progn
        (set (make-local-variable 'reader-idle-time) reader-idle-time)
        (set (make-local-variable 'reader-idle-repeat-time) reader-idle-repeat-time)
        (set (make-local-variable 'reader-cmd) reader-cmd)
        (set (make-local-variable 'reader-repeat) reader-repeat)
        (set (make-local-variable 'smooth-step-timer) nil)
        (add-hook 'pre-command-hook #'pause-smooth-read)
        (set (make-local-variable 'reader-idle-timer)
             (run-with-idle-timer reader-idle-time reader-idle-repeat-time #'resume-smooth-read))
        (message "hi reader mode %s" reader-idle-timer))
      (progn
        (remove-hook 'pre-command-hook #'pause-smooth-read)
        (cancel-timer reader-idle-timer)
        (cancel-smooth-read)
        (set (make-local-variable 'reader-idle-timer) nil)
        (message "by reader mode"))))

(defun reader-show-timers ()
  (interactive)
  (if reader-idle-timer
      (message "reader-idle-timer %s" reader-idle-timer)
      (message "no reader-idle-timer"))
  (if smooth-step-timer
      (message "smooth-step-timer %s" smooth-step-timer)
      (message "no smooth-step-timer")))

(defun smooth-read ()
  ;; (let ((cmd (key-binding (read-key-sequence "safds: ") t)))
  (set (make-local-variable 'smooth-step-timer)
       (run-with-timer 1 reader-repeat
                       (lambda (cmdx)
                         ; (when smooth-read-active
                         (call-interactively cmdx)
                         (run-hooks 'post-command-hook))
                         ; )
                       ;; #'call-interactively
                       reader-cmd))
  (run-mode-hooks 'reader-mode-smooth-read-start-hook))

(defun pause-smooth-read ()
  (interactive)
  (when (and (boundp 'smooth-step-timer)
             smooth-step-timer)
    (run-mode-hooks 'reader-mode-pause-hook)
    (timer-activate smooth-step-timer t)))

(defun resume-smooth-read ()

  (if smooth-step-timer
    (timer-activate smooth-step-timer)
    (smooth-read))

  (run-mode-hooks 'reader-mode-resume-hook))

(defun cancel-smooth-read ()
  (interactive)
  (when smooth-step-timer
    (cancel-timer smooth-step-timer)
    ;; (set (make-local-variable 'smooth-read-active) nil)
    (set (make-local-variable 'smooth-step-timer) nil)
    (run-mode-hooks 'reader-mode-smooth-read-end-hook)))


;; (funcall #'call-at-steps :micros 800 :fn #'forward-sentence)

;;}}




;; (when nil                               ;Old deprecated
;;   (defun* call-at-steps (&key (count 100) (micros 100) (fn 'next-line))
;;     (loop repeat count do
;;          (progn
;;            (funcall fn)
;;            (sit-for 0 micros))))

;;   (defun smooth-next-line ()
;;     (interactive)
;;     (call-at-steps :fn 'next-line))

;;   (defun smooth-forward-char ()
;;     (interactive)
;;     (call-at-steps :count 10000 :fn 'forward-char))

;;   (defun smooth-step (num key micros)
;;     (interactive "p num: \nkkey: \nnmicrosecs: ")
;;     ;; (let ((cmd (key-binding (read-key-sequence "safds: ") t)))
;;     (let ((cmd (key-binding key t))
;;           (num (if (> num 1) num 100))
;;           (micros (if (> micros 1) micros 100))
;;           )

;;       ;; (message num)
;;       (call-at-steps :count num :micros micros :fn cmd)))



;;   (defun smooth-read ()
;;     (interactive)
;;     (call-at-steps :micros 4800 :fn '(lambda ()
;;                                       (forward-sentence)
;;                                       ;; (speechd-speak-read-sentence)
;;                                       ))))

(provide 'reader-config)
;;; reader-config.el ends here
