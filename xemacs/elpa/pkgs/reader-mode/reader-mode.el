;;; reader-mode.el --- Reader Config

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
;; (setq ccm-vpos-init '(round (window-text-height) 2))
  )


;;{{ from: http://www.gnu.org/software/emacs/manual/html_node/cl/
(require 'cl)
(require 'general-testing)






(defvar reader-mode-smooth-read-start-hook nil "")
(defvar reader-mode-smooth-read-end-hook nil "")

(defvar reader-mode-start-hook nil "")
(defvar reader-mode-end-hook nil "")

(defvar reader-mode-resume-hook nil "")
(defvar reader-mode-pause-hook nil "")



(defvar reader-idle-timer nil "")
(make-variable-buffer-local 'reader-idle-timer)

(defvar smooth-step-timer nil "")
(make-variable-buffer-local 'smooth-step-timer)

(defvar reader-idle-time 7 "Reader idle time")
(make-variable-buffer-local 'reader-idle-time)

(defvar reader-idle-repeat-time 7 "Reader idle repeat time")
(make-variable-buffer-local 'reader-idle-repeat-time)




(defun my-next-line () ;; (&optional arg try-vscroll)
  ""
  (interactive)
  ;; (interactive "^p\np")
  ;; (or arg (setq arg 1))
  (condition-case nil
      (line-move 1 nil nil t)
    (end-of-buffer (goto-char (point-min)))))




;; (defvar reader-cmd #'forward-char "command")
(defvar reader-cmd 'my-next-line "command")
(make-variable-buffer-local 'reader-cmd)

(defvar reader-repeat 0.25 "repeat interval")
(make-variable-buffer-local 'reader-repeat)

(defvar reader-mode-config
  '((mode-line-format)
    (elscreen-display-tab)
    (cursor-type)
    (global-hl-line-mode . -1)
    (hl-line-toggle-when-idle . -1)
    (centered-cursor-mode . t)
    (ccm-vpos-init
     '(or ccm-vpos
       (1- (count-lines (window-start) (point)))))
    (view-mode . t)
    (fullscreen . fullboth))
  "Desired reader mode config")


(defun reader-mode-set-config (key value)
  (interactive
   (let* ((key (car (read-from-string
                     (completing-read "key: "
                                      (mapcar (lambda (k)
                                                (symbol-name (car k))) reader-mode-config)
                                      nil
                                      t))))
          (value (car (read-from-string (read-from-minibuffer
                                         (format "%s value: " key)
                                         (format "%s" (reader-mode-get-config key)))))))
     (list key value)))
  (setq reader-mode-config
   (append (list (cons key value)) reader-mode-config)))


(setq
 reader-mode-resume-hook nil
 reader-mode-pause-hook nil
 reader-mode-start-hook nil
 reader-mode-end-hook nil)

(defun reader-mode-get-config (key)
  (cdr (assoc key reader-mode-config)))

;; ;; To keep cursor on same place
;; (setq ccm-vpos-init
;;       '(or ccm-vpos
;;         (1- (count-lines (window-start) (point)))))
;; (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
;;   (setq ccm-vpos nil) t))


;; (defun test123 ()
;;   (interactive)
;;   (if (called-interactively-p 'interactive)
;;       (message "interactive")
;;       (message "uninteractive")))

;; (global-set-key-if-unbind (kbd "C-c u") 'test123)


(require 'centered-cursor-mode)


;; (run-with-timer 10 nil #'message "hl-line-when-idle-p %s" hl-line-when-idle-p)

(add-hook 'reader-mode-pause-hook
          ;; stop reader
          (lambda ()
            ;; (set (make-local-variable 'reader-mode-smooth-step-active) nil)
            (testing
             (message "reader-mode-pause-hook")
             (reader-show-timers))
            (if (boundp 'old-fullscreen)
                (progn
                  (set-frame-parameter nil 'fullscreen old-fullscreen)
                  (testing (message "pause-hook: old-fullscreen %s" old-fullscreen)))
                (testing (message "no old cursor")))
            (if (boundp 'old-elscreen-display-tab)
                (progn
                  (set (make-local-variable 'elscreen-display-tab) old-elscreen-display-tab)
                  (elscreen-notify-screen-modification 'force-immediately)
                  ;; (elscreen-run-screen-update-hook)
                  (testing (message "pause-hook: old-elscreen-display-tab %s" old-elscreen-display-tab)))
                (testing (message "no old cursor")))
            (if (boundp 'old-mode-line-format)
                (progn
                  (set (make-local-variable 'mode-line-format) old-mode-line-format)
                  (testing (message "pause-hook: old-mode-line-format %s" old-mode-line-format)))
                (testing (message "no old cursor")))
            (if (boundp 'old-cursor-type)
                (progn
                  (set (make-local-variable 'cursor-type) old-cursor-type)
                  (testing (message "pause-hook: old-cursor-type %s" old-cursor-type)))
                (testing (message "no old cursor")))
            (if (boundp 'old-centered-cursor-mode)
                (progn
                  (centered-cursor-mode (if (null old-centered-cursor-mode) -1 t))
                  (testing (message "pause-hook: old-centered-cursor-mode %s" old-centered-cursor-mode))
                  (ad-disable-advice 'ccm-first-start 'before 'reset-ccm-vpos)
                  (ad-activate #'ccm-first-start)
                  (ad-update #'ccm-first-start))
                (testing (message "no old centered")))
            (if (boundp 'old-hl-line-when-idle-p)
                (progn
                  (hl-line-toggle-when-idle (if (null old-hl-line-when-idle-p) -1 1))
                  (hl-line-toggle-when-idle (if (null gold-hl-line-when-idle-p) -1 1))
                  (testing
                   (message "pause-hook: old-hl-line-when-idle-p %s" old-hl-line-when-idle-p)))
                (testing (message "no old hl")))
            (if (boundp 'old-view-mode)
                (progn
                  (view-mode (if (null old-view-mode) -1 t))
                  (testing (message "pause-hook: old-view-mode %s" old-view-mode)))
                (testing (message "no old view")))

            (testing
             (message "old values:")
             (message "pause-hook: old-cursor-type %s" old-cursor-type)
             (message "pause-hook: old-hl-line-when-idle-p %s" old-hl-line-when-idle-p)
             (message "pause-hook: old-centered-cursor-mode %s" old-centered-cursor-mode)
             (message "pause-hook: old-view-mode %s" old-view-mode)

             (message "values:")

             (message "pause-hook: cursor-type %s" cursor-type)
             (message "pause-hook: hl-line-when-idle-p %s" hl-line-when-idle-p)
             (message "pause-hook: centered-cursor-mode %s" centered-cursor-mode)
             (message "pause-hook: view-mode %s" view-mode))
            (message nil)))

(add-hook 'reader-mode-resume-hook
          ;; start reader
          (lambda ()

            (testing
             (message "reader-mode-resume-hook")
             (reader-show-timers))
            (set (make-local-variable 'old-fullscreen) (frame-parameter nil 'fullscreen))
            (set-frame-parameter nil 'fullscreen (reader-mode-get-config 'fullscreen))
            (set (make-local-variable 'old-mode-line-format) mode-line-format)
            (set (make-local-variable 'mode-line-format)
                 (reader-mode-get-config 'mode-line-format))
            (set (make-local-variable 'old-elscreen-display-tab) elscreen-display-tab)
            (set (make-local-variable 'elscreen-display-tab)
                 (reader-mode-get-config 'elscreen-display-tab))
            (elscreen-notify-screen-modification 'force-immediately)
            ;; (elscreen-run-screen-update-hook)
            (set (make-local-variable 'old-cursor-type) cursor-type)
            (set (make-local-variable 'cursor-type)
                 (reader-mode-get-config 'cursor-type))
            (set (make-local-variable 'old-global-hl-line-mode) global-hl-line-mode)
            (global-hl-line-mode
             (reader-mode-get-config 'global-hl-line-mode))
            (set (make-local-variable 'old-hl-line-when-idle-p) hl-line-when-idle-p)
            (setq gold-hl-line-when-idle-p hl-line-when-idle-p)
            (hl-line-toggle-when-idle
             (reader-mode-get-config 'hl-line-toggle-when-idle))
            (set (make-local-variable 'old-centered-cursor-mode)
                 centered-cursor-mode)
            (centered-cursor-mode (reader-mode-get-config 'centered-cursor-mode))
            (ad-enable-advice 'ccm-first-start 'before 'reset-ccm-vpos)
            (ad-activate #'ccm-first-start)
            (ad-update #'ccm-first-start)

            (set (make-local-variable 'old-view-mode) view-mode)
            (view-mode  (reader-mode-get-config 'view-mode))

            (testing
             (message "old values:")
             (message "resume-hook: old-cursor-type %s" old-cursor-type)
             (message "resume-hook: old-global-hl-line-mode %s" old-global-hl-line-mode)
             (message "resume-hook: old-hl-line-when-idle-p %s" old-hl-line-when-idle-p)
             (message "resume-hook: old-centered-cursor-mode %s" old-centered-cursor-mode)
             (message "resume-hook: old-view-mode %s" old-view-mode)

             (message "values:")

             (message "resume-hook: cursor-type %s" cursor-type)
             (message "resume-hook: global-hl-line-mode %s" global-hl-line-mode)
             (message "resume-hook: hl-line-when-idle-p %s" hl-line-when-idle-p)
             (message "resume-hook: centered-cursor-mode %s" centered-cursor-mode)
             (message "resume-hook: view-mode %s" view-mode)
             )
            (message nil)))


(add-hook 'reader-mode-start-hook
          (lambda ()
            (progn
              (set (make-local-variable 'before-reader-mode-fullscreen) (frame-parameter nil 'fullscreen))
              (set (make-local-variable 'before-reader-mode-mode-line-format) mode-line-format)
              (set (make-local-variable 'before-reader-mode-elscreen-display-tab) elscreen-display-tab)
              (set (make-local-variable 'before-reader-mode-cursor-type) cursor-type)
              (set (make-local-variable 'before-reader-mode-global-hl-line-mode) global-hl-line-mode)
              (set (make-local-variable 'before-reader-mode-hl-line-when-idle-p) hl-line-when-idle-p)
              (setq gbefore-reader-mode-hl-line-when-idle-p hl-line-when-idle-p)
              (set (make-local-variable 'before-reader-mode-centered-cursor-mode) centered-cursor-mode)
              (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
                (setq ccm-vpos nil) t)
              (set (make-local-variable 'before-reader-mode-view-mode) view-mode))))



(add-hook 'reader-mode-end-hook
          (lambda ()
            (progn
              (set-frame-parameter nil 'fullscreen before-reader-mode-fullscreen)
              (set (make-local-variable 'mode-line-format) before-reader-mode-mode-line-format)
              (set (make-local-variable 'elscreen-display-tab) before-reader-mode-elscreen-display-tab)
              (set (make-local-variable 'cursor-type) before-reader-mode-cursor-type)
              (global-hl-line-mode (if (null before-reader-mode-global-hl-line-mode) -1 t))
              (hl-line-toggle-when-idle (if (null before-reader-mode-hl-line-when-idle-p) -1 t))
              (centered-cursor-mode (if (null before-reader-mode-centered-cursor-mode) -1 t))
              ;; Delete advise
              ;; (defadvice ccm-first-start (before reset-ccm-vpos (animate) activate)
              ;;   (setq ccm-vpos nil) t))
              (when (ad-find-advice 'ccm-first-start 'before 'reset-ccm-vpos)
                (ad-disable-advice 'ccm-first-start 'before 'reset-ccm-vpos)
                (ad-remove-advice 'ccm-first-start 'before 'reset-ccm-vpos)
                (ad-activate #'ccm-first-start))
              (ad-update #'ccm-first-start)
              (view-mode  (if (null before-reader-mode-view-mode) -1 t)))))


(define-minor-mode reader-mode
    "reader-mode"
  :initial-value nil
  :init-value nil
  :lighter (:eval
            (if (and (boundp 'reader-debug)
                     reader-debug)
                (concat
                 " rd["
                 (if reader-paused-manually "P")
                 (if (not reader-idle-timer) "i")
                 (if (not (and (boundp 'smooth-step-timer)
                               smooth-step-timer
                               (not (timer--triggered smooth-step-timer))))
                     "s")
                 "]")
                (concat
                 " rd"
                 (if reader-paused-manually "[P]"
                     (if (not reader-idle-timer) "[i]"
                         (if (not (and (boundp 'smooth-step-timer)
                                       smooth-step-timer
                                       (not (timer--triggered smooth-step-timer))))
                             "[s]"))))))
  :global nil
  (if (and arg
           (if (> (prefix-numeric-value arg) 0)
               reader-mode (not reader-mode)))
      (progn
        (run-hooks 'reader-mode-start-hook)

        (set (make-local-variable 'reader-idle-time) reader-idle-time)
        (set (make-local-variable 'reader-idle-repeat-time) reader-idle-repeat-time)
        (set (make-local-variable 'reader-cmd) reader-cmd)
        (set (make-local-variable 'reader-repeat) reader-repeat)
        (set (make-local-variable 'smooth-step-timer) nil)
        (set (make-local-variable 'reader-paused-manually) nil)
        (set (make-local-variable 'reader-debug) nil)
        (set (make-local-variable 'reader-no-smooth-step-timer) (if current-prefix-arg t nil))

        (set (make-local-variable 'reader-mode-buffer) (current-buffer))

        (set (make-local-variable 'reader-idle-timer)
             (run-with-idle-timer
              reader-idle-time
              reader-idle-repeat-time
              #'resume-smooth-read (current-buffer)))

        (add-hook 'kill-buffer-hook
                  #'(lambda ()
                      (when (and reader-mode
                                 (eq reader-mode-buffer (current-buffer))
                                 reader-idle-timer)
                        (cancel-timer reader-idle-timer)
                        (set (make-local-variable 'reader-idle-timer) nil)
                        (when (and
                               (boundp 'smooth-step-timer)
                               smooth-step-timer)
                          (cancel-timer smooth-step-timer)))) t t)
        (testing (message "hi reader mode %s" reader-idle-timer)))
      (progn
        (remove-hook 'pre-command-hook 'pause-smooth-read)
        (cancel-timer reader-idle-timer)
        (cancel-smooth-read)
        (set (make-local-variable 'reader-idle-timer) nil)
        (testing (message "by reader mode"))
        (run-hooks 'reader-mode-end-hook)))
  (message nil)
  t)

(defun reader-pause (&optional auto)
  (interactive)
  (when (and reader-mode
             (eq reader-mode-buffer (current-buffer))
             reader-idle-timer)
    (cancel-timer reader-idle-timer)
    (set (make-local-variable 'reader-idle-timer) nil)
    (when (and
           (boundp 'smooth-step-timer)
           smooth-step-timer)
      (timer-activate smooth-step-timer t))
    (testing (message "called reader-pause"))
    (set (make-local-variable 'reader-paused-manually) (called-interactively-p 'interactive))))

(defun reader-resume ()
  (interactive)
  (when (and reader-mode
             (if reader-paused-manually
                 (called-interactively-p 'interactive)
                 t)
             (eq reader-mode-buffer (current-buffer))
             (null reader-idle-timer))
    (set (make-local-variable 'reader-idle-timer)
             (run-with-idle-timer
              reader-idle-time
              reader-idle-repeat-time
              #'resume-smooth-read (current-buffer)))
    (testing (message "called reader-resume"))
    (set (make-local-variable 'reader-paused-manually) nil)))

(defun reader-show-timers ()
  (interactive)
  (message "%s" (current-buffer))
  (if reader-idle-timer
      (message "reader-idle-timer %s" reader-idle-timer)
      (message "no reader-idle-timer"))
  (if smooth-step-timer
      (message "smooth-step-timer %s" smooth-step-timer)
      (message "no smooth-step-timer")))

(defun smooth-read ()
  (when (and reader-mode
             (eq reader-mode-buffer (current-buffer))
             ;; (null reader-no-smooth-step-timer)
             )
    (set (make-local-variable 'smooth-step-timer)
         (run-with-timer 1 reader-repeat
                         (lambda (cmdx cbuf)
                           (when (and reader-idle-timer
                                      smooth-step-timer
                                      (eq cbuf (current-buffer)))
                             (call-interactively cmdx)
                             (run-hooks 'post-command-hook)))
                         reader-cmd (current-buffer)))
    (run-hooks 'reader-mode-smooth-read-start-hook)))

(defun pause-smooth-read ()
  (interactive)
  (when (and
         reader-mode
         (eq reader-mode-buffer (current-buffer)))
    (if (and
         (boundp 'smooth-step-timer)
         smooth-step-timer)
        (timer-activate smooth-step-timer t))
    (if (eq reader-mode-buffer (current-buffer))
        (run-hooks 'reader-mode-pause-hook))
    (testing
     (message "pause-smooth-read: removing pause-smooth-read from pre-command-hook")
     (unless (member #'pause-smooth-read pre-command-hook)
       (message "error: pause-smooth-read not in pre-command-hook(%s)" pre-command-hook)))
    (remove-hook 'pre-command-hook 'pause-smooth-read t)
    ;; (set (make-local-variable 'reader-mode-smooth-step-active) nil)
    ))

(defun resume-smooth-read (cbuf)
  (when (and
         (eq reader-mode-buffer (current-buffer))
         (eq cbuf (current-buffer)))
    (unless reader-no-smooth-step-timer
      (if (and
           (boundp 'smooth-step-timer)
           smooth-step-timer)
          (timer-activate smooth-step-timer)
          (if reader-mode (smooth-read))))

    (when (and
           reader-mode
           ;; (boundp 'smooth-step-timer) smooth-step-timer
           )
      (if (eq reader-mode-buffer (current-buffer))
          (run-hooks 'reader-mode-resume-hook))
      (if (add-hook 'pre-command-hook 'pause-smooth-read t t)
          (testing
           (message "resume-smooth-read: added pause-smooth-read to pre-command-hook(%s)" pre-command-hook))
          (testing
           (message "failed"))))))

(defun cancel-smooth-read ()
  (interactive)
  (when smooth-step-timer
    (cancel-timer smooth-step-timer)
    ;; (set (make-local-variable 'smooth-read-active) nil)
    (set (make-local-variable 'smooth-step-timer) nil)
    (run-hooks 'reader-mode-smooth-read-end-hook)
    ;; (set (make-local-variable 'reader-mode-smooth-step-active) nil)
    )
  (remove-hook 'pre-command-hook 'pause-smooth-read t))




;; (funcall #'call-at-steps :micros 800 :fn #'forward-sentence)



 (add-hook 'on-focus-out-hook
           #'(lambda ()
               (testing
                (message "focus OUT cursor %s , buffer %s" cursor-type (current-buffer)))
               (when reader-mode
                 (when (member 'pause-smooth-read pre-command-hook)
                   (pause-smooth-read)
                   (testing
                    (message "running pause")))
                 (reader-pause))))


 (add-hook 'on-focus-in-hook
           #'(lambda ()
               (testing
                (message "focus IN cursor %s , buffer %s" cursor-type (current-buffer)))
               (when reader-mode
                 (reader-resume))))

;;}}

;;{{ http://stackoverflow.com/questions/1230245/how-to-automatically-save-files-on-lose-focus-in-emacs/13917428#13917428
(require 'macros)
(deh-section "Focus"
  (message "deh-sec loaded")
 ;; when (and
 ;;       (featurep 'x)
 ;;       window-system)
 (defvar on-blur--timer nil "Timer refreshing known focused window.")
 (defvar on-focus-out-hook nil "on-focus-out-hook")
 (defvar on-focus-in-hook nil "on-focus-in-hook")

 (setq old-active-window-id 0)

 (defun on-blur--refresh ()
   "Runs on-blur-hook if emacs has lost focus."
   (if (and
        (featurep 'x)
        window-system)
       (let* ((active-window (x-window-property
                              "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
              (active-window-id (if (numberp active-window)
                                    active-window
                                    (string-to-number
                                     (format ; "%x%x"
                                      "%x00%x"
                                      (car active-window)
                                      (cdr active-window)) 16)))
              (emacs-window-id (string-to-number
                                (frame-parameter nil 'outer-window-id))))

         (when (not (= active-window-id old-active-window-id))
           (if (= emacs-window-id active-window-id)
               (run-hooks 'on-focus-in-hook)
               (run-hooks 'on-focus-out-hook))
           (setq old-active-window-id active-window-id))
         ;; (when on-blur--timer
         ;;     (cancel-timer on-blur--timer))
         ;; (setq on-blur--timer
         ;;  (run-with-timer 1 nil 'on-blur--refresh))
         )
       (message "on-blur--refresh: Not in Graphical Window system.")))

 (defun run-on-blur-timer ()
   (interactive)
   (setq on-blur--timer
         (run-with-timer 1 1 'on-blur--refresh)))

 (defun cancel-on-blur-timer ()
   (interactive)
   (if on-blur--timer
       (cancel-timer on-blur--timer)))


 (testing
  (if (and (boundp 'on-blur--timer)
           on-blur--timer)
      (cancel-timer on-blur--timer)))

 (add-hook 'lotus-enable-login-session-interrupting-feature-hook
           '(lambda ()
             (if (and (featurep 'x) window-system)
                 (unless (and (boundp 'on-blur--timer)
                              on-blur--timer)
                   (run-on-blur-timer))
                 (message "Not in Graphical Window system."))))

 (add-hook 'lotus-disable-login-session-interrupting-feature-hook
           '(lambda ()
             (when (and
                    (featurep 'x)
                    window-system)
               (when (and (boundp 'on-blur--timer)
                          on-blur--timer)
                 (cancel-on-blur-timer))
               (message "Not in Graphical Window system."))))

)

;;}}




(define-minor-mode reader-light-mode
    "Prepare for working with collarative office project."
  :init-value nil
  :lighter " rl"
  :global nil
  (if reader-light-mode
      (progn
        (run-hooks 'reader-mode-start-hook)
        (set (make-local-variable 'mode-line-format)
                   (reader-mode-get-config 'mode-line-format))
        (set (make-local-variable 'elscreen-display-tab)
                   (reader-mode-get-config 'elscreen-display-tab))
        (elscreen-notify-screen-modification 'force-immediately)
        (set (make-local-variable 'cursor-type)
                   (reader-mode-get-config 'cursor-type))
        (global-hl-line-mode (reader-mode-get-config 'global-hl-line-mode))
        (hl-line-toggle-when-idle (reader-mode-get-config 'hl-line-toggle-when-idle))
        (centered-cursor-mode (reader-mode-get-config 'centered-cursor-mode))
        (view-mode  (reader-mode-get-config 'view-mode)))
      (run-hooks 'reader-mode-end-hook)))


(provide 'reader-mode)
;;; reader-mode.el ends here

;; (testing
;;  (run-with-timer 10 nil
;;                  #'(lambda ()
;;                      (let* ((active-window (x-window-property
;;                                             "_NET_ACTIVE_WINDOW" nil "WINDOW" 0 nil t))
;;                             (active-window-id (if (numberp active-window)
;;                                                   active-window
;;                                                   (string-to-number
;;                                                    (format "%x%x"
;;                                                            (car active-window)
;;                                                            (cdr active-window)) 16)))
;;                             (emacs-window-id (string-to-number
;;                                               (frame-parameter nil 'outer-window-id))))
;;                        (message "emacs id %x aw id %x" emacs-window-id active-window-id))))


;;  (setq t1 (run-with-timer 10 10 #'message "abcd"))


;;  (progn
;;    (timer-activate t1 )
;;    (message "Activate"))


;;  (progn
;;    (timer-activate t1 t)
;;    (message "Deactivate"))


;;  (message "%s"
;;           (timer--triggered t1))



;;  (run-with-timer 20 nil #'(lambda ()
;;                             (message "pre-command-hook: %s" pre-command-hook)
;;                             (if (member #'pause-smooth-read pre-command-hook)
;;                                 (message "yes")
;;                                 (message "no")))))
