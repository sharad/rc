;;; lotus-misc-utils.el --- copy config  -*- lexical-binding: t; -*-

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

;;

;;; Code:

(provide 'lotus-misc-utils)


(require 'timer)
(require 'timer-utils-lotus)
;; timer


(defmacro eval-if-focus (focus-body unfocus-body)
  "Eval FOCUS-BODY if focus, else eval UNFOCUS-BODY"
  `(if (has-focus)
       ,focus-body
     ,unfocus-body))
(put 'eval-if-focus 'lisp-indent-function 1)

(defmacro eval-with-focus (&rest body)
  "Eval BODY with focus"
  `(progn
     (unless (has-focus)
       (grab-focus))
     ,@body))
(put 'eval-with-focus 'lisp-indent-function 0)

(defmacro with-no-active-minibuffer (&rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (when (active-minibuffer-window)
       (abort-recursive-edit))))
(put 'with-no-active-minibuffer 'lisp-indent-function 0)

(defmacro with-no-active-minibuffer-ensured (&rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(with-no-active-minibuffer
    (unless (active-minibuffer-window)
      ,@body)))
(put 'with-no-active-minibuffer-ensured 'lisp-indent-function 0)

(defmacro with-no-active-minibuffer-debug (minibuffer-body &rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(progn
     (when (active-minibuffer-window)
       ,minibuffer-body)
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    ,@body))
     (when (active-minibuffer-window)
       (abort-recursive-edit))))
(put 'with-no-active-minibuffer-debug 'lisp-indent-function 1)

(defmacro with-no-active-minibuffer-ensured-debug (minibuffer-body &rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(with-no-active-minibuffer-debug
    ,minibuffer-body
    (unless (active-minibuffer-window)
      ,@body)))
(put 'with-no-active-minibuffer-ensured-debug 'lisp-indent-function 1)

(when nil
  (defun test-minibuffer-quiting ()
    (with-no-active-minibuffer
      (message "Hello")))

  (run-with-timer 3 nil 'test-minibuffer-quiting)

  (get 'quit 'error-message))

(defun lotus-new-lower-win-size ()
  ;; TODO: improve it.
  ;; If the mode line might interfere with the calculator
  ;; buffer, use 3 lines instead.
  (if (and
       (fboundp 'face-attr-construct)
       (let* ((dh (plist-get (face-attr-construct 'default) :height))
              (mf (face-attr-construct 'mode-line))
              (mh (plist-get mf :height)))
         ;; If the mode line is shorter than the default,
         ;; stick with 2 lines.  (It may be necessary to
         ;; check how much shorter.)
         (and
          (not
           (or (and (integerp dh)
                    (integerp mh)
                    (< mh dh))
               (and (numberp mh)
                    (not (integerp mh))
                    (< mh 1))))
          (or
           ;; If the mode line is taller than the default,
           ;; use 3 lines.
           (and (integerp dh)
                (integerp mh)
                (> mh dh))
           (and (numberp mh)
                (not (integerp mh))
                (> mh 1))
           ;; If the mode line has a box with non-negative line-width,
           ;; use 3 lines.
           (let* ((bx (plist-get mf :box))
                  (lh (plist-get bx :line-width)))
             (and bx
                  (or
                   (not lh)
                   (> lh 0))))
           ;; If the mode line has an overline, use 3 lines.
           (plist-get (face-attr-construct 'mode-line) :overline)))))
      -12 -15))

;; create smaller and proper sized window
;; TODO: org-fit-window-to-buffer
(defun lotus-make-new-win ()
  (let ((size (lotus-new-lower-win-size))
        (window-min-height 7))
    (prog1
        (split-window-below size)
      (lwarn 'lotus-new-win :debug "newwin size %d" size)
      (message "size %d" size))))


(defmacro lotus-with-new-win (newwin &rest body)
  `(lexical-let* ((,newwin (lotus-make-new-win)))
       ;; maybe leave two lines for our window because of the
       ;; normal `raised' mode line
       (select-window ,newwin 'norecord)
       (progn
         ,@body)))
(put 'lotus-with-new-win 'lisp-indent-function 1)

(defmacro lotus-with-timed-new-win (timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  (lexical-let ((temp-win-config (make-symbol "test-lotus-with-timed-new-win-config")))
    `(lexical-let* ((,temp-win-config (current-window-configuration))
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
         (lexical-let* ((,timer (run-with-idle-plus-timer ,timeout
                                                          nil
                                                          ,cleanupfn-newwin
                                                          ,newwin
                                                          ,cleanupfn-local)))
           (condition-case err
               (progn
                 ,@body)
             ((quit)
              (funcall ,cleanupfn-newwin ,newwin ,cleanupfn-local))))))))
(put 'lotus-with-timed-new-win 'lisp-indent-function 1)

;; Marker Macros Starts
(defmacro lotus-with-marker-alt (marker &rest body)
  `(let ((buffer (marker-buffer ,marker)))
     (save-excursion ; Do not replace this with `with-current-buffer'.
       (with-no-warnings (set-buffer buffer))
       (save-restriction
         (widen)
         (goto-char ,marker)
         (progn
           ,@body)))))
;; Marker Macros Ends

(defmacro lotus-with-marker (marker &rest body)
  `(when (marker-buffer ,marker)
     (let ((target-buffer (marker-buffer   ,marker))
           (pos           (marker-position ,marker)))
       (if target-buffer
           (with-current-buffer target-buffer
             (message "lotus-with-file-pos-new-win: selecting buf %s" target-buffer)
             (if (<= pos (point-max))
                 (progn
                   (goto-char pos)
                   ,@body)
                 (error "position %d greater than point max %d" pos (point-max))))
           (error "No buffer")))))
(put 'lotus-with-marker 'lisp-indent-function 1)

(defmacro lotus-with-pos (pos &rest body)
  `(progn
     (message "lotus-with-buffer-pos-new-win: selecting buf %s" (current-buffer))
     (if (<= ,pos (point-max))
         (progn
           (goto-char ,pos)
           ,@body)
         (error "position %d greater than point max %d" ,pos (point-max)))))
(put 'lotus-with-pos 'lisp-indent-function 1)

;; (defmacro lotus-with-buffer-pos (buffer pos &rest body)
;;   `(let ((target-buffer (if ,buffer ,buffer (current-buffer))))
;;      (if target-buffer
;;          (with-current-buffer target-buffer
;;            (lotus-with-pos ,@body))
;;          (error "No buffer"))))
;; (put 'lotus-with-buffer-pos 'lisp-indent-function 1)

(defmacro lotus-with-file-pos (file pos &rest body)
  `(let ((buff (find-file-noselect ,file)))
     (if buff
         (with-current-buffer buff
           (lotus-with-pos ,pos ,@body))
         (error "can not open file %f" ,file))))
(put 'lotus-with-file-pos 'lisp-indent-function 1)


(defmacro lotus-with-marker-new-win (marker newwin &rest body)
  `(when (marker-buffer ,marker)
     (let ((target-buffer (marker-buffer   ,marker))
           (pos           (marker-position ,marker)))
       (if target-buffer
           (lotus-with-new-win ,newwin
             (message "lotus-with-file-pos-new-win: selecting buf %s in %s win" target-buffer ,newwin)
             ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
             (switch-to-buffer target-buffer)
             (if (<= pos (point-max))
                 (progn
                   (goto-char pos)
                   ,@body)
                 (error "position %d greater than point max %d" pos (point-max))))
           (error "No buffer")))))
(put 'org-with-marker-new-win 'lisp-indent-function 1)

(defmacro lotus-with-pos-new-win (pos newwin &rest body)
  `(let ((target-buffer (current-buffer)))
     (if target-buffer
         (lotus-with-new-win ,newwin
           (message "lotus-with-file-pos-new-win: selecting buf %s in %s win" target-buffer ,newwin)
           ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
           (switch-to-buffer target-buffer)
           (if (<= ,pos (point-max))
               (progn
                 (goto-char ,pos)
                 ,@body)
               (error "position %d greater than point max %d" ,pos (point-max))))
         (error "No buffer"))))
(put 'lotus-with-pos-new-win 'lisp-indent-function 1)

(defmacro lotus-with-buffer-pos-new-win (buffer pos newwin &rest body)
  `(let ((target-buffer (if ,buffer ,buffer (current-buffer))))
     (if target-buffer
         (lotus-with-new-win ,newwin
           (message "lotus-with-file-pos-new-win: selecting buf %s in %s win" target-buffer ,newwin)
           ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
           (switch-to-buffer target-buffer)
           (if (<= ,pos (point-max))
               (progn
                 (goto-char ,pos)
                 ,@body)
               (error "position %d greater than point max %d" ,pos (point-max))))
         (error "No buffer"))))
(put 'lotus-with-buffer-pos-new-win 'lisp-indent-function 1)

(defmacro lotus-with-file-pos-new-win (file pos newwin &rest body)
  `(let ((buff (find-file-noselect ,file)))
     (if buff
         (with-current-buffer buff
           (lotus-with-pos-new-win
            buff ,pos
            ,@body))
         (error "can not open file %f" ,file))))
(put 'lotus-with-file-pos-new-win 'lisp-indent-function 1)

;; (query-replace-regexp "org-with-marker-timed-new-win" "lotus-with-marker-timed-new-win" t nil nil nil)
;;
;; (query-replace "org-with-marker-timed-new-win" "lotus-with-marker-timed-new-win" t nil nil nil)

;; TODO: newwin clean should be done here
(defmacro lotus-with-marker-timed-new-win (marker timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(when (marker-buffer ,marker)
     (let ((target-buffer (marker-buffer   ,marker))
           (pos           (marker-position ,marker)))
       (lotus-with-timed-new-win
           ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
           (message "lotus-with-marker-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
           ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
           (switch-to-buffer target-buffer)
           (if (<= pos (point-max))
               (progn
                 (goto-char pos)
                 ,@body)
               (error "position %d greater than point max %d" pos (point-max)))))))
(put 'lotus-with-marker-timed-new-win 'lisp-indent-function 1)

(defmacro lotus-with-pos-timed-new-win (pos timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(let ((target-buffer (current-buffer)))
     (lotus-with-timed-new-win
         ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
         (message "lotus-with-buffer-pos-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
         ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
         (switch-to-buffer target-buffer)
         (if (<= ,pos (point-max))
             (progn
               (goto-char ,pos)
               ,@body)
             (error "position %d greater than point max %d" ,pos (point-max))))))
(put 'lotus-with-pos-timed-new-win 'lisp-indent-function 1)

(defmacro lotus-with-buffer-pos-timed-new-win (buffer pos timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(let ((target-buffer (if ,buffer ,buffer (current-buffer))))
     (if target-buffer
         (lotus-with-timed-new-win
             ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
             (message "lotus-with-buffer-pos-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
             ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
             (switch-to-buffer target-buffer)
             (if (<= ,pos (point-max))
                 (progn
                   (goto-char ,pos)
                   ,@body)
                 (error "position %d greater than point max %d" ,pos (point-max))))
         (error "No buffer"))))
(put 'lotus-with-buffer-pos-timed-new-win 'lisp-indent-function 1)

(defmacro lotus-with-file-pos-timed-new-win (file pos timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(let ((target-buffer (find-file-noselect ,file)))
     (if target-buffer
         (with-current-buffer target-buffer
           (lotus-with-pos-timed-new-win
            ,pos ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
            ,@body))
         (error "can not open file %f" ,file))))
(put 'lotus-with-file-pos-timed-new-win 'lisp-indent-function 1)
;; move out
;; Misc Macros Ends

(defmacro lotus-with-no-active-minibuffer (&rest body)
  ;;could schedule in little further.
  `(unless (active-minibuffer-window)
     (progn
       ,@body)))
(put 'lotus-with-no-active-minibuffer 'lisp-indent-function 0)

(defmacro lotus-with-no-active-minibuffer-if (minibuffer-body &rest body)
  ;;could schedule in little further.
  `(if (active-minibuffer-window)
       (progn
         ,minibuffer-body
         (lwarn 'active-minibuffer-if :debug "%s: %s: cancelled as active minibuffer found." (time-stamp-string) 'lotus-with-no-active-minibuffer-if))
     (progn
       (lwarn 'active-minibuffer-if :debug "%s: %s: no active minibuffer found." (time-stamp-string) 'lotus-with-no-active-minibuffer-if)
       ,@body)))
(put 'lotus-with-no-active-minibuffer-if 'lisp-indent-function 1)

(defmacro lotus-with-override-minibuffer (&rest body)
  `(with-no-active-minibuffer-ensured
    ,@body))
(put 'lotus-with-override-minibuffer 'lisp-indent-function 0)

(defmacro lotus-with-override-minibuffer-if (minibuffer-body &rest body)
  `(with-no-active-minibuffer-ensured-debug
    (progn
      (lwarn 'active-minibuffer-if :debug "%s: %s: aborting active minibuffer." (time-stamp-string) 'lotus-with-override-minibuffer-if)
      ,minibuffer-body)
    ,@body))
(put 'lotus-with-override-minibuffer-if 'lisp-indent-function 1)

(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus ,action))))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (when (not (advice-function-member-p #'quiet--select-frame (symbol-function 'select-frame-set-input-focus ,action)))
                         (lwarn 'event-input :debug "%s: %s: readfn: <%s> add quiet 5 as already was present" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                         (add-function :override (symbol-function  'select-frame-set-input-focus ,action) #'quiet--select-frame))
                     (when (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus ,action))
                       (lwarn 'event-input :debug "%s: %s: readfn: <%s> remove quiet 5 as already was present" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                       (remove-function (symbol-function 'select-frame-set-input-focus ,action) #'quiet--select-frame)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (lwarn 'event-input :debug "%s: %s: readfn: <%s> frame %s" (time-stamp-string) 'lotus-with-other-frame-event ,action frame)
                     (lwarn 'event-input :debug "%s: %s: readfn: <%s> 1 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                     (add-hook
                      'pre-command-hook
                      (lambda ()
                        (funcall hookfn)))
                     (lwarn 'event-input :debug "%s: %s: readfn: <%s> 2 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                     ;; (unless sel-frame-adviced-p
                     ;;   (remove-function (symbol-function 'select-frame-set-input-focus ,action) #'quiet--select-frame)
                     ;;   (lwarn 'event-input :debug "readfn: removed quiet-sel-frame"))
                     (condition-case nil
                         (progn
                           (funcall set-advice-fn)
                           (lwarn 'event-input :debug "%s: %s: readfn: <%s> 3 running orginal code minibuffer<%s> " (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           ,@body
                           (lwarn 'event-input :debug "%s: %s: readfn: <%s> 4 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                           (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                           (funcall set-advice-fn))
                       (quit
                        (lwarn 'event-input :debug "%: %s: <%s> quit" (time-stamp-string) 'lotus-with-other-frame-event ,action))))))

              (hookfn1
               #'(lambda ()
                   (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> last-input-event: %s last-event-frame: %s frame: %s"
                          (time-stamp-string)
                          'lotus-with-other-frame-event ,action
                          last-input-event
                          last-event-frame
                          frame)
                   (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> removing hook 1" (time-stamp-string) 'lotus-with-other-frame-event ,action )
                   (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> 1 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn1)))
                   (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> 2 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> removing hook 2" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                         (remove-hook 'pre-command-hook
                                      (lambda ()
                                        (funcall hookfn1))))
                     (progn
                       (setq frame nil)
                       (with-selected-frame last-event-frame
                         (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> with-selected-frame running timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (run-with-timer 0 nil #'(lambda () (funcall readfn)))
                         (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> adding quiet-sel-frame minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (add-function :override (symbol-function  'select-frame-set-input-focus ,action) #'quiet--select-frame)
                         (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> going to run abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (when (active-minibuffer-window)
                           (lwarn 'event-input :debug "%s: %s: hookfn1: <%s> running abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (abort-recursive-edit)))))))

              (hookfn
               #'(lambda ()
                   (lwarn 'event-input :debug "%s: %s: hookfn: <%s> last-input-event: %s last-event-frame: %s frame: %s minibuffer<%s>"
                          (time-stamp-string)
                          'lotus-with-other-frame-event ,action
                          last-input-event
                          last-event-frame
                          frame
                          (active-minibuffer-window))
                   (lwarn 'event-input :debug "%s: %s: hookfn: <%s> removing hook 1 minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                   (lwarn 'event-input :debug "%s: %s: hookfn: <%s> 1 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (lwarn 'event-input :debug "%s: %s: hookfn: <%s> 2 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (remove-function (symbol-function 'select-frame-set-input-focus ,action) #'quiet--select-frame)
                         ;; (lwarn 'event-input :debug "hookfn: removing hook 2")
                         ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                         t)
                     (with-selected-frame last-event-frame
                       (progn
                         (setq frame nil)
                         (run-with-timer 0 nil
                                         #'(lambda ()
                                             (progn
                                               ;; (setq frame (selected-frame))
                                               ;; (setq debug-on-quit nil)
                                               (lwarn 'event-input :debug "%s: %s: hookfn: <%s> with-selected-frame running timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                                               ;; (funcall set-advice-fn)
                                               ,@(cond
                                                  ((or
                                                    (eq :restart action)
                                                    (eq t action))
                                                   `(
                                                     (with-selected-frame last-event-frame
                                                       (funcall readfn))))
                                                  ((consp action)
                                                   `(
                                                     (progn
                                                       ,action)))
                                                  ((or
                                                    (eq :cancel action)
                                                    (null action))
                                                   nil)))))
                         (progn
                           (lwarn 'event-input :debug "%s: %s: hookfn: <%s> adding quiet-sel-frame minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)
                           (lwarn 'event-input :debug "%s: %s: hookfn: <%s> going to run abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (when (active-minibuffer-window)
                             (lwarn 'event-input :debug "%s: %s: hookfn: <%s> running abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                             (abort-recursive-edit)))))))))
       (lwarn 'event-input :debug "%s: %s: <%s> calling readfn minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
       (funcall readfn))))

(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus))))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (when (not (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus)))
                         (lwarn 'event-input :debug "%s: %s: readfn: <%s> add quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
                     (when (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus))
                       (lwarn 'event-input :debug "%s: %s: readfn: <%s> remove quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                       (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                     ;; (unless sel-frame-adviced-p
                     ;;   (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame))
                     (condition-case nil
                         (progn
                           (funcall set-advice-fn)
                           ,@body
                           (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                           (funcall set-advice-fn))
                       (quit nil)))))
              (hookfn
               #'(lambda ()
                   ;; (lwarn 'event-input :debug "hookfn: last-input-event: %s last-event-frame: %s frame: %s"
                   ;;          last-input-event
                   ;;          last-event-frame
                   ;;          frame)
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                         ;; (lwarn 'event-input :debug "hookfn: removing hook 2")
                         ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                         t)
                     (with-selected-frame last-event-frame
                       (progn
                         (setq frame nil)
                         (run-with-timer 0 nil
                                         #'(lambda ()
                                             (progn
                                               ;; (setq frame (selected-frame))
                                               ;; (setq debug-on-quit nil)
                                               ;; (funcall set-advice-fn)
                                               ,@(cond
                                                  ((or
                                                    (eq :restart action)
                                                    (eq t action))
                                                   `(
                                                     (with-selected-frame last-event-frame
                                                       (funcall readfn))))
                                                  ((consp action)
                                                   `(
                                                     (progn
                                                       ,action)))
                                                  ((or
                                                    (eq :cancel action)
                                                    (null action))
                                                   nil)))))
                         (progn
                           (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)
                           (when (active-minibuffer-window)
                             (abort-recursive-edit)))))))))
       (funcall readfn))))
(put 'lotus-with-other-frame-event 'lisp-indent-function 1)


(defmacro lotus-with-other-frame-event-debug (name action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus))))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (when (not (advice-function-member-p #'quiet--select-frame (symbol-function 'select-frame-set-input-focus)))
                         (lwarn 'event-input :debug "%s: %s: set-advice-fn: %s <%s> add quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                         (lwarn 'event-input :debug "%s: %s: set-advice-fn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                (time-stamp-string) 'lotus-with-other-frame-event-debug
                                ,name ,action
                                last-input-event
                                last-event-frame
                                frame
                                (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                         (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame))
                     (when (advice-function-member-p #'quiet--select-frame (symbol-function 'select-frame-set-input-focus))
                       (lwarn 'event-input :debug "%s: %s: readfn: %s <%s> remove quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                       (lwarn 'event-input :debug "%s: %s: set-advice-fn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                              (time-stamp-string) 'lotus-with-other-frame-event-debug
                              ,name ,action
                              last-input-event
                              last-event-frame
                              frame
                              (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                       (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                     (condition-case nil
                         (progn
                           (lwarn 'event-input :debug "%s: %s: readfn: %s <%s> inside readfn minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                           (lwarn 'event-input :debug "%s: %s: readfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                  (time-stamp-string) 'lotus-with-other-frame-event-debug
                                  ,name ,action
                                  last-input-event
                                  last-event-frame
                                  frame
                                  (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                           (funcall set-advice-fn)
                           ,@body
                           (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                           (funcall set-advice-fn))
                       (quit nil)))))
              (hookfn
               #'(lambda ()
                   (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                          (time-stamp-string) 'lotus-with-other-frame-event-debug
                          ,name ,action
                          last-input-event
                          last-event-frame
                          frame
                          (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                         ;; (lwarn 'event-input :debug "hookfn: removing hook 2")
                         ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                         t)
                     (with-selected-frame last-event-frame
                       (progn
                         (lwarn 'event-input :debug "%s: %s: hookfn: %s <%s> running readfn from hookfn outside timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                         (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                (time-stamp-string) 'lotus-with-other-frame-event-debug
                                ,name ,action
                                last-input-event
                                last-event-frame
                                frame
                                (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                         (setq frame nil)
                         (run-with-timer 0 nil
                                         #'(lambda ()
                                             (progn
                                               ;; (setq frame (selected-frame))
                                               ;; (setq debug-on-quit nil)
                                               (lwarn 'event-input :debug "%s: %s: hookfn: %s <%s> timer remove quiet 1 minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                                               (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                                      (time-stamp-string) 'lotus-with-other-frame-event-debug
                                                      ,name ,action
                                                      last-input-event
                                                      last-event-frame
                                                      frame
                                                      (selected-frame)
                                                      (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                                               ;; (funcall set-advice-fn)
                                               (prog1
                                                   ,@(cond
                                                      ((or
                                                        (eq :restart action)
                                                        (eq t action))
                                                       `(
                                                         (with-selected-frame last-event-frame
                                                           (lwarn 'event-input :debug "%s: %s: hookfn: %s <%s> running readfn from hookfn inside timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                                                           (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                                                  (time-stamp-string) 'lotus-with-other-frame-event-debug
                                                                  ,name ,action
                                                                  last-input-event
                                                                  last-event-frame
                                                                  frame
                                                                  (selected-frame)
                                                                  (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                                                           (funcall readfn))))
                                                      ((consp action)
                                                       `(
                                                         (progn
                                                           ,action)))
                                                      ((or
                                                        (eq :cancel action)
                                                        (null action))
                                                       nil))
                                                 (lwarn 'event-input :debug
                                                        "%s: %s: hookfn: %s <%s> finished running %s frame=%s minibuffer<%s>"
                                                        (time-stamp-string)
                                                        'lotus-with-other-frame-event-debug
                                                        ,name
                                                        ,action
                                                        ,action
                                                        (selected-frame)
                                                        (active-minibuffer-window))
                                                 (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                                        (time-stamp-string) 'lotus-with-other-frame-event-debug
                                                        ,name ,action
                                                        last-input-event
                                                        last-event-frame
                                                        frame
                                                        (selected-frame)
                                                        (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))))))
                         (progn
                           (lwarn 'event-input :debug "%s: %s: hookfn: %s <%s> add quiet 2 frame=%s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (selected-frame) (active-minibuffer-window))
                           (lwarn 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                  (time-stamp-string) 'lotus-with-other-frame-event-debug
                                  ,name ,action
                                  last-input-event
                                  last-event-frame
                                  frame
                                  (selected-frame)
                                  (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                           (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)
                           (when (active-minibuffer-window)
                             (abort-recursive-edit)))))))))
       (funcall readfn))))
(put 'lotus-with-other-frame-event-debug 'lisp-indent-function 2)

(defmacro lotus-restart-with-other-frame-event (&rest body)
  `(lotus-with-other-frame-event :restart ,@body))
(put 'lotus-restart-with-other-frame-event 'lisp-indent-function 0)

(defmacro lotus-cancel-with-other-frame-event (&rest body)
  `(lotus-with-other-frame-event :cancel ,@body))
(put 'lotus-cancel-with-other-frame-event 'lisp-indent-function 0)

(defmacro lotus-run-with-other-frame-event (action &rest body)
  `(lotus-with-other-frame-event ,action ,@body))
(put 'lotus-cancel-with-other-frame-event 'lisp-indent-function 1)


;; *Messages*

;; (No changes need to be saved)
;; Saving file /home/s/hell/.emacs.d/autoconfig/gnus/newsrc.eld...
;; Wrote /home/s/hell/.emacs.d/autoconfig/gnus/newsrc.eld
;; next-line: End of buffer [3 times]
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; readfn: occ-add-to-org-heading-when-idle inside readfn
;; Getting targets...done
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; triggered timer for new-win #<window 1073 on *helm-mode-occ-clock-in-curr-ctx-if-not*>
;; readfn: occ-clock-in-curr-ctx-if-not add quiet 5 as already was present
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn [15 times]
;; readfn: org-rl-resolve-clocks-if-idle inside readfn
;; org-clock-last-user-idle-seconds: nil
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org [3 times]
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn [13 times]
;; gnus-demon-scan-mail-and-news-now: current time Wed 17:05:00, idle time 600
;; gnus-demon-scan-mail-and-news-now 3
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; Saving file /home/s/hell/.emacs.d/.cache/autoconfig/recentf/recentf...
;; Wrote /home/s/hell/.emacs.d/.cache/autoconfig/recentf/recentf
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn [4 times]
;; gnus demon timeout
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn [45 times]
;; hookfn: org-rl-resolve-clocks-if-idle running readfn from hookfn outside timer
;; hookfn: org-rl-resolve-clocks-if-idle add quiet 2
;; hookfn: org-rl-resolve-clocks-if-idle timer remove quiet 1
;; hookfn: org-rl-resolve-clocks-if-idle running readfn from hookfn inside timer
;; readfn: org-rl-resolve-clocks-if-idle inside readfn
;; org-clock-last-user-idle-seconds: 310.001730628
;; lotus-with-file-pos-new-win: selecting buf Unnamed.org [3 times]
;; hookfn: occ-add-to-org-heading-when-idle running readfn from hookfn outside timer
;; hookfn: occ-add-to-org-heading-when-idle add quiet 2
;; hookfn: org-rl-resolve-clocks-if-idle finished running :restart
;; hookfn: occ-add-to-org-heading-when-idle timer remove quiet 1
;; hookfn: occ-add-to-org-heading-when-idle finished running :cancel
;; ElScreen version 20180321
;; Mark set
;; Mark saved where search started
;; Mark set [2 times]



(defmacro run-when-idle (secs &rest body)
  `(letrec ((timer nil)
            (fn
             (lambda ()
               (let ((retval
                      (while-no-input (redisplay)
                                      ,@body
                                      :complete)))
                 ))))
     (setq
      timer
      (run-with-idle-timer secs nil
                           ))
     `(while-no-input (redisplay)
                      )))



(defmacro run-unobtrusively (&rest body)
  `(let ((retval (while-no-input
                   (redisplay)
                   ,@body)))
     (when (eq retval t)
       (message "user input %s retval %s" last-input-event retval))
     retval))
(put 'run-unobtrusively 'lisp-indent-function 0)

(defmacro run-unobtrusively-throw-on-input (&rest body) ;throw
  `(while-no-input
     (redisplay)
     ,@body))
(put 'run-unobtrusively-throw-on-input 'lisp-indent-function 0)

;; TODO complete it using letrec
(defmacro run-unobtrusively-complete-when-idle (idletime &rest body) ;throw
  `(let ((retval
          (while-no-input
            (redisplay)
            ,@body)))
     retval))
(put 'run-unobtrusively-complete-when-idle 'lisp-indent-function 0)


;; https://stackoverflow.com/questions/3811448/can-call-with-current-continuation-be-implemented-only-with-lambdas-and-closures
;; CALL/CC

(defun has-focus-p ()
  )

(defmacro run-with-idle-timer-and-focus (sec repeat fn arg)
  ;; todo: how to cancel the timer later
  (let ((timer nil)
        (frame (selected-frame)))
    (letrec ((focusfn
              (lambda ()
                (if (and
                     (has-focus-p)
                     (eq frame (selected-frame))
                     (frame-visible-p (selected-frame)))
                    (funcall fn arg)
                  (progn
                    (when timer
                      (cancel-timer timer)
                      (setq timer nil))
                    (setq timer
                          (run-with-idle-timer (+ sec sec) repeat focusfn)))))))
      (setq timer
            (run-with-idle-timer sec repeat focusfn)))))

(when nil

  (lotus-with-other-frame-event :restart
    (completing-read
     "test"
     '("a" "b" "c")))

  (lotus-with-other-frame-event :cancel
    (completing-read
     "test"
     '("a" "b" "c")))

  (lotus-with-other-frame-event (message "Hi")
    (completing-read
     "test"
     '("a" "b" "c")))



  )

(when nil
  (lotus-restart-with-other-frame-event
    (completing-read
     "test"
     '("a" "b" "c")))
  )

(when nil

  (macroexpand-1
   '(lotus-with-other-frame-event :restart
     (completing-read
      "test"
      '("a" "b" "c"))))

  (macroexpand-1
   '(lotus-with-other-frame-event :cancel
     (completing-read
      "test"
      '("a" "b" "c"))))

  (macroexpand-1
   '(lotus-with-other-frame-event (message "Hi")
     (completing-read
      "test"
      '("a" "b" "c"))))



  )

(when nil
  (let ((a 1))                            ; binding (1)
    (let ((f (lambda () (print a))))
      (let ((a 2))                        ; binding (2)
        (funcall f)
        f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; post-command-hook is a variable defined in `C source code'.
;; Its value is (yas--post-command-handler t)

;; Permanently local in buffer *Help*; global value is
;; (global-font-lock-mode-check-buffers mmm-check-changed-buffers global-spacemacs-leader-override-mode-check-buffers global-undo-tree-mode-check-buffers evil-mode-check-buffers global-anzu-mode-check-buffers global-evil-search-highlight-persist-check-buffers global-evil-surround-mode-check-buffers global-flycheck-mode-check-buffers elscreen-run-screen-update-hook global-page-break-lines-mode-check-buffers show-smartparens-global-mode-check-buffers magit-auto-revert-mode-check-buffers yas-global-mode-check-buffers cperl-lazy-hook winner-save-old-configurations flycheck-pos-tip-hide-messages sp--post-command-hook-handler clean-aindent--check-last-point evil-repeat-post-hook hcz-set-cursor-color-according-to-mode eldoc-schedule-timer mode-local-post-major-mode-change)


;;   This variable is potentially risky when used as a file local variable.

;; Documentation:
;; Normal hook run after each command is executed.
;; If an unhandled error happens in running this hook,
;; the function in which the error occurred is unconditionally removed, since
;; otherwise the error might happen repeatedly and make Emacs nonfunctional.

;; It is a bad idea to use this hook for expensive processing.  If
;; unavoidable, wrap your code in `(while-no-input (redisplay) CODE)' to
;; avoid making Emacs unresponsive while the user types.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(when nil

  (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)

  (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)

  (advice-function-member-p #'quiet--select-frame (symbol-function  'select-frame-set-input-focus))

  )

;;; lotus-misc-utils.el ends here
