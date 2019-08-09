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


(require 'lotus-utils-debug)
(require 'timer)
(require 'timer-utils-lotus)
(require 'lotus-idle-utils)
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


(defmacro without-active-minibuffer (&rest body)
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
(put 'without-active-minibuffer 'lisp-indent-function 0)

(defmacro without-active-minibuffer-ensured (&rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(without-active-minibuffer
     (unless (active-minibuffer-window)
       ,@body)))
(put 'without-active-minibuffer-ensured 'lisp-indent-function 0)

(defmacro without-active-minibuffer-debug (minibuffer-body &rest body)
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
(put 'without-active-minibuffer-debug 'lisp-indent-function 1)

(defmacro without-active-minibuffer-ensured-debug (minibuffer-body &rest body)
  ;; https://oremacs.com/2015/07/16/callback-quit/
  ;; https://emacs.stackexchange.com/questions/20974/exit-minibuffer-and-execute-a-command-afterwards
  `(without-active-minibuffer-debug
       ,minibuffer-body
     (unless (active-minibuffer-window)
       ,@body)))
(put 'without-active-minibuffer-ensured-debug 'lisp-indent-function 1)

(when nil
  (defun test-minibuffer-quiting ()
    (without-active-minibuffer
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
      -12 -10))

;; create smaller and proper sized window
;; TODO: org-fit-window-to-buffer
(defun lotus-make-new-win ()
  (let ((size (lotus-new-lower-win-size))
        (window-min-height 7))
    (prog1
        (split-window-below size)
      (lotus-utils-debug 'lotus-new-win :debug "newwin size %d" size)
      (message "size %d" size))))

(defmacro lotus-with-new-win (newwin &rest body)
  `(let* ((,newwin (lotus-make-new-win)))
     ;; maybe leave two lines for our window because of the
     ;; normal `raised' mode line
     (select-window ,newwin 'norecord)
     (progn
       ,@body)))
(put 'lotus-with-new-win 'lisp-indent-function 1)

(defun safe-delete-window (window)
  (let ((win-parent            (window-parent window))
        (major-non-side-window (window--major-non-side-window nil)))
    (unless (or
             (not win-parent)
             (eq window major-non-side-window))
      (delete-window window))
    (progn                                ; debug
      (if (not win-parent)
          (lotus-utils-message "window = %s has no parent, (window-parent window) %s"
                               window
                               win-parent))
      (if (eq window
              major-non-side-window)
          (lotus-utils-message "window = %s is a major non side window."
                               window)))))

(defmacro lotus-with-timed-new-win (timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  (let ((temp-win-config (make-symbol "test-lotus-with-timed-new-win-config")))
    `(let* ((,temp-win-config (current-window-configuration))
            (,cleanupfn-newwin #'(lambda (w localfn)
                                   ;; (message "cleaning up newwin and triggered timer for newwin %s" w)
                                   (when localfn (funcall localfn))
                                   ;; (when (active-minibuffer-window) ;not required here. it is just creating timed new-win
                                   ;;   (abort-recursive-edit))
                                   (when (and w
                                              (windowp w)
                                              (window-valid-p w))
                                     ;; check if this is a sole window do not delete it.
                                     (safe-delete-window w))
                                   (when ,temp-win-config
                                     (set-window-configuration ,temp-win-config)
                                     (setq ,temp-win-config nil)))))
       (lotus-with-new-win ,newwin
         (let* ((,timer
                 (when (and
                        ,timeout
                        (numberp ,timeout)
                        (not (> ,timeout 0)))
                   (run-with-idle-plus-timer ,timeout
                                             nil
                                             ,cleanupfn-newwin
                                             ,newwin
                                             ,cleanupfn-local))))
           (condition-case err
               (progn
                 ,@body)
             ((quit)
              (funcall ,cleanupfn-newwin ,newwin ,cleanupfn-local))))))))
(put 'lotus-with-timed-new-win 'lisp-indent-function 5)


;; Marker Macros Starts
(defmacro lotus-with-marker-alt (marker &rest body)
  `(let* ((marker ,marker)
          (buffer (marker-buffer marker)))
     (save-excursion ; Do not replace this with `with-current-buffer'.
       (with-no-warnings (set-buffer buffer))
       (save-restriction
         (widen)
         (goto-char marker)
         (progn
           ,@body)))))
;; Marker Macros Ends

(defmacro lotus-with-marker (marker &rest body)
  `(let ((marker ,marker))
     (when (and marker (marker-buffer marker))
       (let ((target-buffer (marker-buffer   marker))
             (pos           (marker-position marker)))
         (if target-buffer
             (with-current-buffer target-buffer
               (message "lotus-with-marker: selecting buf %s" target-buffer)
               (if (<= pos (point-max))
                   (progn
                     (goto-char pos)
                     ,@body)
                 (error "position %d greater than point max %d" pos (point-max))))
           (error "No buffer"))))))
(put 'lotus-with-marker 'lisp-indent-function 1)

(defmacro lotus-with-pos (pos &rest body)
  `(let ((pos ,pos))
     (message "lotus-with-buffer-pos-new-win: selecting buf %s" (current-buffer))
     (if (<= pos (point-max))
         (progn
           (goto-char pos)
           ,@body)
       (error "position %d greater than point max %d" pos (point-max)))))
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
  `(let ((marker ,marker))
     (when (marker-buffer marker)
       (let ((target-buffer (marker-buffer   marker))
             (pos           (marker-position marker)))
         (if target-buffer
             (lotus-with-new-win ,newwin
               (message "lotus-with-marker-new-win: selecting buf %s in %s win" target-buffer ,newwin)
               ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
               (switch-to-buffer target-buffer)
               (if (<= pos (point-max))
                   (progn
                     (goto-char pos)
                     ,@body)
                 (error "position %d greater than point max %d" pos (point-max))))
           (error "No buffer"))))))
(put 'org-with-marker-new-win 'lisp-indent-function 1)

(defmacro lotus-with-pos-new-win (pos newwin &rest body)
  `(let ((pos ,pos)
         (target-buffer (current-buffer)))
     (if target-buffer
         (lotus-with-new-win ,newwin
           (message "lotus-with-pos-new-win: selecting buf %s in %s win" target-buffer ,newwin)
           ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
           (switch-to-buffer target-buffer)
           (if (<= pos (point-max))
               (progn
                 (goto-char pos)
                 ,@body)
             (error "position %d greater than point max %d" pos (point-max))))
       (error "No buffer"))))
(put 'lotus-with-pos-new-win 'lisp-indent-function 1)

(defmacro lotus-with-buffer-pos-new-win (buffer pos newwin &rest body)
  `(let ((pos ,pos)
         (target-buffer (if ,buffer ,buffer (current-buffer))))
     (if target-buffer
         (lotus-with-new-win ,newwin
           (message "lotus-with-buffer-pos-new-win: selecting buf %s in %s win" target-buffer ,newwin)
           ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
           (switch-to-buffer target-buffer)
           (if (<= pos (point-max))
               (progn
                 (goto-char pos)
                 ,@body)
             (error "position %d greater than point max %d" pos (point-max))))
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
  `(let ((marker ,marker))
     (when (marker-buffer marker)
       (let ((target-buffer (marker-buffer   marker))
             (pos           (marker-position marker)))
         (lotus-with-timed-new-win
             ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
             (message "lotus-with-marker-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
             ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
             (switch-to-buffer target-buffer)
             (if (<= pos (point-max))
                 (progn
                   (goto-char pos)
                   ,@body)
               (error "position %d greater than point max %d" pos (point-max))))))))
(put 'lotus-with-marker-timed-new-win 'lisp-indent-function 1)

(defmacro lotus-with-pos-timed-new-win (pos timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(let ((pos ,pos)
         (target-buffer (current-buffer)))
     (lotus-with-timed-new-win
         ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
         (message "lotus-with-buffer-pos-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
         ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
         (switch-to-buffer target-buffer)
         (if (<= pos (point-max))
             (progn
               (goto-char pos)
               ,@body)
           (error "position %d greater than point max %d" pos (point-max))))))
(put 'lotus-with-pos-timed-new-win 'lisp-indent-function 1)

(defmacro lotus-with-buffer-pos-timed-new-win (buffer pos timeout timer cleanupfn-newwin cleanupfn-local newwin &rest body)
  `(let ((pos ,pos)
         (target-buffer (if ,buffer ,buffer (current-buffer))))
     (if target-buffer
         (lotus-with-timed-new-win
             ,timeout ,timer ,cleanupfn-newwin ,cleanupfn-local ,newwin
             (message "lotus-with-buffer-pos-timed-new-win: selecting buf %s in %s win" target-buffer ,newwin)
             ;; (set-buffer target-buffer) ;; it work temporarily so can not use.
             (switch-to-buffer target-buffer)
             (if (<= pos (point-max))
                 (progn
                   (goto-char pos)
                   ,@body)
               (error "position %d greater than point max %d" pos (point-max))))
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
         (lotus-utils-debug 'active-minibuffer-if :debug "%s: %s: cancelled as active minibuffer found." (time-stamp-string) 'lotus-with-no-active-minibuffer-if))
     (progn
       (lotus-utils-debug 'active-minibuffer-if :debug "%s: %s: no active minibuffer found." (time-stamp-string) 'lotus-with-no-active-minibuffer-if)
       ,@body)))
(put 'lotus-with-no-active-minibuffer-if 'lisp-indent-function 1)

(defmacro lotus-with-override-minibuffer (&rest body)
  `(without-active-minibuffer-ensured
     ,@body))
(put 'lotus-with-override-minibuffer 'lisp-indent-function 0)

(defmacro lotus-with-override-minibuffer-if (minibuffer-body &rest body)
  `(without-active-minibuffer-ensured-debug
       (progn
         (lotus-utils-debug 'active-minibuffer-if :debug "%s: %s: aborting active minibuffer." (time-stamp-string) 'lotus-with-override-minibuffer-if)
         ,minibuffer-body)
     ,@body))
(put 'lotus-with-override-minibuffer-if 'lisp-indent-function 1)




;;; TODO: extend it to include elscreen change also.
(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (select-frame-set-input-focus-no-raise-p)))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (unless (select-frame-set-input-focus-no-raise-p)
                         (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> add quiet 5 as already was present" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                         (select-frame-set-input-disable-raise))
                     (when (select-frame-set-input-focus-no-raise-p)
                       (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> remove quiet 5 as already was present" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                       (select-frame-set-input-enable-raise)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> frame %s" (time-stamp-string) 'lotus-with-other-frame-event ,action frame)
                     (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> 1 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                     (add-hook
                      'pre-command-hook
                      (lambda ()
                        (funcall hookfn)))
                     (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> 2 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                     ;; (unless sel-frame-adviced-p
                     ;;   (select-frame-set-input-enable-raise)
                     ;;   (lotus-utils-debug 'event-input :debug "readfn: removed quiet-sel-frame"))
                     (condition-case nil
                         (progn
                           (funcall set-advice-fn)
                           (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> 3 running orginal code minibuffer<%s> " (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           ,@body
                           (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> 4 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                           (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                           (funcall set-advice-fn))
                       (quit
                        (lotus-utils-debug 'event-input :debug "%: %s: <%s> quit" (time-stamp-string) 'lotus-with-other-frame-event ,action))))))

              (hookfn1
               #'(lambda ()
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> last-input-event: %s last-event-frame: %s frame: %s"
                          (time-stamp-string)
                          'lotus-with-other-frame-event ,action
                          last-input-event
                          last-event-frame
                          frame)
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> removing hook 1" (time-stamp-string) 'lotus-with-other-frame-event ,action )
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> 1 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn1)))
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> 2 pre-command-hook %s" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook)
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> removing hook 2" (time-stamp-string) 'lotus-with-other-frame-event ,action)
                         (remove-hook 'pre-command-hook
                                      (lambda ()
                                        (funcall hookfn1))))
                     (progn
                       (setq frame nil)
                       (with-selected-frame last-event-frame
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> with-selected-frame running timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (run-with-timer 0 nil #'(lambda () (funcall readfn)))
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> adding quiet-sel-frame minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (select-frame-set-input-disable-raise)
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> going to run abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (when (active-minibuffer-window)
                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn1: <%s> running abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (abort-recursive-edit)))))))

              (hookfn
               #'(lambda ()
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> last-input-event: %s last-event-frame: %s frame: %s minibuffer<%s>"
                          (time-stamp-string)
                          'lotus-with-other-frame-event ,action
                          last-input-event
                          last-event-frame
                          frame
                          (active-minibuffer-window))
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> removing hook 1 minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> 1 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> 2 pre-command-hook %s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action pre-command-hook (active-minibuffer-window))
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (select-frame-set-input-enable-raise)
                         ;; (lotus-utils-debug 'event-input :debug "hookfn: removing hook 2")
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
                                               (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> with-selected-frame running timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
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
                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> adding quiet-sel-frame minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (select-frame-set-input-disable-raise)
                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> going to run abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                           (when (active-minibuffer-window)
                             (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: <%s> running abort-recursive-edit minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                             (abort-recursive-edit)))))))))
       (lotus-utils-debug 'event-input :debug "%s: %s: <%s> calling readfn minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
       (funcall readfn))))

(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (select-frame-set-input-focus-no-raise-p)))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (unless (select-frame-set-input-focus-no-raise-p)
                         (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> add quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                         (select-frame-set-input-disable-raise))
                     (when (select-frame-set-input-focus-no-raise-p)
                       (lotus-utils-debug 'event-input :debug "%s: %s: readfn: <%s> remove quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event ,action (active-minibuffer-window))
                       (select-frame-set-input-enable-raise)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                     ;; (unless sel-frame-adviced-p
                     ;;   (select-frame-set-input-enable-raise))
                     (condition-case nil
                         (progn
                           (funcall set-advice-fn)
                           ,@body
                           (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                           (funcall set-advice-fn))
                       (quit nil)))))
              (hookfn
               #'(lambda ()
                   ;; (lotus-utils-debug 'event-input :debug "hookfn: last-input-event: %s last-event-frame: %s frame: %s"
                   ;;          last-input-event
                   ;;          last-event-frame
                   ;;          frame)
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (select-frame-set-input-enable-raise)
                         ;; (lotus-utils-debug 'event-input :debug "hookfn: removing hook 2")
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
                           (select-frame-set-input-disable-raise)
                           (when (active-minibuffer-window)
                             (abort-recursive-edit)))))))))
       (funcall readfn))))
(put 'lotus-with-other-frame-event 'lisp-indent-function 1)

(defmacro lotus-with-other-frame-event-debug (name action &rest body)
  `(let ((frame nil)
         (sel-frame-adviced-p
          (select-frame-set-input-focus-no-raise-p)))
     (letrec ((set-advice-fn
               #'(lambda ()
                   (if sel-frame-adviced-p
                       (unless (select-frame-set-input-focus-no-raise-p)
                         (lotus-utils-debug 'event-input :debug "%s: %s: set-advice-fn: %s <%s> add quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                         (lotus-utils-debug 'event-input :debug "%s: %s: set-advice-fn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                            (time-stamp-string) 'lotus-with-other-frame-event-debug
                                            ,name ,action
                                            last-input-event
                                            last-event-frame
                                            frame
                                            (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                         (select-frame-set-input-disable-raise))
                     (when (select-frame-set-input-focus-no-raise-p)
                       (lotus-utils-debug 'event-input :debug "%s: %s: readfn: %s <%s> remove quiet 5 as already was present minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                       (lotus-utils-debug 'event-input :debug "%s: %s: set-advice-fn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                          (time-stamp-string) 'lotus-with-other-frame-event-debug
                                          ,name ,action
                                          last-input-event
                                          last-event-frame
                                          frame
                                          (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                       (select-frame-set-input-enable-raise)))))
              (readfn
               #'(lambda ()
                   (progn
                     (setq frame (selected-frame))
                     (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                     (progn
                       (lotus-utils-debug 'event-input :debug "%s: %s: readfn: %s <%s> inside readfn minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                       (lotus-utils-debug 'event-input :debug "%s: %s: readfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                          (time-stamp-string) 'lotus-with-other-frame-event-debug
                                          ,name ,action
                                          last-input-event
                                          last-event-frame
                                          frame
                                          (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                       (funcall set-advice-fn)
                       (prog1
                           (progn
                             ,@body)
                         (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                         (funcall set-advice-fn))))))
              (hookfn
               #'(lambda ()
                   (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                      (time-stamp-string) 'lotus-with-other-frame-event-debug
                                      ,name ,action
                                      last-input-event
                                      last-event-frame
                                      frame
                                      (selected-frame) (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                   (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   ;; TODO: Here add support for elscreen change also, make it generalized function
                   (if (eql last-event-frame frame)
                       (progn
                         (setq frame nil)
                         ;; (select-frame-set-input-enable-raise)
                         ;; (lotus-utils-debug 'event-input :debug "hookfn: removing hook 2")
                         ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                         t)
                     (with-selected-frame last-event-frame
                       (progn
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: %s <%s> running readfn from hookfn outside timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                         (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
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
                                               (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: %s <%s> timer remove quiet 1 minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                                               (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
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
                                                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: %s <%s> running readfn from hookfn inside timer minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (active-minibuffer-window))
                                                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
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
                                                 (lotus-utils-debug 'event-input :debug
                                                                    "%s: %s: hookfn: %s <%s> finished running %s frame=%s minibuffer<%s>"
                                                                    (time-stamp-string)
                                                                    'lotus-with-other-frame-event-debug
                                                                    ,name
                                                                    ,action
                                                                    ,action
                                                                    (selected-frame)
                                                                    (active-minibuffer-window))
                                                 (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                                                    (time-stamp-string) 'lotus-with-other-frame-event-debug
                                                                    ,name ,action
                                                                    last-input-event
                                                                    last-event-frame
                                                                    frame
                                                                    (selected-frame)
                                                                    (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))))))
                         (progn
                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: %s <%s> add quiet 2 frame=%s minibuffer<%s>" (time-stamp-string) 'lotus-with-other-frame-event-debug ,name ,action (selected-frame) (active-minibuffer-window))
                           (lotus-utils-debug 'event-input :debug "%s: %s: hookfn: name=%s <%s> last-input-event: %s last-event-frame: %s frame: %s selected-frame=%s eq=%s eql=%s equal=%s minibuffer<%s>"
                                              (time-stamp-string) 'lotus-with-other-frame-event-debug
                                              ,name ,action
                                              last-input-event
                                              last-event-frame
                                              frame
                                              (selected-frame)
                                              (eq last-event-frame frame) (eql last-event-frame frame) (equal last-event-frame frame) (active-minibuffer-window))
                           (select-frame-set-input-disable-raise)
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


(defmacro lotus-run-when-idle (secs &rest body)
  `(letrec ((timer nil)
            (fn
             (lambda ()
               (let ((retval
                      (while-no-input (redisplay)
                                      ,@body
                                      :complete)))))))

     (setq
      timer
      (run-with-idle-timer secs nil))

     `(while-no-input (redisplay))))


(defmacro lotus-run-unobtrusively (&rest body)
  `(let ((retval (while-no-input
                   (redisplay)
                   ,@body)))
     (when (eq retval t)
       (lotus-utils-debug 'event-input :debug "user input %s retval %s" last-input-event retval))
     retval))
(put 'lotus-run-unobtrusively 'lisp-indent-function 0)

(defmacro lotus-run-unobtrusively-throw-on-input (&rest body) ;throw
  `(while-no-input
     (redisplay)
     ,@body))
(put 'lotus-run-unobtrusively-throw-on-input 'lisp-indent-function 0)

;; TODO complete it using letrec
(defmacro lotus-run-unobtrusively-complete-when-idle (idletime &rest body) ;throw
  `(let ((retval
          (while-no-input
            (redisplay)
            ,@body)))
     retval))
(put 'lotus-run-unobtrusively-complete-when-idle 'lisp-indent-function 0)


;; https://stackoverflow.com/questions/3811448/can-call-with-current-continuation-be-implemented-only-with-lambdas-and-closures
;; CALL/CC

(defun lotus-has-focus-p ()
  )

(defmacro lotus-run-with-idle-timer-and-focus (sec repeat fn arg)
  ;; todo: how to cancel the timer later
  (let ((timer nil)
        (frame (selected-frame)))
    (letrec ((focusfn
              (lambda ()
                (if (and
                     (lotus-has-focus-p)
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

  (select-frame-set-input-enable-raise)

  (select-frame-set-input-disable-raise)

  (select-frame-set-input-focus-no-raise-p)

  )

;;; lotus-misc-utils.el ends here
