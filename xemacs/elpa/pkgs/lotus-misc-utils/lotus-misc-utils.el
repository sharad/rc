;;; lotus-misc-utils.el --- copy config  -*- lexical-binding: t; -*-

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <spratap@merunetworks.com>
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

(require 'timer)
(require 'timer-utils-lotus)
;; timer

(defmacro eval-if-focus (focus-body unfocus-body)
  "Eval FOCUS-BODY if focus, else eval UNFOCUS-BODY"
  `(if (has-focus)
      ,focus-body
      ,unfocus-body))

(defmacro eval-with-focus (&rest body)
  "Eval BODY with focus"
  `(progn
     (unless (has-focus)
       (grab-focus))
     ,@body))

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
(defun lotus-make-new-win ()
  (let ((size (lotus-new-lower-win-size))
        (window-min-height 7))
    (prog1
        (split-window-below size)
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

(defmacro lotus-with-no-active-minibuffer (minibuffer-body &rest body)
  ;;could schedule in little further.
  `(if (active-minibuffer-window)
       ,minibuffer-body
       (progn
         ,@body)))
(put 'lotus-with-no-active-minibuffer 'lisp-indent-function 1)

(defmacro lotus-with-override-minibuffer (&rest body)
  `(progn
     (when (active-minibuffer-window)
       (abort-recursive-edit))
     (unless (active-minibuffer-window)
       (progn
         ,@body))))
(put 'lotus-with-override-minibuffer 'lisp-indent-function 0)

(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil))
     (letrec ((readfn
               (lambda ()
                 (progn
                   (setq frame (selected-frame))
                   (message "readfn: frame %s" frame)
                   (message "readfn: 1 pre-command-hook %s" pre-command-hook)
                   (add-hook
                    'pre-command-hook
                    (lambda ()
                      (funcall hookfn)))
                   (message "readfn: 2 pre-command-hook %s" pre-command-hook)
                   (message "readfn: added hookfn")
                   (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                   (message "readfn: removed quiet-sel-frame")
                   (condition-case nil
                       (progn
                         (message "readfn: 1 running orginal code")
                         ,@body
                         (message "readfn: 1 pre-command-hook %s" pre-command-hook)
                         (remove-hook 'pre-command-hook (lambda () (funcall hookfn))))
                     (quit
                      (message "quit"))))))

              (hookfn1
               (lambda ()
                 (message "hookfn1: last-input-event: %s last-event-frame: %s frame: %s"
                          last-input-event
                          last-event-frame
                          frame)
                 (message "hookfn1: removing hook 1")
                 (message "hookfn1: 1 pre-command-hook %s" pre-command-hook)
                 (remove-hook 'pre-command-hook (lambda () (funcall hookfn1)))
                 (message "hookfn1: 2 pre-command-hook %s" pre-command-hook)
                 (if (eql last-event-frame frame)
                     (progn
                       (setq frame nil)
                       (message "hookfn1: removing hook 2")
                       (remove-hook 'pre-command-hook
                                    (lambda ()
                                      (funcall hookfn1))))
                   (progn
                     (setq frame nil)
                     (with-selected-frame last-event-frame
                       (message "hookfn1: with-selected-frame running timer")
                       (run-with-timer 0 nil (lambda () (funcall readfn)))
                       (message "hookfn1: adding quiet-sel-frame")
                       (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)
                       (message "hookfn1: going to run abort-recursive-edit")
                       (when (active-minibuffer-window)
                         (abort-recursive-edit)
                         (message "hookfn1: abort-recursive-edit")))))))

              (hookfn
               (lambda ()
                 (message "hookfn: last-input-event: %s last-event-frame: %s frame: %s"
                          last-input-event
                          last-event-frame
                          frame)
                 (message "hookfn: removing hook 1")
                 (message "hookfn: 1 pre-command-hook %s" pre-command-hook)
                 (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                 (message "hookfn: 2 pre-command-hook %s" pre-command-hook)
                 (if (eql last-event-frame frame)
                     (progn
                       (setq frame nil)
                       ;; (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                       ;; (message "hookfn: removing hook 2")
                       ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                       t)
                   (with-selected-frame last-event-frame
                     (progn
                       (setq frame nil)
                       (run-with-timer 0 nil
                                       (lambda ()
                                         (progn
                                           ;; (setq frame (selected-frame))
                                           (setq debug-on-quit nil)
                                           (message "hookfn: with-selected-frame running timer")
                                           (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
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
                         (message "hookfn: adding quiet-sel-frame")
                         (add-function :override (symbol-function  'select-frame-set-input-focus) #'quiet--select-frame)
                         (message "hookfn: going to run abort-recursive-edit")
                         (when (active-minibuffer-window)
                           (abort-recursive-edit)
                           (message "hookfn: abort-recursive-edit")))))))))
       (message "calling readfn")
       (funcall readfn))))

(defmacro lotus-with-other-frame-event (action &rest body)
  `(let ((frame nil))
     (letrec ((readfn
               (lambda ()
                 (progn
                   (setq frame (selected-frame))
                   (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                   (condition-case nil
                       (progn
                         ,@body
                         (remove-hook 'pre-command-hook (lambda () (funcall hookfn))))
                     (quit nil)))))
              (hookfn
               (lambda ()
                 ;; (message "hookfn: last-input-event: %s last-event-frame: %s frame: %s"
                 ;;          last-input-event
                 ;;          last-event-frame
                 ;;          frame)
                 (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                 (if (eql last-event-frame frame)
                     (progn
                       (setq frame nil)
                       ;; (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                       ;; (message "hookfn: removing hook 2")
                       ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                       t)
                   (with-selected-frame last-event-frame
                     (progn
                       (setq frame nil)
                       (run-with-timer 0 nil
                                       (lambda ()
                                         (progn
                                           ;; (setq frame (selected-frame))
                                           (setq debug-on-quit nil)
                                           (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
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
  `(let ((frame nil))
     (letrec ((readfn
               (lambda ()
                 (progn
                   (setq frame (selected-frame))
                   (add-hook 'pre-command-hook (lambda () (funcall hookfn)))
                   (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                   (condition-case nil
                       (progn
                         (message "readfn: inside %s" name)
                         ,@body
                         (remove-hook 'pre-command-hook (lambda () (funcall hookfn))))
                     (quit nil)))))
              (hookfn
               (lambda ()
                 ;; (message "hookfn: last-input-event: %s last-event-frame: %s frame: %s"
                 ;;          last-input-event
                 ;;          last-event-frame
                 ;;          frame)
                 (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                 (if (eql last-event-frame frame)
                     (progn
                       (setq frame nil)
                       ;; (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                       ;; (message "hookfn: removing hook 2")
                       ;; (remove-hook 'pre-command-hook (lambda () (funcall hookfn)))
                       t)
                   (with-selected-frame last-event-frame
                     (progn
                       (setq frame nil)
                       (run-with-timer 0 nil
                                       (lambda ()
                                         (progn
                                           ;; (setq frame (selected-frame))
                                           (setq debug-on-quit nil)
                                           (remove-function (symbol-function 'select-frame-set-input-focus) #'quiet--select-frame)
                                           ,@(cond
                                               ((or
                                                 (eq :restart action)
                                                 (eq t action))
                                                `(
                                                  (with-selected-frame last-event-frame
                                                    (message "hookfn: %s running readfn from hookfn" name)
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

(defmacro lotus-restart-with-other-frame-event (&rest body)
  `(lotus-with-other-frame-event :restart ,@body))
(put 'lotus-restart-with-other-frame-event 'lisp-indent-function 0)

(defmacro lotus-cancel-with-other-frame-event (&rest body)
  `(lotus-with-other-frame-event :cancel ,@body))
(put 'lotus-cancel-with-other-frame-event 'lisp-indent-function 0)

(defmacro lotus-run-with-other-frame-event (action &rest body)
  `(lotus-with-other-frame-event ,action ,@body))
(put 'lotus-cancel-with-other-frame-event 'lisp-indent-function 1)

;; https://stackoverflow.com/questions/3811448/can-call-with-current-continuation-be-implemented-only-with-lambdas-and-closures
;; CALL/CC

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


(let ((a 1))                            ; binding (1)
  (let ((f (lambda () (print a))))
    (let ((a 2))                        ; binding (2)
      (funcall f)
      f)))

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

(provide 'lotus-misc-utils)
;;; lotus-misc-utils.el ends here
