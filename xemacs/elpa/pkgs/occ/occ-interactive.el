;;; occ-interactive.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-interactive)


(require 'timer-utils-lotus)
(eval-when-compile
  (require 'timer-utils-lotus))
(require 'lotus-utils)
(eval-when-compile
  (require 'lotus-utils))
(require 'lotus-idle-utils)
(eval-when-compile
  (require 'lotus-idle-utils))
(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))
(require 'org-misc-utils-lotus)
(eval-when-compile
  (require 'org-misc-utils-lotus))

(require 'occ-obj-method)


(cl-defgeneric occ-select-propetry (tsk
                                    ctx
                                    &optional prompt)
  "occ-select-propetry")

(cl-defmethod occ-select-propetry ((tsk occ-tsk)
                                   (ctx occ-ctx)
                                   &optional prompt)
  (let ((prompt (or prompt "proptery: "))
        (fixed-keys '(edit done))
        (keys   (cl-method-sigs-matched-arg
                 '(occ-readprop         (`((head ,val) occ-ctx) val))
                 '(occ-ctx-property-get (`((head ,val)) val))
                 ctx)))
    (let ((maxkeylen (apply
                      #'max
                      (mapcar #'(lambda (sym) ;https://www.gnu.org/software/emacs/manual/html_node/elisp/Formatting-Strings.html
                                  (length (symbol-name sym)))
                              (append keys fixed-keys))))
          (key-vals  (occ-get-properties tsk keys)))
      (let* ((key-val-collection
              (mapcar
               #'(lambda (key-val)
                   (cons
                    (if (cdr key-val)
                        (format "%s: %s" (car key-val) (cdr key-val))
                      (symbol-name (car key-val)))
                    (car key-val)))
               key-vals))
             (key-val-collection (append
                                  key-val-collection
                                  (mapcar #'(lambda (fk) (cons (symbol-name fk) fk))
                                          fixed-keys))))
        (let ((sel
               (assoc
                (occ-completing-read prompt
                                     key-val-collection
                                     nil
                                     t)
                key-val-collection)))
          (occ-debug :debug "selected option")
          (cdr sel))))))


(defun org-get-flag-proprty-drawer (&optional force)
  (let ((range (org-get-property-block (point) force)))
    (when range
      org-cycle-subtree-status)))

(defun org-flag-proprty-drawer (flag
                                &optional force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  ;; (recenter-top-bottom 2)
  ;; (unless flag                  ;; creating issue in cleanupfn error as display buffer and current buffer is not same.
  ;;   (recenter-top-bottom 2))
  (let ((prop-range (org-get-property-block (point) force)))
    ;; first show heading
    (when (eq org-cycle-subtree-status 'folded)
      (unless flag
        ;; https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00573.html
        (progn
          (when (or
                 (org-invisible-p)
                 (org-invisible-p2))
            (org-show-context 'org-goto)))
        (progn                                        ; changed from org-show-tsk to org-show-entry
          (org-show-entry)
          (occ-debug :debug
                     "did %s entry `%s'" (if flag "close" "open") heading)
          (org-unlogged-message "CHILDREN")
          (setq org-cycle-subtree-status 'children))))
    ;; show expand property if flag is nil, else hide
    (let* ((prop-range (org-get-property-block (point) force))
           (prop-loc   (when (consp prop-range) (1- (car prop-range)))))
      (when prop-range
        (occ-debug :debug "pos %d before jumping to %s drawer, will jump to pos %d"
                   (point)
                   (if flag "close" "open")
                   prop-loc)
        (goto-char prop-loc)
        (occ-debug :debug "reached to %s drawer" (if flag "close" "open"))
        (if (org-at-drawer-p)
            ;; show drawer
            (let ((drawer (org-element-at-point)))
              (when (memq (org-element-type drawer)
                          '(node-property drawer property-drawer))
                (occ-debug :debug
                           "trying to %s drawer %s current pos %d"
                           (if flag "close" "open")
                           drawer
                           (point))
                (org-flag-drawer flag drawer)
                ;; Make sure to skip drawer entirely or we might flag
                ;; it another time when matching its ending line with
                ;; `org-drawer-regexp'.
                (when nil       ;;BUG ?? what
                  (goto-char (org-element-property :end drawer)))))
          (occ-debug :debug "not at drawer to %s current pos is %s"
                     (if flag "close" "open")
                     (point)))
        (goto-char prop-loc)
        (occ-debug :debug "reached to %s drawer1 current pos %d"
                   (if flag "close" "open")
                   (point))
        prop-range))))


(defun org-get-flag-proprty-drawer-at-marker (marker
                                              &optional force)
  (let ((buff (marker-buffer marker))
        (loc  (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (org-get-flag-proprty-drawer force))))))

(defun org-flag-proprty-drawer-at-marker (marker
                                          flag
                                          &optional force)
  "NIL to open drawer T to close drawer"
  ;; https://orgmode.org/worg/org-hacks.html
  ;; https://orgmode.org/worg/org-hacks.html#org6d4906f
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker))
        (heading (org-get-heading 'notags)))
    (when (and buff loc)
      (with-current-buffer buff
        (let ((currloc (point)))
          (goto-char loc)
          (occ-debug :debug "%s: called to %s drawer of heading `%s' in file %s loc %d"
                     (time-stamp-string)
                     (if flag "close" "open")
                     heading
                     (buffer-file-name buff)
                     loc)
          (org-flag-proprty-drawer flag force))))))


(cl-defmethod occ-open-prop-block ((obj marker))
  ;; Find better name
  (let ((mrk              obj)
        (buffer-read-only nil))
    ;; (occ-debug :debug "timer started for win %s" win)
    (let ((buff (marker-buffer   mrk))
          (pos  (marker-position mrk)))
      ;; show proptery drawer
      (when buff
        (switch-to-buffer buff)
        (goto-char pos)
        (set-marker mrk (point))
        (recenter-top-bottom 2)
        (let* ((prop-range (org-flag-proprty-drawer-at-marker mrk nil))
               (prop-loc   (when (consp prop-range) (1- (car prop-range)))))

          (show-all)
          (when (numberp prop-loc)
            (goto-char prop-loc)))))))

(cl-defmethod occ-open-prop-block ((obj null))
  (occ-open-prop-block (point-marker)))


(cl-defmethod occ-props-edit-with ((obj occ-obj-tsk)
                                   (ctx occ-ctx))
  (if (occ-open-prop-block (occ-obj-marker obj))
      (let ((prop nil))
        (while (not
                (member
                 (setq prop (occ-select-propetry obj ctx))
                 '(edit done)))
          (when (occ-editprop prop ctx)
            (occ-tsk-update-tsks t))))
    (error "can not edit props")))

(cl-defmethod occ-props-edit ((obj occ-obj-ctx-tsk))
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-props-edit-with tsk ctx)))


(cl-defmethod occ-props-window-edit-with ((obj occ-tsk)
                                          (ctx occ-ctx)
                                          &optional timeout)
  (let* ((tsk     obj)
         (mrk     (occ-tsk-marker obj))
         (timeout (or timeout 0)))
    (when mrk
      (org-with-cloned-marker mrk "<proptree>"
        (org-with-narrow-to-marker mrk

          (let* ((marker (point-marker))
                 (local-cleanup
                  #'(lambda ()
                      (save-excursion ;what to do here
                        (org-flag-proprty-drawer-at-marker mrk t))
                      (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                        (abort-recursive-edit)))))
            (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                timeout timer cleanup local-cleanup win
                (condition-case-control nil err
                  (let ((prop (occ-props-edit-with obj ctx)))
                    (cond
                     ((eql 'done prop)
                      (funcall cleanup win local-cleanup)
                      (when timer (cancel-timer timer)))
                     ((eql 'edit prop)
                      ;; (funcall cleanup win local-cleanup)
                      (occ-debug :debug "occ-obj-prop-edit: debug editing")
                      (when timer (cancel-timer timer))
                      (when (and win (windowp win) (window-valid-p win))
                        (select-window win 'norecord)))
                     (t
                      (funcall cleanup win local-cleanup)
                      (when timer (cancel-timer timer)))))
                  ((quit)
                   (progn
                     (funcall cleanup win local-cleanup)
                     (if timer (cancel-timer timer))
                     (signal (car err) (cdr err))))))))))))


(cl-defmethod occ-props-window-edit ((obj occ-obj-ctx-tsk)
                                     &key
                                     collector
                                     action
                                     action-transformer
                                     timeout)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-props-window-edit-with tsk ctx timeout)))

(cl-defmethod occ-props-window-edit ((obj occ-ctx)
                                     &key
                                     candidate-transformer
                                     occ-select-clock-in-tranform
                                     occ-select-clock-in-tranformer-fun-transform
                                     collector
                                     action
                                     action-transformer
                                     timeout)
  (let* ((collector          (or collector #'occ-list))
         (action             (or action (occ-helm-actions obj)))
         (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
         (timeout            (or timeout occ-idle-timeout)))
    (lwarn 'occ :debug "occ-select-obj-prop-edit: [body] lotus-with-no-active-minibuffer-if")
    (let ((buff (occ-ctx-buffer obj)))
      (if (and
           (buffer-live-p buff)
           (not (occ-helm-buffer-p buff))
           (let ((ctx-tsk (occ-select obj
                                      :collector          collector
                                      :action             action
                                      :action-transformer action-transformer
                                      :timeout            timeout)))
             ;; BUG: will do run recursively as another method with (obj null) is define below.
             (occ-props-window-edit ctx-tsk)))
          (occ-debug :debug "not running add-ctx-to-org-heading as context buff is deleted or not live 1 %s, 2 %s"
                     (buffer-live-p buff)
                     (not (occ-helm-buffer-p buff)))))))

(cl-defmethod occ-props-window-edit ((obj null)
                                     &key
                                     collector
                                     action
                                     action-transformer
                                     timeout)
  (let ((collector          (or collector #'occ-list))
        (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
      (occ-props-window-edit (occ-make-ctx-at-point)
                             :collector          collector
                             :action             action
                             :action-transformer action-transformer
                             :timeout            timeout)))


(cl-defmethod occ-safe-props-window-edit ((obj occ-ctx)
                                          &key
                                          collector
                                          action
                                          action-transformer
                                          timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx-at-point) occ-idle-timeout))
  (let ((collector          (or collector #'occ-list))
        (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "begin occ-delayed-select-obj-prop-edit")
    (lotus-with-no-active-minibuffer-if
        (progn
          (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit: [minibuffer-body] lotus-with-no-active-minibuffer-if")
          (occ-debug :debug "add-ctx-to-org-heading: minibuffer already active quitting")
          (occ-debug :debug nil))
      ;;; TODO: extend lotus-with-other-frame-event-debug it to include elscreen change also.
      (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit" :cancel
        (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit: lotus-with-other-frame-event-debug")
        (let ((buff (occ-ctx-buffer obj)))
          (if (eq (current-buffer) buff)
              (occ-props-window-edit obj
                                     :collector          collector
                                     :action             action
                                     :action-transformer action-transformer
                                     :timeout            timeout)
            (occ-debug :debug "context is not for current buffer.")))))
    (occ-debug :debug "finished occ-delayed-select-obj-prop-edit")))

(cl-defmethod occ-safe-props-window-edit ((obj marker)
                                          &key
                                          collector
                                          action
                                          action-transformer
                                          timeout)
  (occ-safe-props-window-edit (occ-make-ctx marker)
                              :collector          collector
                              :action             action
                              :action-transformer action-transformer
                              :timeout            timeout))

(cl-defmethod occ-safe-ignore-quit-props-window-edit ((obj occ-ctx)
                                                      &key
                                                      collector
                                                      action
                                                      action-transformer
                                                      timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (let ((collector          (or collector #'occ-list))
        (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug :debug "called occ-delayed-select-obj-prop-edit-when-idle")
    (occ-debug :debug "%s: begin: occ-delayed-select-obj-prop-edit-when-idle" (time-stamp-string))
    ;; timed-newwin of occ-delayed-select-obj-prop-edit pass quit
    ;; signal to caller mean here, so need to be handled, else this function can
    ;; not return any value to its caller, which result into no next-action in
    ;; caller function.
    (condition-case-control nil nil
      (progn
        ;; TODO: Add code to which check if only focus present than only trigger
        ;; else postpone it by calling run-with-idle-plus-timer
        (lwarn 'occ
               :debug
               "occ-delayed-select-obj-prop-edit-when-idle: calling occ-delayed-select-obj-prop-edit with this-command=%s" this-command)
        (occ-safe-props-window-edit obj
                                    :collector          collector
                                    :action             action
                                    :action-transformer action-transformer
                                    :timeout            timeout))

      ;; (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit-when-idle" :cancel
      ;;   (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit-when-idle: lotus-with-other-frame-event-debug")
      ;;   (occ-delayed-select-obj-prop-edit ctx timeout))
      ((quit)))
    (occ-debug :debug "%s: end: occ-delayed-select-obj-prop-edit-when-idle" (time-stamp-string))))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  occ-idle-timeout nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-delayed-select-obj-prop-edit args)) (list ctx timeout))






(cl-defgeneric occ-obj-prop-edit (obj
                                  ctx
                                  timeout)
  "occ-prop-edit")

(cl-defmethod occ-obj-prop-edit ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 timeout)
  (let* ((timeout (or timeout 0))
         (tsk     obj)
         (mrk     (occ-tsk-marker obj)))
    (when mrk
      (org-with-cloned-marker mrk "<proptree>"
        (org-with-narrow-to-marker mrk
          (let* ((marker (point-marker))
                 (local-cleanup
                  #'(lambda ()
                      (save-excursion ;what to do here
                        (org-flag-proprty-drawer-at-marker mrk t))
                      (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                        (abort-recursive-edit)))))
            (show-all)
            (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                timeout timer cleanup local-cleanup win

                (let ((target-buffer (marker-buffer marker))
                      (pos           (marker-position marker)))

                  (when target-buffer
                    (progn
                      (progn
                        (switch-to-buffer target-buffer)
                        (goto-char pos)
                        (set-marker marker (point))
                        (recenter-top-bottom 2))
                      (progn
                        (occ-debug :debug "called add-ctx-to-org-heading %s" (current-buffer))
                        (condition-case-control nil err

                          ;; see can be put into one function
                          (let ((buffer-read-only nil))
                            (occ-debug :debug "timer started for win %s" win)

                            ;; show proptery drawer
                            (let* ((prop-range (org-flag-proprty-drawer-at-marker marker nil))
                                   (prop-loc   (when (consp prop-range) (1- (car prop-range)))))
                              (if (numberp prop-loc)
                                  (goto-char prop-loc)))

                            ;; try to read values of properties.
                            (let ((prop nil))
                              (while (not
                                      (member
                                       (setq prop (occ-select-propetry tsk ctx))
                                       '(edit done)))
                                (when (occ-editprop prop ctx)
                                  (occ-tsk-update-tsks t)))
                              (cond
                               ((eql 'done prop)
                                (funcall cleanup win local-cleanup)
                                (when timer (cancel-timer timer)))
                               ((eql 'edit prop)
                                ;; (funcall cleanup win local-cleanup)
                                (occ-debug :debug "occ-obj-prop-edit: debug editing")
                                (when timer (cancel-timer timer))
                                (when (and win (windowp win) (window-valid-p win))
                                  (select-window win 'norecord)))
                               (t
                                (funcall cleanup win local-cleanup)
                                (when timer (cancel-timer timer))))))

                          ((quit)
                           (progn
                             (funcall cleanup win local-cleanup)
                             (if timer (cancel-timer timer))
                             (signal (car err) (cdr err))))))))))))))))

(cl-defmethod occ-obj-prop-edit ((obj occ-ctsk)
                                 (ctx occ-ctx)
                                 timeout)
  (occ-obj-prop-edit (occ-ctsk-tsk obj) ctx timeout))

(cl-defmethod occ-obj-prop-edit ((obj marker)
                                 (ctx occ-ctx)
                                 timeout)
  (occ-obj-prop-edit (occ-make-tsk obj) ctx timeout))

(cl-defmethod occ-obj-prop-edit ((obj null)
                                 (ctx occ-ctx)
                                 timeout))



;; (safe-timed-org-refile-get-marker occ-idle-timeout)

;; (defun occ-select-marker)

(cl-defgeneric occ-select-obj-prop-edit (obj
                                         ctx
                                         timeout)
  "occ-prop-edit")

(cl-defmethod occ-select-obj-prop-edit ((obj null)
                                        (ctx occ-ctx)
                                        timeout)
  (let* ((timeout (or timeout occ-idle-timeout))
         (buff    (occ-ctx-buffer ctx)))
    (lwarn 'occ :debug "occ-select-obj-prop-edit: [body] lotus-with-no-active-minibuffer-if")
    (if (and
         (buffer-live-p buff)
         (not (occ-helm-buffer-p buff)))
        ;; TODO: BUG: Here propagate selection abort properly for (occ-clock-in ((obj occ-ctx)) to work properly.
        (occ-obj-prop-edit (occ-select obj #'occ-list timeout) ctx timeout)
      (occ-debug :debug "not running add-ctx-to-org-heading as context buff is deleted or not live 1 %s, 2 %s 3 %s"
                 (eq (current-buffer) buff)
                 (buffer-live-p buff)
                 (eq buff
                     (get-buffer "*helm-mode-occ-select-obj-prop-edit*"))))))

(cl-defmethod occ-select-obj-prop-edit ((obj occ-ctx)
                                        (ctx occ-ctx)
                                        timeout)
  (let* ((timeout (or timeout occ-idle-timeout))
         (buff    (occ-ctx-buffer ctx)))
    (lwarn 'occ :debug "occ-select-obj-prop-edit: [body] lotus-with-no-active-minibuffer-if")
    (if (and
         (buffer-live-p buff)
         (not (occ-helm-buffer-p buff)))
        (occ-obj-prop-edit
         (occ-select obj :collector #'occ-list :timeout timeout)
         ctx
         timeout)
      (occ-debug :debug "not running add-ctx-to-org-heading as context buff is deleted or not live 1 %s, 2 %s"
                 (buffer-live-p buff)
                 (not (occ-helm-buffer-p buff))))))


(cl-defmethod occ-delayed-select-obj-prop-edit (obj
                                                (ctx occ-ctx)
                                                timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx-at-point) occ-idle-timeout))
  (occ-debug :debug "begin occ-delayed-select-obj-prop-edit")
  (lotus-with-no-active-minibuffer-if
      (progn
        (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit: [minibuffer-body] lotus-with-no-active-minibuffer-if")
        (occ-debug :debug "add-ctx-to-org-heading: minibuffer already active quitting")
        (occ-debug :debug nil))
    ;;; TODO: extend lotus-with-other-frame-event-debug it to include elscreen change also.
    (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit" :cancel
      (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit: lotus-with-other-frame-event-debug")
      (let ((buff    (occ-ctx-buffer ctx)))
        (if (eq (current-buffer) buff)
            (occ-select-obj-prop-edit obj ctx timeout)
          (occ-debug :debug "context is not for current buffer.")))))
  (occ-debug :debug "finished occ-delayed-select-obj-prop-edit"))


(cl-defmethod occ-delayed-select-obj-prop-edit (obj
                                                (ctx marker)
                                                timeout)
  (occ-delayed-select-obj-prop-edit obj (occ-make-ctx marker)))

(cl-defmethod occ-delayed-select-obj-prop-edit-when-idle (obj
                                                          (ctx occ-ctx)
                                                          timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug :debug "called occ-delayed-select-obj-prop-edit-when-idle")
  (occ-debug :debug "%s: begin: occ-delayed-select-obj-prop-edit-when-idle" (time-stamp-string))
  ;; timed-newwin of occ-delayed-select-obj-prop-edit pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case-control nil nil
    (progn
      ;; TODO: Add code to which check if only focus present than only trigger
      ;; else postpone it by calling run-with-idle-plus-timer
      (lwarn 'occ
             :debug
             "occ-delayed-select-obj-prop-edit-when-idle: calling occ-delayed-select-obj-prop-edit with this-command=%s" this-command)
      (occ-delayed-select-obj-prop-edit obj ctx timeout))

    ;; (lotus-with-other-frame-event-debug "occ-delayed-select-obj-prop-edit-when-idle" :cancel
    ;;   (lwarn 'occ :debug "occ-delayed-select-obj-prop-edit-when-idle: lotus-with-other-frame-event-debug")
    ;;   (occ-delayed-select-obj-prop-edit ctx timeout))

    ((quit)))

  (occ-debug :debug "%s: end: occ-delayed-select-obj-prop-edit-when-idle" (time-stamp-string)))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  occ-idle-timeout nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-delayed-select-obj-prop-edit args)) (list ctx timeout))


;;; occ-interactive.el ends here
