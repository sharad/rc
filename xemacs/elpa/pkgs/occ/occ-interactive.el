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


(cl-defgeneric occ-select-propetry (tsk ctx &optional prompt)
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

(defun org-flag-proprty-drawer-at-marker (marker flag)
  "NIL to open drawer T to close drawer"
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
          (recenter-top-bottom 2)
          (unless flag                  ;; creating issue in cleanupfn error as display buffer and current buffer is not same.
            (recenter-top-bottom 2))
          (let ((prop-range (org-get-property-block (point) 'force)))
            ;; first show heading
            (when (eq org-cycle-subtree-status 'folded)
              (unless flag
                ;;https://lists.gnu.org/archive/html/emacs-orgmode/2015-02/msg00573.html
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
            (let* ((prop-range (org-get-property-block (point) 'force))
                   (prop-loc   (1- (car prop-range))))
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
                prop-range))))))))

(defun org-get-flag-proprty-drawer-at-marker (marker)
  (let ((buff (marker-buffer marker))
        (loc (marker-position marker)))
    (when (and buff loc)
      (with-current-buffer buff
        (when (goto-char loc)
          (let ((range (org-get-property-block (point) 'force)))
            org-cycle-subtree-status))))))

;; (safe-timed-org-refile-get-marker 7)

;; q(defun occ-select-marker)

(cl-defgeneric occ-add-to-heading-internal (ctx timeout)
  "occ-add-to-heading-internal")

(cl-defmethod occ-add-to-heading-internal ((ctx occ-ctx) timeout)
  (let* (;; (marker (safe-timed-org-refile-get-marker timeout))
         (tsk (occ-select nil))
         (mrk (if tsk (occ-tsk-marker tsk))))
    (when mrk
      (lotus-with-marker mrk
       (let* ((marker (point-marker))
              (local-cleanup
               #'(lambda ()
                   (save-excursion ;what to do here
                     (org-flag-proprty-drawer-at-marker marker t))
                   (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                     (abort-recursive-edit)))))

           (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
               timeout timer cleanup local-cleanup win

               (let ((target-buffer (marker-buffer marker))
                     (pos           (marker-position marker)))

                 (when target-buffer
                   (switch-to-buffer target-buffer)
                   (goto-char pos)
                   (set-marker marker (point)))

                 (occ-debug :debug "called add-ctx-to-org-heading %s" (current-buffer))

                 (progn
                   (condition-case-control nil err
                     (let ((buffer-read-only nil))
                       (occ-debug :debug "timer started for win %s" win)

                       ;; show proptery drawer
                       (let* ((prop-range (org-flag-proprty-drawer-at-marker marker nil))
                              (prop-loc   (1- (car prop-range))))
                         (if (numberp prop-loc)
                             (goto-char prop-loc)))

                       ;; try to read values of properties.
                       (let ((prop nil))
                         (while (not
                                 (member
                                  (setq prop (occ-select-propetry (occ-make-tsk nil) ctx))
                                  '(edit done)))
                           (when (occ-editprop prop ctx)
                             (occ-tsk-update-tsks t)))
                         (cond
                          ((eql 'done prop)
                           (funcall cleanup win local-cleanup)
                           (when timer (cancel-timer timer)))
                          ((eql 'edit prop)
                           ;; (funcall cleanup win local-cleanup)
                           (occ-debug :debug "debug editing")
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
                        (signal (car err) (cdr err)))))))))))))

;;;###autoload
(cl-defmethod occ-add-to-org-heading ((ctx occ-ctx) timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx) 7))
  (occ-debug :debug "begin occ-add-to-org-heading")
  (lotus-with-no-active-minibuffer-if
      (progn
        (lwarn 'occ :debug "occ-add-to-org-heading: [minibuffer-body] lotus-with-no-active-minibuffer-if")
        (occ-debug :debug "add-ctx-to-org-heading: minibuffer already active quitting")
        (occ-debug :debug nil))
    (lotus-with-other-frame-event-debug "occ-add-to-org-heading" :cancel
      (lwarn 'occ :debug "occ-add-to-org-heading: lotus-with-other-frame-event-debug")
      (let* ((timeout (or timeout 7))
             (ctx     (or ctx (occ-make-ctx)))
             (buff    (occ-ctx-buffer ctx)))
        (lwarn 'occ :debug "occ-add-to-org-heading: [body] lotus-with-no-active-minibuffer-if")
        (if (and
             (eq (current-buffer) buff)
             (buffer-live-p buff)
             (not
              (eq buff (get-buffer "*helm-mode-occ-add-to-org-heading*"))))
            (occ-add-to-heading-internal ctx timeout)
          (occ-debug :debug "not running add-ctx-to-org-heading as context buff is deleted or not live 1 %s, 2 %s 3 %s"
                     (eq (current-buffer) buff)
                     (buffer-live-p buff)
                     (eq buff
                         (get-buffer "*helm-mode-occ-add-to-org-heading*")))))))
  (occ-debug :debug "finished occ-add-to-org-heading"))

(cl-defmethod occ-add-to-org-heading-when-idle ((ctx occ-ctx) timeout)

  ;; either this should also be in occ-obj-method
  ;; or (cl-defmethod occ-clock-in ((ctx occ-ctx))
  ;;    (defun occ-sacha-helm-select (ctxasks)
  ;; should be here.

  ;; NOTE: presently it is not running on idle time, it simply runs immediately

  "Return value is important to decide next action to (create unnamed tsk.)"
  (occ-debug 6 "called occ-add-to-org-heading-when-idle")
  (occ-debug :debug "%s: begin: occ-add-to-org-heading-when-idle" (time-stamp-string))
  ;; timed-newwin of occ-add-to-org-heading pass quit
  ;; signal to caller mean here, so need to be handled, else this function can
  ;; not return any value to its caller, which result into no next-action in
  ;; caller function.
  (condition-case-control nil nil
    (progn
      ;; TODO: Add code to which check if only focus present than only trigger
      ;; else postpone it by calling run-with-idle-plus-timer
      (lwarn 'occ
             :debug
             "occ-add-to-org-heading-when-idle: calling occ-add-to-org-heading with this-command=%s" this-command)
      (occ-add-to-org-heading ctx timeout))

    ;; (lotus-with-other-frame-event-debug "occ-add-to-org-heading-when-idle" :cancel
    ;;   (lwarn 'occ :debug "occ-add-to-org-heading-when-idle: lotus-with-other-frame-event-debug")
    ;;   (occ-add-to-org-heading ctx timeout))

    ((quit)))

  (occ-debug :debug "%s: end: occ-add-to-org-heading-when-idle" (time-stamp-string)))
  ;; (run-with-idle-timer-nonobtrusive-simple
  ;;  7 nil
  ;;  #'(lambda (args)
  ;;      (apply 'occ-add-to-org-heading args)) (list ctx timeout))


;;; occ-interactive.el ends here
