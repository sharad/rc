;;; occ-deprecated.el --- occ deprecated             -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <spratap@merunetworks.com>
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

(progn                                  ;method

  ;; (cl-defmethod occ-readprop ((tsk-pair (head root))
  ;;                             (ctx occ-ctx))
  ;;   (let* ((file (if ctx (occ-ctx-file ctx)))
  ;;          (dir (if (stringp file) (file-name-directory file) (dirname-of-file file)))
  ;;          (prompt (concat (symbol-name (car tsk-pair)) ": ")))
  ;;     (ido-read-directory-name prompt dir dir)))
  ;; (cl-defmethod occ-readprop ((tsk-pair (head subtree))
  ;;                             (ctx occ-ctx))
  ;;   (let ((prompt (concat (symbol-name (car tsk-pair)) ": ")))
  ;;     (file-relative-name
  ;;      (ido-read-file-name ;; org-iread-file-name
  ;;       prompt
  ;;       default-directory default-directory))))
  ;; (cl-defmethod occ-writeprop ((tsk-pair (head subtree))))

  ;; deprecated
  ;; (cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
  ;;   "Insert a line for the clock selection menu.
  ;; And return a cons cell with the selection character integer and the marker
  ;; pointing to it."
  ;;   (let ((marker (occ-ctxual-tsk-marker ctxask))
  ;;         (rank   (occ-ctxual-tsk-rank   ctxask)))
  ;;     (when (marker-buffer marker)
  ;;       (with-current-buffer (org-base-buffer (marker-buffer marker))
  ;;         (org-with-wide-buffer
  ;;          (progn ;; ignore-errors
  ;;            (goto-char marker)
  ;;            (let* ((cat (org-get-category))
  ;;                   (heading (org-get-heading 'notags))
  ;;                   (prefix (save-excursion
  ;;                             (org-back-to-heading t)
  ;;                             (looking-at org-outline-regexp)
  ;;                             (match-string 0)))
  ;;                   (tsk (substring
  ;;                         (org-fontify-like-in-org-mode
  ;;                          (concat prefix heading)
  ;;                          org-odd-levels-only)
  ;;                         (length prefix))))
  ;;              (when tsk ;; (and cat tsk)
  ;;                ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
  ;;                ;; marker
  ;;                (cons (occ-print ctxask) ctxask))))))))) ;TODO

  (cl-defmethod occ-clock-in ((ctx occ-ctx))
    "marker and ranked version"
    (interactive
     (list (occ-make-ctx-at-point)))
    (progn
      (occ-debug :debug "in occ-clock-in occ-ctx 1")
      (let* ((ctx (or ctx (occ-make-ctx-at-point)))
             (matched-ctxual-tsks
              (run-unobtrusively           ;heavy task

                ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)

                ;; begin occ-clock-in-curr-ctx-if-not
                ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
                ;; occ-clock-in-if-not: Now really going to clock.
                ;; in occ-clock-in occ-ctx 1
                ;; user input 111 retval t
                ;; trying to create unnamed tsk.
                ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
                ;; occ-clock-in-if-not: Now really clock done.

                (remove-if-not
                 #'(lambda (ctxual-tsk)
                     (let* ((marker (occ-ctxual-tsk-marker ctxual-tsk)))
                       (and
                        marker
                        (marker-buffer marker))))
                 (occ-matching-ctxual-tsks (occ-collection-object) ctx)))))
        (unless (eq matched-ctxual-tsks t)
          (if matched-ctxual-tsks
              (let* ((sel-ctxual-tsk
                      (if (> (length matched-ctxual-tsks) 1)
                          (occ-sacha-helm-select-timed matched-ctxual-tsks)
                        (car matched-ctxual-tsks))))
                ;; (sel-tsk   (if sel-ctxual-tsk (plist-get sel-ctxual-tsk :tsk)))
                ;; (sel-marker (if sel-tsk      (plist-get sel-tsk      :tsk-clock-marker)))

                ;; (occ-debug :debug "sel-ctxual-tsk %s sel-tsk %s sel-marker %s" sel-ctxual-tsk sel-tsk sel-marker)
                (if sel-ctxual-tsk (occ-clock-in sel-ctxual-tsk)))
            (progn
              ;; here create unnamed tsk, no need
              (setq *occ-update-current-ctx-msg* "null clock")
              (occ-debug :debug
                         "No clock found please set a match for this ctx %s, add it using M-x occ-add-to-org-heading."
                         ctx)
              (occ-add-to-org-heading-when-idle ctx 7)
              nil))))))

  (cl-defmethod occ-clock-in ((new-ctxask occ-ctxual-tsk))
    ;;TODO add org-insert-log-not
    (occ-debug :debug "occ-clock-in-marker %s" new-ctxask)
    (let* (retval
           (old-ctxual-tsk     (car *occ-clocked-ctxual-tsk-ctx-history*))
           (old-tsk            (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
           (old-marker         (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
           (old-heading        (if old-tsk (occ-tsk-heading old-tsk)))
           (new-tsk            (occ-ctxual-tsk-tsk new-ctxask))
           (new-marker         (if new-tsk (occ-tsk-marker new-tsk)))
           (new-heading        (if new-tsk (occ-tsk-heading new-tsk))))
      (when (and
             new-marker
             (marker-buffer new-marker))

        (let* ((org-log-note-clock-out nil)
               (old-marker org-clock-marker)
               (old-buff   (marker-buffer old-marker)))

          (occ-debug :debug "clocking in %s" new-marker)

          (let ((old-buff-read-only
                 (if old-buff
                     (with-current-buffer (marker-buffer old-marker)
                       buffer-read-only))))

            (if old-buff
                (with-current-buffer old-buff
                  (setq buffer-read-only nil)))

            (setq *occ-update-current-ctx-msg* old-marker)

            (run-hook-with-args 'occ-clock-in-hooks
                                old-marker
                                new-marker)

            (when (and
                   new-heading
                   old-marker
                   (marker-buffer old-marker))
              (org-insert-log-note old-marker (format "clocking out to clockin to <%s>" new-heading)))

            (with-current-buffer (marker-buffer new-marker)
              (let ((buffer-read-only nil))
                (when old-heading
                  (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))
                (condition-case-control t err
                  (progn
                    (occ-straight-org-clock-clock-in (list new-marker))
                    (setq retval t)
                    (push new-ctxask *occ-clocked-ctxual-tsk-ctx-history*))
                  ((error)
                   (progn
                     (setq retval nil)
                     (signal (car err) (cdr err)))))))

            (if old-buff
                (with-current-buffer old-buff
                  (setq buffer-read-only old-buff-read-only)))
            retval)))))






  (cl-defgeneric occ-sacha-selection-line (obj))


  (cl-defmethod occ-sacha-selection-line ((mrk marker))
    "Insert a line for the clock selection menu.
  And return a cons cell with the selection character integer and the mrk
  pointing to it."
    (when (mrk-buffer mrk)
      (with-current-buffer (org-base-buffer (mrk-buffer mrk))
        (org-with-wide-buffer
         (progn ;; ignore-errors
           (goto-char mrk)
           (let* ((cat (org-get-category))
                  (heading (org-get-heading 'notags))
                  (prefix (save-excursion
                            (org-back-to-heading t)
                            (looking-at org-outline-regexp)
                            (match-string 0)))
                  (tsk (substring
                        (org-fontify-like-in-org-mode
                         (concat prefix heading)
                         org-odd-levels-only)
                        (length prefix))))
             (when tsk ;; (and cat tsk)
               ;; (insert (format "[%c] %-12s  %s\n" i cat tsk))
               ;; mrk
               (cons tsk mrk))))))))

  (cl-defmethod occ-sacha-selection-line ((tsk occ-tsk))
    "Insert a line for the clock selection menu.
  And return a cons cell with the selection character integer and the marker
  pointing to it."
    (cons (occ-print tsk) tsk))

  (cl-defmethod occ-sacha-selection-line ((ctxask occ-ctxual-tsk))
    "Insert a line for the clock selection menu.
  And return a cons cell with the selection character integer and the marker
  pointing to it."
    (cons (occ-print ctxask) ctxask))

  (defun occ-sacha-helm-select (ctxasks)
    ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
    (occ-debug :debug "Running occ-sacha-helm-select")
    (helm
     (list
      (helm-build-sync-source "Select matching tsks"
        :candidates (mapcar 'occ-sacha-selection-line ctxasks)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'identity))
        :history 'org-refile-history))))
  ;; (helm-build-dummy-source "Create tsk"
  ;;   :action (helm-make-actions
  ;;            "Create tsk"
  ;;            'sacha/helm-org-create-tsk))


  (defun occ-sacha-helm-select-timed (ctxasks)
    (helm-timed 7
      (occ-debug :debug "running sacha/helm-select-clock")
      (occ-sacha-helm-select ctxasks))))



(cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
  "occ-sacha-helm-action")

(cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  ;; (setq sacha/helm-org-refile-locations tbl)
  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching tsks"
        :candidates (mapcar 'occ-sacha-selection-line ctxask)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
        :history 'org-refile-history)))))
;; (helm-build-dummy-source "Create tsk"
;;   :action (helm-make-actions
;;            "Create tsk"
;;            'sacha/helm-org-create-tsk))




;; (defun org-get-property (prop-key)
;;   (org-entry-get nil prop-key))

;; (defun occ-get-property (prop-key)
;;   (org-get-property prop-key))

;; (defun occ-set-property (prop-key value ctx &rest args)
;;   (let ((prop-key-str (if (eq (elt prop-key 0 ) ?\:) (substring prop-key 1))))
;;     (org-set-property prop-key
;;                       (if value
;;                           value
;;                           (funcall
;;                            (occ-key-fun prop-key :getter)
;;                            prop-key nil ctx args))))
;;   t)

;; (eq (elt ":root" 0) ?\:)

;; (occ-select-propetry nil)

;; (occ-keys-with-operation :getter nil)

;; (occ-set-property (intern ":root") nil (list :file "/home/s/paradise/git/main/src/wnc/security/authenticator/ieee802_1x.cpp" :buffer (get-buffer "ieee802_1x.cpp")))






;; occ-interactive.el



(defun org-flag-proprty-drawer-at-marker (marker flag)
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
          (recenter-top-bottom 2)
          (unless flag                  ;; creating issue in cleanupfn error as display buffer and current buffer is not same.
            (recenter-top-bottom 2))
          (let ((prop-range (org-get-property-block (point) 'force)))
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


(cl-defmethod occ-add-to-heading-internal ((ctx occ-ctx) timeout)
  (let* (;; (marker (safe-timed-org-refile-get-marker timeout))
         (tsk (occ-select nil #'occ-list))
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


(cl-defmethod occ-add-to-org-heading ((ctx occ-ctx) timeout)
  "add-ctx-to-org-heading"
  ;; TODO: make helm conditional when it is used than only it should be handled.
  (interactive '((occ-make-ctx-at-point) 7))
  (occ-debug :debug "begin occ-add-to-org-heading")
  (lotus-with-no-active-minibuffer-if
      (progn
        (lwarn 'occ :debug "occ-add-to-org-heading: [minibuffer-body] lotus-with-no-active-minibuffer-if")
        (occ-debug :debug "add-ctx-to-org-heading: minibuffer already active quitting")
        (occ-debug :debug nil))
    (lotus-with-other-frame-event-debug "occ-add-to-org-heading" :cancel
      (lwarn 'occ :debug "occ-add-to-org-heading: lotus-with-other-frame-event-debug")
      (let* ((timeout (or timeout 7))
             (ctx     (or ctx (occ-make-ctx-at-point)))
             (buff    (occ-ctx-buffer ctx)))
        (lwarn 'occ :debug "occ-add-to-org-heading: [body] lotus-with-no-active-minibuffer-if")
        (if (and
             (eq (current-buffer) buff)
             (buffer-live-p buff)
             (not
              (eq buff
                  (get-buffer "*helm-mode-occ-add-to-org-heading*"))))

            (org-with-file-loc-timed-refile
                file pos                                               ;;TODO: ASAP optional to generated helm-source helm-sync-source DOIT
                timeout '((occ-files :maxlevel . 4)) "Refile" ;heavy task, but present in macro !

              (let* ((marker (point-marker))
                     (local-cleanup
                      #'(lambda ()
                          (save-excursion ;what to do here
                            (org-flag-proprty-drawer-at-marker marker t))
                          (when (active-minibuffer-window) ;required here, this function itself using minibuffer via helm-refile and occ-select-propetry
                            (abort-recursive-edit)))))

                ;; (set-marker marker (point))
                ;; (occ-debug :debug "1 marker %s" marker)

                (lotus-with-timed-new-win ;break it in two macro call to accommodate local-cleanup
                    timeout timer cleanup local-cleanup win

                    (let ((target-buffer (find-file-noselect file)))

                      (when target-buffer
                        (switch-to-buffer target-buffer)
                        (goto-char pos)
                        (set-marker marker (point)))
                      ;; (occ-debug :debug "2 marker %s" marker)

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
                             (signal (car err) (cdr err))))))))))
          (progn
            (occ-debug :debug "not running add-ctx-to-org-heading 1 %s, 2 %s 3 %s"
                       (eq (current-buffer) buff)
                       (buffer-live-p buff)
                       (eq buff
                           (get-buffer "*helm-mode-occ-add-to-org-heading*"))))))))
  (occ-debug :debug "finished occ-add-to-org-heading"))
;; occ-interactive.el


;; occ-simple
(cl-defmethod occ-select ((obj occ-ctx) collector &optional timeout)
  "return interactively selected CTXUAL-TSK or NIL, marker and ranked version"
  (interactive
   (list (occ-make-ctx-at-point)))
  (progn
    (occ-debug :debug "in occ-clock-in occ-ctx 1")
    (let* ((obj (or obj (occ-make-ctx)))
           (matched-ctxual-tsks
            (run-unobtrusively           ;heavy task

              ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)

              ;; begin occ-clock-in-curr-ctx-if-not
              ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
              ;; occ-clock-in-if-not: Now really going to clock.
              ;; in occ-clock-in occ-ctx 1
              ;; user input 111 retval t
              ;; trying to create unnamed tsk.
              ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
              ;; occ-clock-in-if-not: Now really clock done.

              (remove-if-not
               #'(lambda (ctxual-tsk)
                   (let* ((mrk (occ-ctxual-tsk-marker ctxual-tsk)))
                     (and
                      mrk
                      (marker-buffer mrk))))
               (funcall collector obj)))))
      (unless (eq matched-ctxual-tsks t)
        (when matched-ctxual-tsks
          (let* ((sel-ctxual-tsk
                  (if (> (length matched-ctxual-tsks) 1)
                      (occ-list-select matched-ctxual-tsks (occ-helm-actions obj) timeout)
                    (car matched-ctxual-tsks))))
            sel-ctxual-tsk))))))
;; occ-simple

(provide 'occ-deprecated)
;;; occ-deprecated.el ends here
