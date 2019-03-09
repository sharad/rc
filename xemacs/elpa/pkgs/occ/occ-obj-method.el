;;; occ-obj-method.el --- occ-api               -*- lexical-binding: t; -*-
;; Copyright (C) 2016  Sharad Pratap

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

(provide 'occ-obj-method)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-prop)
(require 'occ-obj-simple)


;; rank based
(defun occ-sacha-helm-select (ctxasks)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (message "Running occ-sacha-helm-select")
  (helm
   (list
    (helm-build-sync-source "Select matching tsks"
      :candidates (mapcar 'occ-sacha-selection-line ctxasks)
      :action (list ;; (cons "Select" 'identity)
               (cons "Clock in and track" #'identity))
      :history 'org-refile-history)
    ;; (helm-build-dummy-source "Create tsk"
    ;;   :action (helm-make-actions
    ;;            "Create tsk"
    ;;            'sacha/helm-org-create-tsk))
    )))

(defun occ-sacha-helm-select-timed (ctxasks)
  (helm-timed 7
    (message "running sacha/helm-select-clock")
    (occ-sacha-helm-select ctxasks)))

(cl-defgeneric occ-sacha-helm-action (ctxask clockin-fn)
  "occ-sacha-helm-action")

(cl-defmethod occ-sacha-helm-action ((ctxask occ-ctxual-tsk) clockin-fn)
  ;; (message "sacha marker %s" (car dyntskpls))
  ;; (setq sacha/helm-org-refile-locations tbl)
  (progn
    (helm
     (list
      (helm-build-sync-source "Select matching tsks"
        :candidates (mapcar 'occ-sacha-selection-line ctxask)
        :action (list ;; (cons "Select" 'identity)
                 (cons "Clock in and track" #'(lambda (c) (funcall clockin-fn c))))
        :history 'org-refile-history)
      ;; (helm-build-dummy-source "Create tsk"
      ;;   :action (helm-make-actions
      ;;            "Create tsk"
      ;;            'sacha/helm-org-create-tsk))
      ))))


(cl-defgeneric occ-rank (obj ctx)
  "occ-rank"
  )

(cl-defmethod occ-rank (tsk-pair ctx)
  0)

;; ISSUE? should it return rank or occ-ctxual-tsk
(cl-defmethod occ-rank ((tsk occ-tsk)
                        (ctx occ-ctx))
  (let ((rank
         (reduce #'+
                 (mapcar
                  #'(lambda (slot)
                      (occ-rank (cons slot tsk) ctx)) ;TODO: check if method exist or not, or use some default method.
                  (occ-class-slots tsk)))))
    rank))


(cl-defmethod occ-included-files ()
  (occ-collection-included-files (occ-collection-object)))


;; ISSUE? should it return rank or occ-ctxual-tsks list
(cl-defmethod occ-matching-ctxual-tsks ((collection occ-list-tsk-collection)
                                        (ctx occ-ctx))
  ;; (message "occ-matching-ctxual-tsks list")
  (let ((tsks (occ-collection collection))
        (ctx ctx))
    (remove-if-not
     #'(lambda (ctxual-tsk)
         (> (occ-ctxual-tsk-rank ctxual-tsk) 0))
     (mapcar
      #'(lambda (tsk)
          (occ-build-ctxual-tsk tsk ctx))
      tsks))))

;; ISSUE? should it return rank or occ-ctxual-tsks map
(cl-defmethod occ-matching-ctxual-tsks ((collection occ-tree-tsk-collection)
                                        (ctx occ-ctx))
  ;; (message "occ-matching-ctxual-tsks tree")
  (let ((tsks (occ-collection collection))
        (matched '()))
    (when tsks
      (occ-debug :debug "occ-matching-ctxual-tsks BEFORE matched %s[%d]" matched (length matched))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args)
           ;; (occ-debug :debug "occ-rank heading = %s" (occ-tsk-heading tsk))
           (let* ((ctxual-tsk (occ-build-ctxual-tsk tsk args))
                  (rank (occ-ctxual-tsk-rank ctxual-tsk)))
             (unless rank (error "occ-entries-associated-to-ctx-by-keys[lambda]: rank is null"))
             (when (> (occ-ctxual-tsk-rank ctxual-tsk) 0)
               (push ctxual-tsk matched)
               (occ-debug :debug "occ-entries-associated-to-ctx-by-keys[lambda]: tsk %s MATCHED RANK %d"
                          (occ-tsk-heading tsk)
                          (length matched)))))
       tsks
       ctx))
    (occ-debug :debug "occ-entries-associated-to-ctx-by-keys: AFTER matched %s[%d]" "matched" (length matched))
    matched))

;;TODO: make it after method
(cl-defmethod occ-matching-ctxual-tsks :around

  ((collection occ-tsk-collection)
   (ctx occ-ctx)) ;TODO: make it after method

  (if (occ-collection-object)
      (let* ((ctxual-tsks (cl-call-next-method))
             (rankslist  (mapcar
                          #'(lambda (ctxual-tsk)
                              (occ-ctxual-tsk-rank ctxual-tsk))
                          ctxual-tsks))
             (avgrank    (if (= 0 (length rankslist))
                             0
                           (/
                            (reduce #'+ rankslist)
                            (length rankslist))))
             (varirank   (if (= 0 (length rankslist))
                             0
                           (sqrt
                            (/
                             (reduce #'+
                                     (mapcar #'(lambda (rank) (expt (- rank avgrank) 2)) rankslist))
                             (length rankslist))))))
        ;; (message "occ-matching-ctxual-tsks :around finish")
        (occ-debug :debug "matched ctxtsks %s" (length ctxual-tsks))
        (remove-if-not
         #'(lambda (ctxual-tsk)
             (>= (occ-ctxual-tsk-rank ctxual-tsk) avgrank))
         ctxual-tsks))
    (error "(occ-collection-object) returned nil")))


(cl-defmethod occ-list ((collection occ-tree-tsk-collection)
                        (ctx occ-ctx))
  "return CTXUAL-TSKs list"
  (occ-matching-ctxual-tsks collection ctx))

(cl-defmethod occ-list ((collection occ-tree-tsk-collection)
                        (ctx null))
  "return TSKs list"
  (occ-collect-tsk-list collection))

;; (occ-list (occ-collection-object) nil)


;;;###autoload
(defun occ-helm-select-tsk (selector
                            action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car tsks))
  (let ()
    (let ((tsks
           (occ-list (occ-collection-object) nil)))
      (push
       (helm-build-sync-source "Select tsk"
         :candidates (mapcar
                      'occ-sacha-selection-line
                      tsks)
         :action (list
                  (cons "Clock in and track" selector))
         :history 'org-refile-history)
       helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-sacha-selection-line (occ-current-tsk)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))

;;;###autoload
(defun occ-helm-select-ctxual-tsk (selector
                                   action)
  ;; here
  ;; (occ-debug :debug "sacha marker %s" (car ctxasks))
  (let (helm-sources
        (ctx (occ-make-ctx)))
    (let ((ctxasks
           (occ-list (occ-collection-object) ctx)))
      (push
       (helm-build-sync-source "Select matching tsk"
         :candidates (mapcar
                      'occ-sacha-selection-line
                      ctxasks)
         :action (list
                  (cons "Clock in and track" selector))
         :history 'org-refile-history)
       helm-sources))

    (when (and
           (org-clocking-p)
           (marker-buffer org-clock-marker))
      (push
       (helm-build-sync-source "Current Clocking Tsk"
         :candidates (list (occ-sacha-selection-line
                            (occ-build-ctxual-tsk (occ-current-tsk) ctx)))
         :action (list
                  (cons "Clock in and track" selector)))
       helm-sources))

    (funcall action (helm helm-sources))))


;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (error "Window #<window 12> too small for splitting")

(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")

(cl-defmethod occ-select ((ctx occ-ctx))
  "return CTXUAL-TSK or NIL, marker and ranked version"
  (interactive
   (list (occ-make-ctx)))
  (progn
    (message "in occ-clock-in occ-ctx 1")
    (let* ((ctx (or ctx (occ-make-ctx)))
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
               (occ-list (occ-collection-object) ctx)))))
      (unless (eq matched-ctxual-tsks t)
        (when matched-ctxual-tsks
          (let* ((sel-ctxual-tsk
                  (if (> (length matched-ctxual-tsks) 1)
                      (occ-sacha-helm-select-timed matched-ctxual-tsks)
                    (car matched-ctxual-tsks))))
            ;; (sel-tsk   (if sel-ctxual-tsk (plist-get sel-ctxual-tsk :tsk)))
            ;; (sel-marker (if sel-tsk      (plist-get sel-tsk      :tsk-clock-marker)))
            ;; (occ-debug :debug "sel-ctxual-tsk %s sel-tsk %s sel-marker %s" sel-ctxual-tsk sel-tsk sel-marker)
            sel-ctxual-tsk))))))


(cl-defmethod occ-clock-in ((mrk marker))
  (let ((org-log-note-clock-out nil))
    (when (marker-buffer new-marker)
      (with-current-buffer (marker-buffer mrk)
        (let ((buffer-read-only nil))
          (condition-case-control t err
            (progn
              (occ-straight-org-clock-clock-in (list mrk)))
            ((error)
             (progn
               (setq retval nil)
               (signal (car err) (cdr err))))))))))

(cl-defmethod occ-clock-in ((tsk occ-tsk))
  (occ-clock-in (occ-tsk-marker tsk)))

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
          (when old-heading
            (org-insert-log-note new-marker (format "clocking in to here from last clock <%s>" old-heading)))

          (occ-clock-in new-tsk)
          (setq retval t)

          (push new-ctxask *occ-clocked-ctxual-tsk-ctx-history*)


          (if old-buff
              (with-current-buffer old-buff
                (setq buffer-read-only old-buff-read-only)))
          retval)))))

(cl-defmethod occ-clock-in ((ctx occ-ctx))
  (let ((tsk (occ-select ctx)))
    (if tsk
        (occ-clock-in tsk)
      (progn
        ;; here create unnamed tsk, no need
        (setq *occ-update-current-ctx-msg* "null clock")
        (occ-debug nil
                   "No clock found please set a match for this ctx %s, add it using M-x occ-add-to-org-heading."
                   ctx)
        (occ-add-to-org-heading-when-idle ctx 7)
        nil))))

(cl-defmethod occ-clock-in ((ctx null))
  (error "Can not clock in NIL"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Errors

;; Mark set
;; begin occ-clock-in-curr-ctx-if-not
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; occ-clock-in-if-chg: ctx [cl-struct-occ-ctx *Messages* *Messages* nil] not suitable to associate
;; end occ-clock-in-curr-ctx-if-not
;; begin occ-clock-in-curr-ctx-if-not
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; occ-clock-in-if-not: Now really going to clock.
;; in occ-clock-in occ-ctx 1
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (wrong-type-argument symbolp ((closure ((start-file . "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/start.org") (party-base-dir . "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/") t) nil (setq org-agenda-files (occ-included-files)))))
;; Mark set
;; Mark saved where search started
;; Mark set
;; begin occ-clock-in-curr-ctx-if-not
;; readfn: occ-clock-in-curr-ctx-if-not inside readfn
;; occ-clock-in-if-not: Now really going to clock.
;; in occ-clock-in occ-ctx 1
;; occ-matching-ctxual-tsks BEFORE matched nil[0]
;; tsk empty heading subtree t
;; tsk empty heading root nil not present.

;;; occ-obj-method.el ends here
