;;; occ-clock.el --- clock                           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <>
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

(provide 'occ-clock)


(require 'occ-obj-utils)
(require 'occ-select)
(require 'occ-prop-edit)


(defvar *occ-clocked-ctxual-tsk-ctx-history* nil)
(defvar occ-clock-in-hooks nil "Hook to run on clockin with previous and next markers.")

(cl-defmethod occ-clock-in ((obj null)
                            &key
                            filters
                            builder
                            action
                            action-transformer
                            timeout)
  (error "Can not clock in NIL"))

(cl-defmethod occ-clock-in ((obj marker)
                            &key
                            filters
                            builder
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(marker=%s)" obj)
  (let ((org-log-note-clock-out nil))
    (when (marker-buffer obj)
      (with-current-buffer (marker-buffer obj)
        (let ((buffer-read-only nil))
          (condition-case-control err
            (progn
              (occ-straight-org-clock-clock-in (list obj)))
            ((error)
             (signal (car err) (cdr err)))))))))

(cl-defmethod occ-clock-in ((obj occ-tsk)
                            &key
                            filters
                            builder
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(occ-tsk=%s)" obj)
  (occ-clock-in (occ-obj-marker obj)))

(cl-defmethod occ-clock-in ((obj occ-ctsk)
                            &key
                            filters
                            builder
                            action
                            action-transformer
                            timeout)
  (occ-debug :debug "occ-clock-in(occ-ctsk=%s)" obj)
  (if (or
       (occ-unnamed-p obj)
       (occ-associable-p obj))
      (occ-clock-in (occ-ctsk-tsk obj)
                    :filters            filters
                    :builder            builder
                    :action             action
                    :action-transformer action-transformer
                    :timeout            timeout)
    (occ-debug :debug
               "occ-clock-in(occ-ctxual-tsk): not clocking in (occ-unnamed-p obj)=%s (occ-associable-p obj)=%s"
               (occ-unnamed-p obj)
               (occ-associable-p obj))))

(cl-defmethod occ-clock-in ((obj occ-ctxual-tsk)
                            &key
                            filters
                            builder
                            action
                            action-transformer
                            timeout)
  ;;TODO add org-insert-log-not
  "return "
  (occ-debug :debug "occ-clock-in(occ-ctxual-tsk=%s)" obj)
  (let* (retval
         (old-ctxual-tsk (car *occ-clocked-ctxual-tsk-ctx-history*))
         (old-tsk        (when old-ctxual-tsk (occ-ctxual-tsk-tsk old-ctxual-tsk)))
         (old-marker     (or (if old-tsk (occ-tsk-marker old-tsk)) org-clock-hd-marker))
         (old-heading    (if old-tsk (occ-tsk-heading old-tsk)))
         (obj-tsk        (occ-ctxual-tsk-tsk obj))
         (obj-ctx        (occ-ctxual-tsk-ctx obj))
         (new-marker     (if obj-tsk (occ-tsk-marker obj-tsk)))
         (new-heading    (if obj-tsk (occ-tsk-heading obj-tsk))))
    (when (and
           new-marker
           (marker-buffer new-marker))

      (let* ((org-log-note-clock-out nil)
             (old-marker             org-clock-marker)
             (old-buff               (marker-buffer old-marker)))

        (occ-debug :debug "clocking in %s" new-marker)

        (let ((old-buff-read-only
               (when old-buff
                 (with-current-buffer (marker-buffer old-marker)
                   buffer-read-only))))

          (when old-buff
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

          ;; (occ-clock-in obj-tsk)
          (if (or
               (occ-unnamed-p obj)
               (occ-associable-p obj))
              (occ-clock-in obj-tsk
                            :filters            filters
                            :builder            builder
                            :action             action
                            :action-transformer action-transformer
                            :timeout            timeout)
            (occ-debug :debug
                       "occ-clock-in(occ-ctxual-tsk): not clocking in (occ-unnamed-p obj)=%s (occ-associable-p obj)=%s"
                       (occ-unnamed-p obj)
                       (occ-associable-p obj)))

          (setq retval t)

          (push obj *occ-clocked-ctxual-tsk-ctx-history*)

          (when old-buff
            (with-current-buffer old-buff
              (setq buffer-read-only old-buff-read-only)))
          retval)))))

(cl-defmethod occ-clock-in ((obj occ-ctx)
                            &key
                            filters
                            builder
                            return-transform
                            action
                            action-transformer
                            auto-select-if-only
                            timeout)
  "Clock-in selected CTXUAL-TSK for occ-ctx OBJ or open interface for adding properties to heading."
  (unless builder (error "Builder can not be nil"))
  (occ-debug :debug "occ-clock-in(occ-ctx=%s)" obj)
  (let ((filters            (or filters (occ-match-filters)))
        (builder            (or builder #'occ-build-ctxual-tsk-with))
        (action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug-uncond "occ-clock-in((obj occ-ctx)): begin")
    (let ((returned-ctxual-tsk
             (occ-select obj ;TODO: if only one match then where it is selecting that.
                         :filters             filters
                         :builder             builder
                         :return-transform    t ;Here I know return value is going to be used, so passing t
                         :action              action ;as return value is going to be used.
                         :action-transformer  action-transformer
                         :auto-select-if-only auto-select-if-only
                         :timeout             timeout)))
      (occ-debug-uncond "occ-clock-in((obj occ-ctx)): selected  returned-ctxual-tsk=%s ret-label=%s value=%s"
                        returned-ctxual-tsk
                        (occ-return-in-labels-p returned-ctxual-tsk occ-return-select-label)
                        (occ-format (occ-return-get-value returned-ctxual-tsk)))
      (if (occ-return-in-labels-p returned-ctxual-tsk ;TODO: should return t if action were done than select[=identity] ;; occ-return-label
                                  occ-return-select-label)
          (let ((ctxual-tsk (occ-return-get-value returned-ctxual-tsk)))
            (prog1
                (when return-transform ;Here caller know if return value is going to be used.
                     (occ-make-return occ-return-true-label nil))
              (if (occ-ctxual-tsk-p ctxual-tsk)
                  (occ-clock-in ctxual-tsk
                                :filters            filters
                                :builder            builder
                                :action             action
                                :action-transformer action-transformer
                                :timeout            timeout)
                (occ-message "%s is not ctxual-tsk" (occ-format ctxual-tsk 'capitalize)))))
        (progn
          ;; here create unnamed tsk, no need
          (setq *occ-update-current-ctx-msg* "null clock")
          (occ-debug :debug
                     "No clock found please set a match for this ctx %s, add it using M-x occ-prop-edit-safe."
                     obj)
          (occ-debug :debug
                     "occ-clock-in(ctx):  with this-command=%s" this-command)
          ;; (occ-delayed-select-obj-prop-edit-when-idle obj obj occ-idle-timeout)
          (occ-debug-uncond "occ-clock-in((obj occ-ctx)): calling occ-safe-ignore-quit-props-window-edit")
          (occ-message "occ-clock-in: Edit properties of a tsk to make associable to current context.")
          (occ-safe-ignore-quit-props-window-edit obj
                                                  :filters            #'occ-list-filters
                                                  :builder            #'occ-build-ctsk-with
                                                  :return-transform   return-transform ;Here caller know if return value is going to be used.
                                                  :action             action
                                                  :action-transformer action-transformer
                                                  :timeout            timeout))))))


;; (cl-defmethod occ-clock-in-if-associable-with ((tsk occ-tsk)
;;                                                (ctx occ-ctx)
;;                                                &key
;;                                                filters
;;                                                builder
;;                                                action
;;                                                action-transformer
;;                                                timeout)
;;   (when (occ-associable-with-p tsk ctx)
;;     (occ-clock-in (occ-build-ctxual-tsk-with tsk ctx)
;;                   :filters            filters
;;                   :builder            builder
;;                   :action             action
;;                   :action-transformer action-transformer
;;                   :timeout            timeout)))

(cl-defmethod occ-clock-in-if-associable ((obj occ-obj-ctx-tsk)
                                          &key
                                          filters
                                          builder
                                          action
                                          action-transformer
                                          timeout)
  (let ((ctxtual-tsk (occ-build-ctxual-tsk obj)))
    (when ctxtual-tsk
      (occ-clock-in ctxtual-tsk
                    :filters            filters
                    :builder            builder
                    :action             action
                    :action-transformer action-transformer
                    :timeout            timeout))))


;; (cl-defmethod occ-try-clock-in-with ((tsk occ-tsk)
;;                                      (ctx occ-ctx)
;;                                      &key
;;                                      filters
;;                                      builder
;;                                      action
;;                                      action-transformer
;;                                      timeout)
;;   (let* ((total-tries 3)
;;          (try         total-tries))
;;     (while (and
;;             (> try 0)
;;             (not (occ-associable-with-p tsk ctx)))
;;       (setq try (1- try))
;;       (occ-message "occ-try-clock-in-with %s is not associable with %s [try %d]"
;;                    (occ-format tsk 'capitalize)
;;                    (occ-format ctx 'capitalize)
;;                    (- total-tries try))
;;       (occ-props-edit-with tsk ctx)))
;;   (unless (occ-clock-in-if-associable-with tsk ctx
;;                                            :filters            filters
;;                                            :builder            builder
;;                                            :action             action
;;                                            :action-transformer action-transformer
;;                                            :timeout            timeout)
;;     (occ-message "%s is not associable with %s not clocking-in."
;;                  (occ-format tsk 'capitalize)
;;                  (occ-format ctx 'capitalize))))


(cl-defmethod occ-try-clock-in ((obj marker)
                                &key
                                filters
                                builder
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj
                :filters            filters
                :builder            builder
                :action             action
                :action-transformer action-transformer
                :timeout            timeout))


(cl-defmethod occ-try-clock-in ((obj occ-obj-tsk)
                                &key
                                filters
                                builder
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj
                :filters            filters
                :builder            builder
                :action             action
                :action-transformer action-transformer
                :timeout            timeout))

;; (cl-defmethod occ-try-clock-in ((obj occ-ctsk)
;;                                 &key
;;                                 filters
;;                                 builder
;;                                 action
;;                                 action-transformer
;;                                 timeout)
;;   (let ((tsk (occ-obj-tsk obj))
;;         (ctx (occ-obj-ctx obj)))
;;     (occ-try-clock-in-with tsk ctx
;;                            :filters            filters
;;                            :builder            builder
;;                            :action             action
;;                            :action-transformer action-transformer
;;                            :timeout            timeout)))

(cl-defmethod occ-try-clock-in ((obj occ-ctsk)
                                &key
                                filters
                                builder
                                action
                                action-transformer
                                timeout)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj))
        (ctxtual-tsk (occ-build-ctxual-tsk obj)))
    (let* ((total-tries 3))
         (try         total-tries))
    (while (and
            (> try 0)
            (not (occ-associable-p ctxtual-tsk)))
      (setq try (1- try))
      (occ-message "occ-try-clock-in %s is not associable with %s [try %d]"
                   (occ-format tsk 'capitalize)
                   (occ-format ctx 'capitalize)
                   (- total-tries try))
      (occ-props-edit ctxtual-tsk)))
  (unless (occ-clock-in-if-associable ctxtual-tsk
                                      :filters            filters
                                      :builder            builder
                                      :action             action
                                      :action-transformer action-transformer
                                      :timeout            timeout)
    (occ-message "%s is not associable with %s not clocking-in."
                 (occ-format tsk 'capitalize)
                 (occ-format ctx 'capitalize))))

(cl-defmethod occ-try-clock-in ((obj null)
                                &key
                                filters
                                builder
                                action
                                action-transformer
                                timeout)
  (occ-clock-in obj
                :filters            filters
                :builder            builder
                :action             action
                :action-transformer action-transformer
                :timeout            timeout))

;; BUG: solve it.
;; Debugger entered--Lisp error: (error "Marker points into wrong buffer" #<marker at 28600 in report.org>)
;;   comment-region-default(#<marker at 28600 in report.org> #<marker (moves after insertion) at 28600 in report.org> nil)
;;   comment-region(#<marker at 28600 in report.org> #<marker (moves after insertion) at 28600 in report.org> nil)
;;   outshine-comment-region(#<marker at 28600 in report.org> #<marker (moves after insertion) at 28600 in report.org>)
;;   ad-Advice-org-store-log-note
;;   apply(ad-Advice-org-store-log-note
;;   org-store-log-note()
;;   org-ctrl-c-ctrl-c(nil)
;;   funcall-interactively(org-ctrl-c-ctrl-c nil)
;;   call-interactively(org-ctrl-c-ctrl-c nil nil)
;;   command-execute(org-ctrl-c-ctrl-c)

(defun occ-clock-out ()
  (error "Implement it."))



(defcustom *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar    *occ-buff-sel-timer*                nil)
(defvar    *occ-tsk-current-ctx-time-interval* 7)
(defvar    *occ-tsk-previous-ctx*              nil)
(defvar    *occ-tsk-current-ctx*               nil)

(cl-defmethod occ-clock-in-if-not ((ctx occ-ctx)
                                   &key
                                   filters
                                   builder
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (unless builder (error "Builder can not be nil"))
  (let ((filters            (or filters (occ-match-filters)))
        (builder            (or builder #'occ-build-ctxual-tsk-with))
        (action             (or action  (occ-helm-actions ctx)))
        (return-transform   t) ;as return value is going to be used.)
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug-uncond "occ-clock-in-if-not((obj occ-ctx)): begin")
    (if (or
         (occ-clock-marker-unnamed-clock-p)
         (not (occ-current-ctxual-tsk ctx)))
        (prog1                ;current clock is not matching
            t
          (occ-debug :debug
                     "occ-clock-in-if-not: Now really going to clock with this-command=%s"
                     this-command)
          ;; TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm
          (occ-debug :debug
             "TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm")
          (let ((retval (occ-clock-in ctx
                                      :filters             filters
                                      :builder             builder
                                      :return-transform    return-transform
                                      :action              action
                                      :action-transformer  action-transformer
                                      :auto-select-if-only auto-select-if-only
                                      :timeout             timeout)))
            (occ-debug-uncond "occ-clock-in-if-not: operate %s retval %s"
                              (occ-return-in-labels-p retval
                                                      occ-return-quit-label
                                                      occ-return-timeout-label)
                              (occ-return-get-value retval))
            (if (occ-return-in-labels-p retval
                                        occ-return-quit-label
                                        occ-return-timeout-label)
                (unless (occ-return-get-value retval)
                  ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not ctx) and (occ-clock-in ctx)
                  ;; begin occ-clock-in-curr-ctx-if-not
                  ;; 2019-03-06 22:55:31 s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug
                  ;; occ-clock-in-if-not: Now really going to clock.
                  ;; in occ-clock-in occ-ctx 1
                  ;; user input 111 retval t
                  ;; trying to create unnamed tsk.
                  ;; occ-maybe-create-unnamed-tsk: Already clockin unnamed tsk
                  ;; occ-clock-in-if-not: Now really clock done.
                  ;; not able to find associated, or intentionally not selecting a clock
                  (if (occ-clock-marker-unnamed-clock-p)
                      (occ-debug :debug "occ-clock-in-if-not: already clock-in into unnamed task ")
                      (progn
                        (occ-debug :debug "trying to create unnamed tsk.")
                        (occ-message "trying to create unnamed tsk.")
                        (occ-maybe-create-clockedin-unnamed-ctxual-tsk ctx))))
              (occ-debug-uncond "occ-clock-in-if-not: Can not operate on %s"
                                (occ-format (occ-return-get-value retval)))))
          (occ-debug :debug "occ-clock-in-if-not: Now really clock done."))
      (prog1
          nil
        (occ-debug :debug
                   "occ-clock-in-if-not: Current tsk already associate to %s"
                   (occ-format ctx 'captilize))))))

(defvar occ-clock-in-ctx-auto-select-if-only t)

(cl-defmethod occ-clock-in-if-chg ((ctx occ-ctx)
                                   &key
                                   filters
                                   builder
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (let* ((filters            (or filters (occ-match-filters)))
         (builder            (or builder #'occ-build-ctxual-tsk-with))
         (action             (or action  (occ-helm-actions ctx)))
         (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
         (timeout            (or timeout occ-idle-timeout)))
    (occ-debug-uncond "occ-clock-in-if-chg((obj occ-ctx)): begin")
    (if (>
         (float-time (time-since *occ-last-buff-sel-time*))
         *occ-tsk-current-ctx-time-interval*)
        (let* ((buff (occ-ctx-buffer ctx)))
          (setq *occ-tsk-current-ctx* ctx)
          (occ-debug-uncond "occ-clock-in-if-chg((obj occ-ctx)): pass1")
          (if (and
               (occ-chgable-p)
               buff (buffer-live-p buff)
               (not (minibufferp buff))
               (not (ignore-p buff))
               (not              ;BUG: Reconsider whether it is catching case after some delay.
                (equal *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)))
              (when (occ-clock-in-if-not ctx
                                         :filters             filters
                                         :builder             builder
                                         :action              action
                                         :action-transformer  action-transformer
                                         :auto-select-if-only auto-select-if-only
                                         :timeout             timeout)
                (occ-debug-uncond "occ-clock-in-if-chg((obj occ-ctx)): calling occ-clock-in-if-not")
                (setq *occ-tsk-previous-ctx* *occ-tsk-current-ctx*))
            (prog1
                nil
              ;; BUG *occ-tsk-previous-ctx* *occ-tsk-current-ctx* not getting
              ;; updated with simple buffer switch as idle tiem occur. IS IT CORRECT OR BUG
              ;; TODO: here describe reason for not trying properly, need to print where necessary.
              (let ((msg (cond
                           ((not (occ-chgable-p))
                            (format "clock is not changeable now."))
                           ((not buff)
                            (format "context buffer is null"))
                           ((not (buffer-live-p buff))
                            (format "context buffer is not live now."))
                           ((minibufferp buff)
                            (format "context buffer is minibuffer."))
                           ((ignore-p buff)
                            (format "context buffer is ignored buffer."))
                           ((equal *occ-tsk-previous-ctx* *occ-tsk-current-ctx*)
                            (format "context is not changed."))
                           (t (format "Unknown reason.")))))
                (let ((full-msg (format "occ-clock-in-if-chg: ctx %s not suitable to associate as %s"
                                        (occ-format ctx 'capitalize)
                                        msg)))
                  ;; (occ-debug :nodisplay full-msg)
                  (occ-message full-msg))))))
      (occ-debug :nodisplay "occ-clock-in-if-chg: not enough time passed."))))

;;; occ-clock.el ends here
