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


(cl-defmethod occ-ignore-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and
     (occ-chgable-p)
     buff (buffer-live-p buff)
     (not (minibufferp buff))
     (not (ignore-p buff)))))

(cl-defmethod occ-clockable-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and
     (occ-chgable-p)
     buff (buffer-live-p buff)
     (not (minibufferp buff))
     (not (ignore-p buff)))))

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
  (if (occ-clockable-p obj)
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
                                                    :timeout            timeout)))))
    (occ-debug :debug "ctx %s is not clockable." obj)))


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
    (occ-try-until 3 (not (occ-associable-p ctxtual-tsk))
      (occ-message "occ-try-clock-in %s is not associable with %s [try %d]"
                   (occ-format tsk 'capitalize)
                   (occ-format ctx 'capitalize)
                   (- total-tries try))
      (occ-props-edit ctxtual-tsk))
    (unless (occ-clock-in-if-associable ctxtual-tsk
                                        :filters            filters
                                        :builder            builder
                                        :action             action
                                        :action-transformer action-transformer
                                        :timeout            timeout)
      (occ-message "%s is not associable with %s not clocking-in."
                   (occ-format tsk 'capitalize)
                   (occ-format ctx 'capitalize)))))

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


(cl-defmethod occ-ctxual-current-tsk ((obj occ-ctx))
  (let ((curr-tsk (occ-current-tsk)))
    (when curr-tsk
      (occ-build-ctxual-tsk-with curr-tsk obj))))

(cl-defmethod occ-clock-unassociated-p ((obj occ-ctx))
  (or
   (occ-clock-marker-unnamed-clock-p)
   ;; TODO: BUG: Here provide option to user in case of non-unnamed tsk to
   ;; increase time prop or other prop or continue to other clock. or
   ;; force checkout for clock.
   (not (occ-associable-p (occ-ctxual-current-tsk obj)))))

(cl-defgeneric occ-edit-properties (obj &rest ops))

(cl-defmethod occ-edit-properties ((obj occ-ctxual-tsk)
                                   &rest
                                   ops)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (let ((retval (helm
                   (helm-build-sync-source "edit"
                     :candidates (append
                                  (apply #'occ-gen-params-for-edit tsk ops)
                                  (occ-gen-methods-for-add obj)
                                  '(("Continue" . t)))))))
      (if (eq retval t)
          t
        (prog1
            nil
          (apply #'occ-edit-prop tsk retval))))))

(cl-defmethod occ-edit-until-associable ((obj occ-ctxual-tsk))
  (let ((retval nil))
    (occ-try-until 3 (or (not (eq t retval))
                         (occ-associable-p obj))
      (setq retval
            (occ-edit-properties obj '(timebeing add 10))))
    retval))

(cl-defmethod occ-edit-clock-if-unassociated ((obj occ-ctx))
  (let*  ((curr-tsk        (occ-current-tsk))
          (ctxual-curr-tsk (occ-build-ctxual-tsk-with curr-tsk obj)))
    (if (and
         ctxual-curr-tsk
         (not (occ-associable-p ctxual-curr-tsk)))
        (occ-edit-until-associable ctxual-curr-tsk))))


(cl-defmethod occ-clock-in-if-not ((obj occ-ctx)
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
        (return-transform   t) ;as return value is going to be used.)
        (action             (or action  (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (occ-debug-uncond "occ-clock-in-if-not((obj occ-ctx)): begin")
    (if (occ-clock-unassociated-p obj)
        (prog1                ;current clock is not matching
            t
          (occ-debug :debug
                     "occ-clock-in-if-not: Now really going to clock with this-command=%s"
                     this-command)
          ;; TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm
          (occ-debug :debug
             "TODO: if (occ-current-tsk) is not unnamed than ask confirmation by :auto-select-if-only 'confirm")
          (let ((retval (occ-clock-in obj
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
                  ;; BUG Urgent TODO: SOLVE ASAP ???? at (occ-clock-in-if-not obj) and (occ-clock-in obj)
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
                        (occ-maybe-create-clockedin-unnamed-ctxual-tsk obj))))
              (occ-debug-uncond "occ-clock-in-if-not: Can not operate on %s"
                                (occ-format (occ-return-get-value retval)))))
          (occ-debug :debug "occ-clock-in-if-not: Now really clock done."))
      (prog1
          nil
        (occ-debug :debug
                   "occ-clock-in-if-not: Current tsk already associate to %s"
                   (occ-format obj 'captilize))))))
;; occ-clock-in-if-not


(cl-defmethod occ-consider-for-clockin-in-p ()
  (>
   (float-time (time-since *occ-last-buff-sel-time*))
   *occ-tsk-current-ctx-time-interval*))

(cl-defmethod occ-try-to-clock-in-p ((curr occ-ctx)
                                     (prev occ-ctx))
  (not              ;BUG: Reconsider whether it is catching case after some delay.
   (equal curr prev)))

(defvar occ-clock-in-ctx-auto-select-if-only t)

(cl-defmethod occ-clock-in-if-chg ((obj occ-ctx)
                                   &key
                                   filters
                                   builder
                                   action
                                   action-transformer
                                   auto-select-if-only
                                   timeout)
  (let* ((filters            (or filters (occ-match-filters)))
         (builder            (or builder #'occ-build-ctxual-tsk-with))
         (action             (or action  (occ-helm-actions obj)))
         (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
         (timeout            (or timeout occ-idle-timeout)))
    (occ-debug-uncond "occ-clock-in-if-chg((obj occ-ctx)): begin")
    (if (occ-consider-for-clockin-in-p)
        (progn
          (setq *occ-tsk-current-ctx* obj)

          (if (occ-try-to-clock-in-p obj *occ-tsk-previous-ctx*)
              (when (occ-clock-in-if-not obj
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
              (occ-describe-try-to-clock-in-p *occ-tsk-current-ctx*
                                              *occ-tsk-previous-ctx*))))
      (occ-debug :nodisplay "occ-clock-in-if-chg: not enough time passed."))))
;; occ-clock-in-if-chg


(defvar *occ-last-buff-sel-time*            (current-time) "*occ-last-buff-sel-time*")
(defvar *occ-buff-sel-timer*                nil)
(defvar *occ-tsk-current-ctx-time-interval* 7)
(defvar *occ-tsk-previous-ctx*              nil)
(defvar *occ-tsk-current-ctx*               nil)


(defvar occ-ignore-buffer-names '(" *helm" "*Help*") "occ-ignore-buffer-names")

(defun occ-add-ignore-buffer-names ()
  (interactive)
  (let ((buffname (buffer-name (current-buffer))))
    (push buffname occ-ignore-buffer-names)))

(cl-defmethod occ-describe-try-to-clock-in-p ((curr occ-ctx)
                                              (prev occ-ctx))
  (let ((buff (occ-ctx-buffer curr)))
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
                 ((equal prev curr)
                  (format "context is not changed."))
                 (t (format "Unknown reason.")))))
      (let ((full-msg (format "occ-clock-in-if-chg: ctx %s not suitable to associate as %s"
                              (occ-format curr 'capitalize)
                              msg)))
         ;; (occ-debug :nodisplay full-msg)
        (occ-message full-msg)))))


;;;###autoload
(defun occ-clock-in-curr-ctx (&optional force)
  (interactive "P")
  (let ((ctx (occ-make-ctx-at-point)))
    (let ((filters             (occ-match-filters))
          (builder             #'occ-build-ctxual-tsk-with)
          (action              (occ-helm-actions ctx))
          (action-transformer  #'occ-helm-action-transformer-fun)
          (auto-select-if-only nil) ; occ-clock-in-ctx-auto-select-if-only)
          (timeout             occ-idle-timeout))
      (occ-clock-in-if-not ctx
                           :filters             filters
                           :builder             builder
                           :action              action
                           :action-transformer  action-transformer
                           :auto-select-if-only auto-select-if-only
                           :timeout             timeout))))

;;;###autoload
(defun occ-clock-in-curr-ctx-if-not (&optional force)
  (interactive "P")
  ;; TODO: Add code to which check if only focus present than only trigger
  ;; else postpone it by calling run-with-idle-plus-timer
  (occ-debug :debug "begin occ-clock-in-curr-ctx-if-not")
  (lotus-with-other-frame-event-debug "occ-clock-in-curr-ctx-if-not" :cancel
    (occ-debug :debug "%s: occ-clock-in-curr-ctx-if-not: lotus-with-other-frame-event-debug" (time-stamp-string))
    (if force
        (occ-clock-in-curr-ctx force)
      (let ((ctx (occ-make-ctx-at-point)))
        (let ((filters             (occ-match-filters))
              (builder             #'occ-build-ctxual-tsk-with)
              (action              (occ-helm-actions ctx))
              (action-transformer  #'occ-helm-action-transformer-fun)
              (auto-select-if-only occ-clock-in-ctx-auto-select-if-only)
              (timeout             occ-idle-timeout))
          (occ-clock-in-if-chg ctx
                               :filters             filters
                               :builder             builder
                               :action              action
                               :action-transformer  action-transformer
                               :auto-select-if-only auto-select-if-only
                               :timeout             timeout)))))
  (occ-debug :nodisplay "%s: end occ-clock-in-curr-ctx-if-not" (time-stamp-string)))


(defun occ-clock-in-curr-ctx-if-not-timer-function ()
  (occ-debug-uncond "occ-clock-in-curr-ctx-if-not-timer-function: begin")
  ;; (unwind-protect                       ;BUG: could be the cause of high MEM usage
  ;;     (occ-clock-in-curr-ctx-if-not nil)
  ;;   (occ-run-curr-ctx-timer))
  (occ-clock-in-curr-ctx-if-not nil))

(cl-defmethod occ-try-clock-in-next-timeout ()
  "Get next timeout to try clock-in"
  (occ-debug-uncond "occ-try-clock-in-next-timeout: begin")
  (let* ((ctx             (occ-make-ctx-at-point))
         (ctxual-curr-tsk (occ-ctxual-current-tsk ctx)))
    (1+ *occ-tsk-current-ctx-time-interval*)))


;;;###autoload
(defun occ-run-curr-ctx-timer ()
  (occ-debug-uncond "occ-run-curr-ctx-timer: begin")
  (setq *occ-last-buff-sel-time* (current-time))
  (when *occ-buff-sel-timer*
    (cancel-timer *occ-buff-sel-timer*)
    (setq *occ-buff-sel-timer* nil))
  (setq *occ-buff-sel-timer*
        ;; distrubing while editing.
        ;; run-with-timer
        (run-with-idle-timer
         (occ-try-clock-in-next-timeout)
         nil
         'occ-clock-in-curr-ctx-if-not-timer-function)))


(defun occ-switch-buffer-run-curr-ctx-timer-function (prev next)
  (occ-debug-uncond "occ-switch-buffer-run-curr-ctx-timer-function: begin")
  (occ-run-curr-ctx-timer))

;;; occ-clock.el ends here
