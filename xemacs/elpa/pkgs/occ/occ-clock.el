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
(require 'occ-config)


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
  (if (occ-config-clock-in)
      (occ-clock-in (occ-obj-marker obj))
    (occ-message "occ-clock-in(obj occ-tsk): clock-in not allowed.")))

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
    (when (and new-marker
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



(cl-defmethod occ-ignore-p ((buff buffer))
  nil)

(cl-defmethod occ-ignore-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and (occ-chgable-p)
         buff
         (buffer-live-p buff)
         (not (minibufferp buff))
         (not (occ-ignore-p buff)))))


(cl-defmethod occ-clockable-p ((obj occ-ctx))
  (let ((buff (occ-ctx-buffer obj)))
    (and (occ-chgable-p)
         buff
         (buffer-live-p buff)
         (not (minibufferp buff))
         (not (occ-ignore-p buff)))))


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
          (action             (or action (occ-get-helm-actions-tree obj '(t actions general))))
          (action-transformer (or action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit))))
          (timeout            (or timeout occ-idle-timeout)))
      (occ-debug :debug "occ-clock-in((obj occ-ctx)): begin")
      (let ((returned-ctxual-tsk
               (occ-select obj ;TODO: if only one match then where it is selecting that.
                           :filters             filters
                           :builder             builder
                           :return-transform    t ;Here I know return value is going to be used, so passing t
                           :action              action ;as return value is going to be used.
                           :action-transformer  action-transformer
                           :auto-select-if-only auto-select-if-only
                           :timeout             timeout)))
        (occ-debug :debug "occ-clock-in((obj occ-ctx)): selected  returned-ctxual-tsk=%s ret-label=%s value=%s"
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
            (occ-debug :debug "occ-clock-in((obj occ-ctx)): calling occ-safe-ignore-quit-props-window-edit")
            (occ-message "occ-clock-in: Edit properties of a tsk to make associable to current context.")
            (occ-safe-ignore-quit-props-window-edit obj
                                                    :filters            (occ-list-filters)
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


(cl-defmethod occ-try-fast-clock-in ((obj marker)
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


(cl-defmethod occ-try-fast-clock-in ((obj occ-ctsk)
                                     &key
                                     filters
                                     builder
                                     action
                                     action-transformer
                                     timeout)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj))
        (ctxtual-tsk (occ-build-ctxual-tsk obj)))
    (occ-try-until 3 (or (not (eq t retval))
                         (occ-associable-p obj))
      ;; (helm (occ-gen-edits-if-required obj nil nil))
      (setq retval
            (occ-edit-properties obj '(timebeing add 10))))
    (unless (occ-clock-in-if-associable ctxtual-tsk
                                        :filters            filters
                                        :builder            builder
                                        :action             action
                                        :action-transformer action-transformer
                                        :timeout            timeout)
      (occ-message "%s is not associable with %s not clocking-in."
                   (occ-format tsk 'capitalize)
                   (occ-format ctx 'capitalize)))))

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

(defun occ-clock-out (&optional switch-to-state
                                fail-quietly
                                at-time)
  (interactive)
  (org-clock-out switch-to-state
                 fail-quietly
                 at-time))

;;; occ-clock.el ends here
