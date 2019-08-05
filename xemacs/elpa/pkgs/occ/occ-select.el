;;; occ-select.el --- selection                      -*- lexical-binding: t; -*-

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

(provide 'occ-select)


(require 'org-misc-utils-lotus)


(require 'occ-list-filter)
(require 'occ-helm)


(cl-defmethod occ-list-select-internal ((obj occ-ctx)
                                        &key
                                        filters
                                        builder
                                        action
                                        action-transformer
                                        auto-select-if-only
                                        timeout)
  ;; (occ-debug :debug "sacha marker %s" (car dyntskpls))
  (lotus-with-no-active-minibuffer-if
      (progn
        (occ-debug :debug "occ-list-select-internal: [minibuffer-body] lotus-with-no-active-minibuffer-if")
        (occ-debug :debug "occ-list-select-internal: minibuffer already active quitting")
        (occ-debug :debug nil))
    (occ-debug :debug "Running occ-list-select-internal")
    (prog1
        (let ((action-transformer  (or action-transformer #'occ-helm-action-transformer-fun))
              (timeout             (or timeout occ-idle-timeout)))

          (let* ((candidates-unfiltered (occ-list obj :builder builder))
                 (unfiltered-count      (length candidates-unfiltered))
                 (candidates-filtered   (occ-filter obj
                                                    filters
                                                    candidates-unfiltered)))
            (when candidates-filtered
              (if (and
                   auto-select-if-only
                   (= 1 (length candidates-filtered)))
                  (let* ((candidate (car candidates-filtered))
                         (action    (car (funcall action-transformer action candidate)))
                         (action    (if (consp action) (cdr action) action)))
                    (funcall action candidate))
                (helm
                 ;; :keymap occ-helm-map
                 :sources
                 (occ-helm-build-candidates-sources obj
                                                    candidates-filtered
                                                    :unfiltered-count   unfiltered-count
                                                    :filters            filters
                                                    :builder            builder
                                                    :action             action
                                                    :action-transformer action-transformer))))))
      (occ-debug :debug "Running occ-list-select-internal"))))

(cl-defmethod occ-list-select ((obj occ-ctx)
                               &key
                               filters
                               builder
                               return-transform
                               action
                               action-transformer
                               auto-select-if-only
                               timeout)
  (let ((action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
      (helm-timed timeout
        (occ-debug :debug "running occ-list-select")
        (let ((action             (if return-transform (occ-return-tranform action) action)) ;as return value is going to be used.
              (action-transformer (if return-transform (occ-return-tranformer-fun-transform action-transformer) action-transformer)))
          (let ((selected (occ-list-select-internal obj
                                                    :filters             filters
                                                    :builder             builder
                                                    :action              action
                                                    :action-transformer  action-transformer
                                                    :auto-select-if-only auto-select-if-only
                                                    :timeout             timeout)))
           (occ-debug :debug "occ-list-select: selected = %s" selected)
           (if return-transform
               (or ;as return value is going to be used.
                selected
                (occ-make-return occ-return-quit-label selected))
             selected))))))


;; TODO: Not to run when frame is not open [visible.]
;; Getting targets...done
;; Error running timer: (error "Window #<window 12> too small for splitting")
;; task-projbuffs-base-dir: changing supplied base-dir nil and task-projbuffs-base-dir to /home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/
;; in occ-clock-in occ-ctx 1
;; Getting targets...done
;; Error running timer ‘occ-clock-in-curr-ctx-if-not’: (error "Window #<window 12> too small for splitting")

(cl-defmethod occ-select ((obj occ-ctx)
                          &key
                          filters
                          builder
                          return-transform
                          action
                          action-transformer
                          auto-select-if-only
                          timeout)
  "return interactively selected TSK or NIL"
  (unless builder (error "Builder can not be nil"))
  (occ-debug :debug "occ-select((obj occ-ctx)): begin")
  (let ((action             (or action (occ-helm-actions obj)))
        (action-transformer (or action-transformer #'occ-helm-action-transformer-fun))
        (timeout            (or timeout occ-idle-timeout)))
    (let* ((unfiltered-count      (occ-length)))
      (if (> unfiltered-count 0)
          (let ((retval (occ-list-select obj
                                         :filters             filters
                                         :builder             builder
                                         :return-transform    return-transform
                                         :action              action
                                         :action-transformer  action-transformer
                                         :auto-select-if-only auto-select-if-only
                                         :timeout             timeout)))
            (occ-debug :debug "occ-select((obj occ-ctx)): occ-list-select returned %s"
                              (occ-format retval 'capitalize))
            retval)
        (prog1
            (when return-transform
              (occ-make-return occ-return-nocndidate-label nil))
          (occ-message "occ-select((obj occ-ctx)): no candidates available from %d."
                       unfiltered-count))))))

(cl-defmethod occ-select ((obj null)
                          &key
                          filters
                          builder
                          return-transform
                          action
                          action-transformer
                          auto-select-if-only
                          timeout)
  (occ-debug :debug "occ-select((obj null)): begin")
  (let ((retval (occ-select (occ-make-ctx-at-point)
                            :filters             filters
                            :builder             builder
                            :return-transform    return-transform
                            :action              action
                            :action-transformer  action-transformer
                            :auto-select-if-only auto-select-if-only
                            :timeout             timeout)))
    (occ-debug :debug "occ-select((obj null)): occ-select((obj occ-ctx)) returned %s"
                      (occ-format retval 'capitalize))
    retval))

;;; occ-select.el ends here
