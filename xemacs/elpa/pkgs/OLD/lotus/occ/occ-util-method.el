;;; occ-util-method.el --- Occ util method           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s


;; Author: s <sh4r4d@gmail.com>
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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(provide 'occ-util-method)


(cl-defmethod occ-match-select ((obj occ-obj-ctx)
                                &key
                                obtrusive)
  (let ((filters            (occ-match-filters))
        (builder            #'occ-build-ctxual-tsk-with)
        (action             (occ-get-helm-actions-tree obj '(t actions general edit)))
        (action-transformer #'(lambda (action candidate)
                                (occ-get-helm-actions-tree obj '(t actions general edit))))
        (timeout            occ-idle-timeout))
    (occ-select obj
                :filters            filters
                :builder            builder
                :action             action
                :action-transformer action-transformer
                :timeout            timeout
                :obtrusive          obtrusive)))

(cl-defmethod occ-list-select ((obj occ-obj-ctx)
                               &key
                               obtrusive)
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (action             (occ-get-helm-actions-tree obj '(t actions general edit)))
        (action-transformer #'(lambda (action candidate)
                                (occ-get-helm-actions-tree obj '(t actions general edit))))
        (timeout            occ-idle-timeout))
   (occ-select obj
               :filters            filters
               :builder            builder
               :action             action
               :action-transformer action-transformer
               :timeout            timeout
               :obtrusive          obtrusive)))

(cl-defmethod occ-list-debug-select ((obj occ-obj-ctx)
                                     &key
                                     obtrusive)
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (action             (occ-get-helm-actions-tree obj '(t actions general edit)))
        (return-transform   t)
        (action-transformer #'(lambda (action candidate)
                                (occ-get-helm-actions-tree obj '(t actions general edit))))
        (timeout            occ-idle-timeout))
    (let ((retval-ctx-tsk (occ-select obj
                                      :filters            filters
                                      :builder            builder
                                      :return-transform   return-transform
                                      :action             action
                                      :action-transformer action-transformer
                                      :timeout            timeout
                                      :obtrusive          obtrusive)))
      (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                        retval-ctx-tsk
                        (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                        (occ-return-get-label retval-ctx-tsk))
      (if (and (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label)
               (occ-return-get-value retval-ctx-tsk))
          (let ((ctsk     (occ-return-get-value retval-ctx-tsk))
                (launcher (cdr (assoc (completing-read "Action: " action) action))))
            (funcall launcher ctsk))
        (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))

(cl-defmethod occ-list-launch ((obj occ-obj-ctx)
                               &key
                               obtrusive)
  (let ((filters            (occ-list-filters))
        (builder            #'occ-build-ctsk-with)
        (return-transform   t)
        (action             (occ-get-helm-actions-tree obj '(t actions general edit)))
        (action-transformer (occ-get-helm-actions-tree-genertator obj '(t actions general edit)))
        (timeout            occ-idle-timeout))
    (let ((retval-ctx-tsk (occ-select obj
                                      :filters            filters
                                      :builder            builder
                                      :return-transform   return-transform
                                      :action             action
                                      :action-transformer action-transformer
                                      :timeout            timeout
                                      :obtrusive          obtrusive)))
       (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): selected original: %s, retval: %s with label %s"
                         retval-ctx-tsk
                         (occ-format (occ-return-get-value retval-ctx-tsk) 'capitalize)
                         (occ-return-get-label retval-ctx-tsk))
       (if (and
            (occ-return-in-labels-p retval-ctx-tsk occ-return-select-label)
            (occ-return-get-value retval-ctx-tsk))
           (let* ((action      (occ-get-helm-actions-tree obj '(t actions general edit)))
                  (ctx-tsk     (occ-return-get-value retval-ctx-tsk))
                  (launcher    (cdr (assoc (completing-read "Action: " action) action))))
             (funcall launcher ctx-tsk))
         (occ-debug-uncond "occ-helm-list-debug-select((obj occ-ctx)): No selection")))))

;;; occ-util-method.el ends here
