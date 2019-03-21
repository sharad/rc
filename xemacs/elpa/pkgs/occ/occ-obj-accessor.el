;;; occ-obj-accessor.el --- occ-api               -*- lexical-binding: t; -*-
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

(provide 'occ-obj-accessor)


(require 'occ-obj-ctor)


(cl-defmethod occ-ctxual-tsk-marker ((ctxask occ-ctxual-tsk))
  (let* ((tsk    (occ-ctxual-tsk-tsk ctxask))
         (marker (occ-tsk-marker tsk)))
    marker))


(cl-defmethod occ-collect-tsks (collection
                                &optional
                                force)
  (error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

(cl-defmethod occ-collect-tsks ((collection occ-tree-collection)
                                &optional
                                force)
  (unless (occ-tree-collection-tree collection)
    (setf
     (occ-tree-collection-tree collection)
     (occ-tree-tsk-build
      #'(lambda ()
          (or
           ;; (occ-make-tsk-at-point #'make-occ-tree-tsk)
           (occ-make-tsk-at-point (occ-tsk-builder))
           (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
      (car (occ-tree-collection-roots collection)))))

  (occ-tree-collection-tree collection))

(cl-defmethod occ-collect-tsks ((collection occ-list-collection)
                                force)
  (unless (occ-list-collection-list collection)
    (setf
     (occ-list-collection-list collection)
     (remove nil
             (org-map-entries
              #'(lambda ()
                  (or
                   ;; (occ-make-tsk-at-point #'make-occ-list-tsk)
                   (occ-make-tsk-at-point (occ-tsk-builder))
                   (make-occ-list-tsk :name "empty list tsk")))
              t
              (occ-list-collection-roots collection))))))


(cl-defmethod occ-collect-files ((collection occ-tree-collection)
                                 &optional
                                 force)
  (unless (occ-tree-collection-files collection)
    (occ-collect-tsks collection nil)
    (setf
     (occ-tree-collection-files collection)
     (remove nil
             (delete-dups
              (let ((tsks (occ-collection collection))
                    (files '()))
                (occ-mapc-tree-tsks
                 #'(lambda (tsk args)
                     (push (occ-tsk-file tsk) files))
                 tsks
                 nil)
                files)))))
  (occ-tree-collection-files collection))

(cl-defmethod occ-collect-files ((collection occ-list-collection)
                                 &optional
                                 force)
  (unless (occ-list-collection-files collection)
    (setf
     (occ-list-collection-files collection)
     (occ-list-collection-roots collection)))
  (occ-list-collection-files collection))


(cl-defmethod occ-collect-list ((collection occ-tree-collection))
  (unless (occ-tree-collection-list collection)
    (let ((tsks (occ-collection collection))
          (tsk-list '()))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args) (push tsk tsk-list))
       ;; (nconc tsk-list (list tsk))
       ;; (push tsk tsk-list)
       tsks
       nil)
      (setf (occ-tree-collection-list collection)
            (nreverse tsk-list))))
  (occ-tree-collection-list collection))

(cl-defmethod occ-collect-list ((collection occ-list-collection))
  (let ((tsks (occ-collection collection)))
    tsks))



(cl-defmethod occ-collection ((collection occ-tree-collection))
  (unless (occ-tree-collection-tree occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil)
    (run-hooks 'occ-global-tsk-collection-change-hook))
  (occ-tree-collection-tree occ-global-tsk-collection))


(cl-defmethod occ-collection ((collection occ-list-collection))
  (unless (occ-list-collection-list occ-global-tsk-collection)
    (occ-collect-tsks occ-global-tsk-collection nil)
    (run-hooks 'occ-global-tsk-collection-change-hook))
  (occ-list-collection-list occ-global-tsk-collection))

;; (cl-defmethod occ-collection-files ((collection occ-tree-collection))
;;   (unless (occ-tree-collection-files occ-global-tsk-collection)
;;     (occ-collect-files occ-global-tsk-collection nil))
;;   (occ-tree-collection-files occ-global-tsk-collection))


(defun occ-collection-object ()
  (unless occ-global-tsk-collection
    (if occ-global-tsk-collection-spec
        (progn
          (occ-make-tsk-collection occ-global-tsk-collection-spec)
          (occ-collect-tsks occ-global-tsk-collection t))
      (progn
        (message "occ-global-tsk-collection-spec is nil, set using occ-set-global-tsk-collection-spec")
        (error "occ-global-tsk-collection-spec is nil"))))
  occ-global-tsk-collection)

;; (occ-collect-list occ-global-tsk-collection)
;; (occ-tree-collection-list occ-global-tsk-collection)


;;; occ-obj-accessor.el ends here
