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


(require 'occ-print)
(require 'occ-obj-ctor)
(require 'occ-rank)


(cl-defmethod occ-class-name (obj)
  "unknown")

(cl-defmethod occ-class-name ((obj symbol))
  "symbol")

(cl-defmethod occ-class-name ((obj null))
  "null")

(cl-defmethod occ-class-name ((obj marker))
  "marker")

(cl-defmethod occ-class-name ((obj occ-tsk))
  "task")

(cl-defmethod occ-class-name ((obj occ-ctsk))
  "context task")

(cl-defmethod occ-class-name ((obj occ-ctxual-tsk))
  "contextual task")


(cl-defmethod occ-obj-tsk ((obj occ-tsk))
  obj)

(cl-defmethod occ-obj-tsk ((obj occ-ctsk))
  (occ-ctsk-tsk obj))


(cl-defmethod occ-obj-ctx ((obj occ-ctx))
  obj)

(cl-defmethod occ-obj-ctx ((obj occ-ctsk))
  (occ-ctsk-ctx obj))


(cl-defmethod occ-obj-marker ((obj marker))
  obj)

(cl-defmethod occ-obj-marker ((obj occ-obj-tsk))
  (occ-tsk-marker (occ-obj-tsk obj)))


(defun occ-case (case title)
  (if (fboundp case)
      (funcall case title)
    title))

(cl-defgeneric occ-title (obj
                          case)
  "occ-format")

(cl-defmethod occ-title (obj
                         case)
  (occ-case case
            (occ-class-name obj)))

(cl-defmethod occ-title ((obj marker)
                         (case symbol))
  (occ-case case
            (occ-class-name obj)))

(cl-defmethod occ-title ((obj occ-obj)
                         (case symbol))
  (occ-case case
            (occ-class-name obj)))


(cl-defmethod occ-heading-marker ((obj null))
  (make-marker))

(cl-defmethod occ-heading-marker ((obj marker))
  (save-excursion
    (with-current-buffer (marker-buffer obj)
      (goto-char obj)
      (end-of-line)
      (outline-previous-heading)
      (point-marker))))

(cl-defmethod occ-heading-marker ((obj occ-obj-tsk))
  (occ-heading-marker
   (occ-obj-marker obj)))


;; occ-tsk - accessors
(cl-defmethod occ-rank ((tsk occ-tsk))
  (occ-debug :debug "occ-tsk-get-rank(occ-tsk=%s)" tsk)
  (let ((rank (occ-tsk-rank tsk)))
    (unless rank
      (setf (occ-tsk-rank tsk) (occ-calculate-rank tsk)))
    (occ-tsk-rank tsk)))

(cl-defmethod (setf occ-rank) (value (tsk occ-tsk))
  (occ-debug :debug "occ-tsk-get-rank(occ-tsk=%s)" tsk)
  (setf (occ-tsk-rank tsk) value))


(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx))
  (occ-calculate-rank-with obj ctx))


;; occ-ctsk - accessors
(cl-defmethod occ-rank ((obj occ-ctsk))
  (occ-debug :debug "occ-rank(occ-ctsk=%s)" obj)
  (let ((tsk (occ-ctsk-tsk obj)))
    (occ-rank tsk)))

(cl-defmethod (setf occ-rank) (value (obj occ-ctsk))
  (occ-debug :debug "occ-rank(occ-ctsk=%s)" obj)
  (let ((tsk (occ-ctsk-tsk obj)))
    (setf (occ-rank tsk) rank)))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-rank ((ctxask occ-ctxual-tsk))
  (occ-debug :debug "occ-rank(occ-ctxual-tsk=%s)" ctxask)
  (let ((rank (occ-ctxual-tsk-rank ctxask)))
    (unless rank
      (setf (occ-ctxual-tsk-rank ctxask) (occ-calculate-rank ctxask)))
    (occ-ctxual-tsk-rank ctxask)))

(cl-defmethod (setf occ-rank) (value (ctxask occ-ctxual-tsk))
  (occ-debug :debug "occ-rank(occ-ctxual-tsk=%s)" ctxask)
  (setf (occ-ctxual-tsk-rank ctxask) value))


;; occ-ctx - accessors
(cl-defmethod occ-avgrank ((obj occ-ctx))
  (occ-debug :debug "occ-avgrank(occ-ctx=%s)" obj)
  (let ((avgrank (occ-ctx-avgrank obj)))
    (unless avgrank
      (setf (occ-ctx-avgrank obj) (occ-calculate-avgrank obj)))
    (occ-ctx-avgrank obj)))

(cl-defmethod (setf occ-avgrank) (value (obj occ-ctx))
  (occ-debug :debug "occ-avgrank(occ-ctx=%s)" obj)
  (setf (occ-ctx-avgrank obj) value))


;; occ-ctx - accessors
(cl-defmethod occ-varirank ((obj occ-ctx))
  (occ-debug :debug "occ-varirank(occ-ctx=%s)" obj)
  (let ((varirank (occ-ctx-varirank obj)))
    (unless varirank
      (setf (occ-ctx-varirank obj) (occ-calculate-varirank obj)))
    (occ-ctx-varirank obj)))

(cl-defmethod (setf occ-varirank) (value (obj occ-ctx))
  (occ-debug :debug "occ-varirank(occ-ctx=%s)" obj)
  (setf (occ-ctx-varirank obj) value))


;; occ-collection - accessors
(cl-defmethod occ-avgrank ((obj occ-collection))
  (occ-debug :debug "occ-avgrank(occ-collection=%s)" obj)
  (let ((avgrank (occ-collection-avgrank obj)))
    (unless avgrank
      (setf (occ-collection-avgrank obj) (occ-calculate-avgrank obj)))
    (occ-collection-avgrank obj)))

(cl-defmethod (setf occ-avgrank) (value (obj occ-collection))
  (occ-debug :debug "occ-avgrank(occ-collection=%s)" obj)
  (setf (occ-collection-avgrank obj) value))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-varirank ((obj occ-collection))
  (occ-debug :debug "occ-varirank(occ-collection=%s)" obj)
  (let ((varirank (occ-collection-varirank obj)))
    (unless varirank
      (setf (occ-collection-varirank obj) (occ-calculate-varirank obj)))
    (occ-collection-varirank obj)))

(cl-defmethod (setf occ-varirank) (value (obj occ-collection))
  (occ-debug :debug "occ-varirank(occ-collection=%s)" obj)
  (setf (occ-collection-varirank obj) value))


(cl-defmethod occ-current-tsk-with ((sym null))
  nil)

(cl-defmethod occ-current-tsk-with ((mrk marker))
  "return created occ-tsk from marker"
  (when (and
         mrk
         (markerp mrk)
         (> (marker-position-nonil mrk) 0))
    (org-with-cloned-marker mrk "<tree>"
      (let ((view-read-only nil)
            (buffer-read-only t))
        (read-only-mode)
        (org-previous-visible-heading 1)
        (let ((tsk (occ-make-tsk
                    (or org-clock-hd-marker mrk)
                    (occ-tsk-builder))))
          tsk)))))

(defun occ-current-tsk (&optional occ-other-allowed)
  (let ((tsk (car
              *occ-clocked-ctxual-tsk-ctx-history*)))
    (let ((clock-marker    (occ-valid-marker org-clock-marker))
          (clock-hd-marker (occ-valid-marker org-clock-hd-marker)))
      (let ((clock (or clock-marker
                       clock-hd-marker)))
        (if (and tsk
                 clock
                 (occ-marker= tsk clock))
            tsk
          (when clock
            (let ((msg
                   (if tsk
                       (format "occ-current-tsk: %s from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s"
                               (occ-format tsk   'captilize)
                               (occ-format clock 'captilize))
                     (format "occ-current-tsk: %s is outside of occ"
                             (occ-format clock 'captilize)))))
              (if occ-other-allowed
                  (occ-debug :warning msg)
                (error msg))
              (occ-current-tsk-with clock))))))))


(cl-defgeneric occ-candidate (obj)
  "occ-candidate")

(cl-defmethod occ-candidate ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (cons (occ-format obj) obj))

(cl-defmethod occ-candidate ((obj occ-obj-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-format obj) obj))


;; global-object - accessors
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
           (occ-make-tsk-at-point (occ-tsk-builder))
           (make-occ-tree-tsk :name "empty tree tsk" :subtree nil))) ;; note: only using first file of roots
      (car (occ-tree-collection-roots collection)) 0)))

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


(cl-defmethod occ-files ()
  (occ-collect-files
   (occ-collection-object)))


(cl-defmethod occ-collect-list ((collection occ-tree-collection))
  (unless (occ-tree-collection-list collection)
    (let ((tsks (occ-collection collection))
          (tsk-list '()))
      (occ-mapc-tree-tsks
       #'(lambda (tsk args) (setf tsk-list (nconc tsk-list (list tsk))))
       tsks
       nil)
      (setf (occ-tree-collection-list collection)
            tsk-list)))
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


(defun occ-collection-object ()
  (unless occ-global-tsk-collection
    (if occ-global-tsk-collection-spec
        (progn
          (occ-make-tsk-collection occ-global-tsk-collection-spec)
          (occ-collect-tsks occ-global-tsk-collection t))
      (progn
        (occ-message "occ-global-tsk-collection-spec is nil, set using occ-set-global-tsk-collection-spec")
        (error "occ-global-tsk-collection-spec is nil"))))
  occ-global-tsk-collection)

;;; occ-obj-accessor.el ends here
