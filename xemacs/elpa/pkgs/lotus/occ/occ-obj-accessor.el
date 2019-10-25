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


(require 'occ-tsk)
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


(cl-defmethod occ-obj-tsk ((obj null))
  nil)

(cl-defmethod occ-obj-tsk ((obj occ-tsk))
  obj)

(cl-defmethod occ-obj-tsk ((obj occ-ctsk))
  (occ-ctsk-tsk obj))


(cl-defmethod occ-obj-ctx ((obj null))
  nil)

(cl-defmethod occ-obj-ctx ((obj occ-ctx))
  obj)

(cl-defmethod occ-obj-ctx ((obj occ-ctsk))
  (occ-ctsk-ctx obj))


(cl-defmethod occ-obj-marker ((obj null))
  nil)

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
(cl-defmethod occ-rank ((obj occ-tsk))
  (occ-debug :debug "occ-rank(occ-tsk=%s)" (occ-format obj 'capitalize))
  (let ((rank (occ-tsk-rank obj)))
    (unless rank
      (setf (occ-tsk-rank obj) (occ-calculate-rank obj)))
    (occ-tsk-rank obj)))

(cl-defmethod (setf occ-rank) (value (obj occ-tsk))
  (occ-debug :debug "setf occ-rank(occ-tsk=%s)" (occ-format obj 'capitalize))
  (setf (occ-tsk-rank obj) value))


;; occ-ctsk - accessors
(cl-defmethod occ-rank ((obj occ-ctsk))
  (occ-debug :debug "occ-rank(occ-ctsk=%s)" (occ-format (occ-obj-tsk obj) 'capitalize))
  (let ((tsk (occ-ctsk-tsk obj)))
    (occ-rank tsk)))

(cl-defmethod (setf occ-rank) (value (obj occ-ctsk))
  (occ-debug :debug "occ-rank(occ-ctsk=%s)" (occ-format (occ-obj-tsk obj) 'capitalize))
  (let ((tsk (occ-ctsk-tsk obj)))
    (setf (occ-rank tsk) rank)))


;; occ-ctxual-tsk - accessors
(cl-defmethod occ-rank ((obj occ-ctxual-tsk))
  (occ-debug :debug "occ-rank(occ-ctxual-tsk=%s)" (occ-format (occ-obj-tsk obj) 'capitalize))
  (let ((rank (occ-ctxual-tsk-rank obj)))
    (unless rank
      (setf (occ-ctxual-tsk-rank obj) (occ-calculate-rank obj)))
    (occ-ctxual-tsk-rank obj)))

(cl-defmethod (setf occ-rank) (value (obj occ-ctxual-tsk))
  (occ-debug :debug "occ-rank(occ-ctxual-tsk=%s)" (occ-format (occ-obj-tsk obj) 'capitalize))
  (setf (occ-ctxual-tsk-rank obj) value))


(cl-defmethod occ-member-tsk-rank ((obj occ-ctxual-tsk))
  (occ-debug :debug "occ-member-tsk-rank(occ-ctxual-tsk=%s)" (occ-format (occ-obj-tsk obj) 'capitalize))
  (let ((tsk (occ-ctxual-tsk-tsk obj)))
    (occ-rank tsk)))


;; occ-tsk - accessors
(cl-defmethod occ-format-string ((obj occ-tsk))
  ;; (occ-debug :debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (let ((format-string (occ-tsk-format-string obj)))
    (unless format-string
      (setf (occ-tsk-format-string obj) (occ-build-format-string obj)))
    (occ-tsk-format-string obj)))

(cl-defmethod (setf occ-format-string) (value (obj occ-tsk))
  ;; (occ-debug :debug "occ-tsk-format-string(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-string obj) value))


;; occ-tsk - accessors
(cl-defmethod occ-format-file ((obj occ-tsk))
  ;; (occ-debug :debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (let ((format-file (occ-tsk-format-file obj)))
    (unless format-file
      (setf (occ-tsk-format-file obj) (occ-build-format-file obj)))
    (occ-tsk-format-file obj)))

(cl-defmethod (setf occ-format-file) (value (obj occ-tsk))
  ;; (occ-debug :debug "occ-tsk-format-file(occ-tsk=%s)" obj)
  (setf (occ-tsk-format-file obj) value))


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


(cl-defgeneric occ-candidate (obj)
  "occ-candidate")

(cl-defmethod occ-candidate ((obj marker))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the obj
pointing to it."
  (cons (occ-format obj nil t) obj))

(cl-defmethod occ-candidate ((obj occ-obj-tsk))
  "Insert a line for the clock selection menu.
And return a cons cell with the selection character integer and the marker
pointing to it."
  (cons (occ-format obj nil t) obj))


(cl-defmethod occ-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj obj)
                   (exp-with-obj (read)))
               #'(lambda ()
                   (funcall
                    `(lambda (obj) ,exp-with-obj) obj)))))
    (funcall fun)))

(cl-defmethod occ-call-with-obj ((obj occ-obj-tsk))
  (let ((fun (let ((obj-name     (read)) ;prefill with obj
                   (exp-with-obj (read)))
               #'(lambda ()
                   (funcall
                    `(lambda (,obj-name) ,exp-with-obj) obj)))))
    (funcall fun)))

(let ((occ-debug-object nil))
  (cl-defmethod occ-set-debug-obj ((obj occ-obj-tsk))
    (setq occ-debug-object obj))
  (cl-defmethod occ-get-debug-obj ()
    occ-debug-object))


(defun occ-current-ctxual-tsk (&optional occ-other-allowed)
  (let* ((ctxual-tsk (car *occ-clocked-ctxual-tsk-ctx-history*)))
    (let ((clock-marker    (occ-valid-marker org-clock-marker))
          (clock-hd-marker (occ-valid-marker org-clock-hd-marker)))
      (let ((clock (or clock-marker
                       clock-hd-marker)))
        (if (and ctxual-tsk
                 clock
                 (occ-marker= ctxual-tsk clock))
            ctxual-tsk
          (when clock
            (let ((msg
                   (if ctxual-tsk
                       (format "occ-current-ctxual-tsk: %s from head of *occ-clocked-ctxual-tsk-ctx-history* is not equal to current clocking clock %s"
                               (occ-format ctxual-tsk   'captilize)
                               (occ-format clock 'captilize))
                     (format "occ-current-ctxual-tsk: %s is outside of occ"
                             (occ-format clock 'captilize)))))
              (if occ-other-allowed
                  (occ-debug :warning msg)
                (error msg))
              (occ-build-ctxual-tsk-with (and clock (occ-make-tsk clock))
                                         (occ-make-ctx-at-point)))))))))

(defun occ-current-tsk (&optional occ-other-allowed)
  (let ((curr-ctxual-tsk (occ-current-ctxual-tsk occ-other-allowed)))
    (when curr-ctxual-tsk
      (occ-obj-tsk curr-ctxual-tsk))))


(defun occ-collection-object ()
  (unless occ-global-tsk-collection
    (if occ-global-tsk-collection-spec
        (progn
          (occ-make-tsk-collection occ-global-tsk-collection-spec)
          (occ-collect-tsks occ-global-tsk-collection t))
      (progn
        (occ-message "occ-global-tsk-collection-spec is nil, set it using M-x occ-build-spec or set occ-global-tsk-collection-spec")
        (error "occ-global-tsk-collection-spec is nil, set it using M-x occ-build-spec or set occ-global-tsk-collection-spec"))))
  occ-global-tsk-collection)


;; global-object - accessors
(cl-defmethod occ-collect-tsks (collection
                                &optional
                                force)
  (error "first argument should be of type (or occ-tree-collection occ-list-collection)"))

;; FIND: what it mean by tsks collection-tree are same ?
(cl-defmethod occ-collect-tsks ((collection occ-tree-collection)
                                &optional
                                force)
  (unless (occ-tree-collection-tree collection)
    (setf
     (occ-tree-collection-tree collection)
     (mapcar #'occ-tree-tsk-build
             (occ-tree-collection-roots collection))))
  (occ-tree-collection-tree collection))

(cl-defmethod occ-collect-tsks ((collection occ-list-collection)
                                force)
  (unless (occ-list-collection-list collection)
    (setf
     (occ-list-collection-list collection)
     (append
      (mapcar #'occ-list-tsk-build
              (occ-list-collection-roots collection)))))
  (occ-list-collection-list collection))


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

                (mapc
                 #'(lambda (tsk)
                     (occ-mapc-tree-tsks
                      #'(lambda (tsk args)
                          (push (occ-tsk-file tsk) files))
                      tsk
                      nil))
                 tsks)

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
      (mapc
       #'(lambda (tsk)
           (occ-mapc-tree-tsks
            #'(lambda (subtsk args) (setf tsk-list (nconc tsk-list (list subtsk))))
            tsk
            nil))
       tsks)

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


(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj occ-ctx)
                                       &key
                                       builder
                                       obtrusive)
  "return CTSKs list"
  (let ((builder (or builder #'occ-build-ctsk-with)))
    (let ((ctsks
           (occ-run-unobtrusively obtrusive
              (let ((tsks (occ-collect-list collection))) ;;????TODO
                (when tsks
                  (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                          tsks))))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it tpo occ-select occ-clock-in
        (occ-message "Busy user input %s" last-input-event)
        ctsks))))

;; FOr now.
(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj occ-ctx)
                                       &key
                                       builder
                                       obtrusive)
  "return CTSKs list"
  (let ((builder (or builder #'occ-build-ctsk-with)))
    (let ((ctsks
           (let ((tsks (occ-collect-list collection))) ;;????TODO
             (when tsks
               (mapcar #'(lambda (tsk) (funcall builder tsk obj))
                       tsks)))))
      (unless (eq t ctsks)
        ;; BUG: TODO: convey it top occ-select occ-clock-in
        (occ-message "Busy user input %s" last-input-event)
        ctsks))))

(cl-defmethod occ-collection-obj-list ((collection occ-collection)
                                       (obj null)
                                       &key
                                       builder
                                       obtrusive)
  "return CTSKs list"
  (occ-collection-obj-list collection
                           (occ-make-ctx-at-point)
                           :builder   builder
                           :obtrusive obtrusive))


;; http://sachachua.com/blog/2015/03/getting-helm-org-refile-clock-create-tasks/

(cl-defgeneric occ-list (obj
                         &key
                         builder
                         obtrusive)
  "occ-list")

(cl-defmethod occ-list ((obj occ-ctx)
                        &key
                        builder
                        obtrusive)
  "return CTXUAL-TSKs container"
  (occ-collection-obj-list (occ-collection-object)
                           obj
                           :builder builder
                           :obtrusive obtrusive))

(cl-defmethod occ-list ((obj null)
                        &key builder
                        obtrusive)
  "return TSKs container"
  (occ-list (occ-make-ctx-at-point)
            :builder   builder
            :obtrusive obtrusive))

(cl-defmethod occ-length ()
  (length (occ-collect-list (occ-collection-object))))

;;; occ-obj-accessor.el ends here
