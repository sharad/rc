;;; occ-obj-ctor.el --- occ-api               -*- lexical-binding: t; -*-
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

;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Generic-Functions.html

;;; Code:

(provide 'occ-obj-ctor)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj)
(require 'occ-prop)
(require 'occ-rank)
(require 'occ-property-rank-methods)


(defvar occ-global-tsk-collection-spec        nil)


(defvar occ-global-tsk-collection             nil)
(defvar occ-global-tsk-collection-change-hook nil
  "run when occ-global-tsk-collection-change-hook get changed.")


(defun occ-tsk-builder ()
  (unless occ-global-tsk-collection (occ-collection-object))
  (if occ-global-tsk-collection
      (let ((classname (cl-classname (occ-collection-object))))
        (cond
         ((eq 'occ-list-collection classname)
          #'make-occ-list-tsk)
         ((eq 'occ-tree-collection classname)
          #'make-occ-tree-tsk)
         (t
          (error "occ-global-tsk-collection is not from occ-list-collection or occ-tree-collection class"))))
    (error "occ-global-tsk-collection is NIL not from occ-list-collection or occ-tree-collection class")))


(defun occ-heading-content-only ()
  (when (org-at-heading-p)
    (save-excursion
      (save-restriction
        (let ((start (progn
                       (goto-char (org-element-property
                                   :contents-begin (org-element-at-point)))
                       (while (org-at-drawer-p)
                         (goto-char (org-element-property :end
                                                          (org-element-at-point))))
                       (point))))
          (unless (org-at-heading-p)
            (progn
              (outline-next-heading)
              ;; (outline-next-visible-heading 1)
              (backward-char)
              (buffer-substring start (point)))))))))


(defun occ-make-tsk-at-point (&optional builder)
    (let ((builder (or builder
                       (occ-tsk-builder))))
        (let ((tsk nil)
              (heading-with-string-prop
               (if (org-before-first-heading-p)
                   'noheading
                 (org-get-heading 'notags))))
          (let ((heading      (when heading-with-string-prop
                                (if (eq heading-with-string-prop 'noheading)
                                    heading-with-string-prop
                                  (substring-no-properties heading-with-string-prop))))
                (heading-prop heading-with-string-prop)
                (marker       (move-marker
                               (make-marker)
                               (point)
                               (org-base-buffer (current-buffer))))
                (file         (buffer-file-name))
                (point        (point))
                (clock-sum    (if (org-before-first-heading-p)
                                  0
                                (org-clock-sum-current-item)))
                ;; BUG: TODO: SHOULD need to maintain plist of :PROPERTIES:
                ;; separately as keys for these are returned in UPCASE. while it
                ;; is not the case with other generic properties which are not
                ;; part of :PROPERTIES: block.

                ;; NOTE also these two are mixed in one list only
                (tsk-plist    (cadr (org-element-at-point))))
            (when heading
              (setf tsk
                    (funcall builder
                             :name         heading
                             :heading      heading
                             :heading-prop heading-prop
                             :marker       marker
                             :file         file
                             :point        point
                             :clock-sum    clock-sum
                             :plist        tsk-plist))
              (let ((inherit t)
                    (inherited-props
                     ;; is it correct ? - guess it is ok and correct.
                     (occ-readprop-props)))
                (dolist (prop inherited-props)
                  (let* ((propstr (if (keywordp prop)
                                      (substring (symbol-name prop) 1)
                                    (symbol-name prop)))
                         (val (org-entry-get nil propstr inherit)))
                    (unless (occ-get-property tsk prop)
                      (occ-set-property tsk prop val))))))
            (occ-reread-props tsk)      ;reset list properties
            tsk))))

(cl-defmethod occ-make-tsk ((obj number)
                            &optional builder)
  (occ-debug :debug "point %s" obj)
  (if (<= obj (point-max))
      (save-restriction
        (save-excursion
          (goto-char obj)
          (occ-make-tsk-at-point builder)))))

(cl-defmethod occ-make-tsk ((obj marker)
                            &optional builder)
  (occ-debug :debug "point %s" obj)
  (if (and
       (marker-buffer obj)
       (numberp       (marker-position obj)))
      (with-current-buffer (marker-buffer obj)
        (if (<= (marker-position obj) (point-max))
            (occ-make-tsk (marker-position obj) builder)))))

(cl-defmethod occ-make-tsk ((obj null)
                            &optional builder)
  (occ-debug :debug "current pos %s" (point-marker))
  (occ-make-tsk (point-marker) builder))

(cl-defmethod occ-make-tsk ((obj occ-tsk)
                            &optional builder)
  obj)


(cl-defmethod occ-make-filter (&key average stddev variance)
  (make-occ-filter
   :average  average
   :stddev   stddev
   :variance variance))


(cl-defgeneric occ-make-ctx (obj)
  "occ-make-ctx")

(cl-defmethod occ-make-ctx-at-point (&optional mrk)
  (let* ((mrk (or mrk (point-marker)))
         (buff (marker-buffer mrk))
         (buff (if buff
                   (if (bufferp buff)
                       buff
                     (if (stringp buff)
                         (or
                          (get-buffer buff)
                          (if (file-exists-p buff)
                              (get-file-buffer buff)))))
                 (window-buffer)))
         (buf (org-base-buffer buf))
         (file (buffer-file-name buff))
         (ctx (make-occ-ctx
               :name (buffer-name buff)
               :file file
               :buffer buff)))
    ctx))

(cl-defmethod occ-make-ctx ((obj buffer))
  (let ((mrk (make-marker)))
    (set-marker mrk 0 obj)
    (occ-make-ctx-at-point mrk)))

(cl-defmethod occ-make-ctx ((obj marker))
  (occ-make-ctx-at-point obj))

(cl-defmethod occ-make-ctx ((obj null))
  (occ-make-ctx-at-point (point-marker)))

(cl-defmethod occ-make-ctx ((obj occ-ctx))
  obj)


(cl-defgeneric occ-make-ctsk-with (tsk ctx)
  "occ-make-ctsk-with")

(cl-defmethod occ-make-ctsk-with ((tsk occ-tsk)
                                  (ctx occ-ctx))
  ;; use occ-build-ctsk-with
  (make-occ-ctsk
   :name    nil
   :tsk     tsk
   :ctx     ctx))

(cl-defmethod occ-make-ctsk ((obj occ-ctsk))
  obj)

(cl-defmethod occ-make-ctsk ((obj occ-ctxual-tsk))
  ;; use occ-build-ctsk-with
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctsk
     :name    nil
     :tsk     tsk
     :ctx     ctx)))

(cl-defmethod occ-build-ctsk-with ((tsk occ-tsk) ;ctor
                                   (ctx occ-ctx))
  (occ-make-ctsk-with tsk ctx))

(cl-defmethod occ-build-ctsk ((obj occ-ctxual-tsk))
  (occ-make-ctsk obj))

(cl-defmethod occ-build-ctsk ((obj occ-ctsk))
  obj)


(cl-defgeneric occ-make-ctxual-tsk-with (tsk
                                         ctx
                                         &optional
                                         rank)
  "occ-make-ctxual-tsk-with")

(cl-defmethod occ-make-ctxual-tsk-with ((tsk occ-tsk)
                                        (ctx occ-ctx)
                                        &optional
                                        rank)
  ;; use occ-build-ctxual-tsk-with
  (make-occ-ctxual-tsk
   :name    nil
   :tsk     tsk
   :ctx     ctx
   :rank    rank))

(cl-defmethod occ-build-ctxual-tsk-with ((tsk occ-tsk) ;ctor
                                         (ctx occ-ctx))
  (occ-make-ctxual-tsk-with tsk ctx))

(cl-defmethod occ-build-ctxual-tsk-with ((tsk occ-ctxual-tsk) ;ctor
                                         (ctx occ-ctx))
  (debug))

(cl-defmethod occ-build-ctxual-tsk-with ((tsk null) ;ctor
                                         (ctx occ-ctx))
  nil)

(cl-defmethod occ-make-ctxual-tsk ((obj occ-ctsk)
                                   &optional
                                   rank)
  (let ((tsk (occ-obj-tsk obj))
        (ctx (occ-obj-ctx obj)))
    (make-occ-ctxual-tsk
     :name    nil
     :tsk     tsk
     :ctx     ctx
     :rank    rank)))

(cl-defmethod occ-make-ctxual-tsk ((obj occ-ctxual-tsk)
                                   &optional
                                   rank)
  obj)

(cl-defmethod occ-build-ctxual-tsk ((obj occ-ctsk)
                                    &optional
                                    rank)
  (occ-make-ctxual-tsk obj rank))

(cl-defmethod occ-build-ctxual-tsk ((obj occ-ctxual-tsk)
                                    &optional
                                    rank)
  obj)


(cl-defmethod occ-build-obj-with ((obj occ-tsk)
                                  (ctx occ-ctx))
   (occ-build-ctxual-tsk-with obj ctx))

(cl-defmethod occ-build-obj-with ((obj occ-tsk)
                                  (ctx null))
  (occ-build-obj-with obj (occ-make-ctx-at-point)))


(cl-defmethod occ-make-tsk-collection ((file-spec (head :tree)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-tree-collection
                       :name "tsk collection tree"
                       :roots (cdr file-spec))))
      (setf occ-global-tsk-collection collection))))

(cl-defmethod occ-make-tsk-collection ((file-spec (head :list)))
  (unless occ-global-tsk-collection
    (let ((collection (make-occ-list-collection
                       :name "tsk collection list"
                       :roots (cdr dir-spec))))
      (setf occ-global-tsk-collection collection))))


(defun occ-make-return (label value)
  (make-occ-return
   :label label
   :value value))

;;; occ-obj-ctor.el ends here
