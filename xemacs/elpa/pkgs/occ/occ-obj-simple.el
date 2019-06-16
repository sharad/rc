;;; occ-obj-simple.el --- occ simple methods         -*- lexical-binding: t; -*-

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

(provide 'occ-obj-simple)


(require 'lotus-misc-utils)
(eval-when-compile
  (require 'lotus-misc-utils))


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-obj-utils)
(require 'occ-util-common)
(require 'occ-print)
(require 'occ-predicate)
(require 'occ-rank)
(require 'occ-prop)
(require 'occ-property-rank-methods)
(require 'occ-list-filter)
(require 'occ-select)
(require 'occ-helm)
(require 'occ-clock)


(defvar occ-idle-timeout 7)


;; (cl-defmethod occ-find ((collection occ-collection) (mrk marker)))

(cl-defmethod occ-find ((collection list)
                        (mrk marker)))

(cl-defgeneric occ-goto (obj)
  "occ-goto")

(cl-defmethod occ-goto ((obj marker))
  (progn
    (switch-to-buffer (marker-buffer obj))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    (org-content 10)
    (goto-char obj)))

(cl-defmethod occ-goto ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-goto mrk)
      (error "marker %s invalid." mrk))))


(cl-defgeneric occ-set-to (obj)
  "occ-set-to")

(cl-defmethod occ-set-to ((obj marker))
  (progn
    (set-buffer (marker-buffer obj))
    ;; TODO find about "org-overview"
    ;; https://stackoverflow.com/questions/25161792/emacs-org-mode-how-can-i-fold-everything-but-the-current-headline
    ;; https://emacs.stackexchange.com/questions/26827/test-whether-org-mode-heading-or-list-is-folded
    ;; https://github.com/facetframer/orgnav
    ;; https://stackoverflow.com/questions/6198339/show-org-mode-outline-up-to-a-certain-heading-level
    ;; (outline-show-all)
    ;; (org-content 10)
    (goto-char obj)))

(cl-defmethod occ-set-to ((obj occ-obj-tsk))
  (let ((mrk (occ-obj-marker obj)))
    (if (and
         (markerp mrk)
         (marker-buffer mrk))
        (occ-set-to mrk)
      (error "marker %s invalid." mrk))))



(cl-defmethod occ-induct-child ((obj occ-tree-tsk)
                                (child occ-tree-tsk))
  (occ-set-property child 'subtree-level
                    (occ-get-property obj 'subtree-level))
  (occ-insert-node-after-element child obj
                                 (occ-tree-collection-list (occ-collection-object)))
  (setf
   (occ-tree-tsk-subtree obj) (nconc
                               (occ-tree-tsk-subtree obj)
                               (list  child))))

(cl-defmethod occ-induct-child ((obj occ-list-tsk)
                                (child occ-list-tsk))
  (occ-set-property child 'subtree-level
                    (occ-get-property obj 'subtree-level))
  (occ-insert-node-after-element child obj
                                 (occ-tree-collection-list (occ-collection-object))))

(cl-defgeneric occ-capture-with (tsk
                                 ctx
                                 &optional clock-in-p)
  "occ-capture-with")

(cl-defmethod occ-capture-with ((tsk occ-obj-tsk)
                                (ctx occ-ctx)
                                &optional clock-in-p)
  (let* ((mrk      (occ-tsk-marker tsk))
         (template (occ-capture+-helm-select-template)))
    (when template
      (with-org-capture+ marker 'entry `(marker ,mrk) template '(:empty-lines 1)
        (let ((tmptsk (occ-make-tsk marker)))
          (occ-props-edit-with tmptsk ctx)
          ;; (occ-props-window-edit-with tsk ctx :timeout occ-idle-timeout)
          t)
        (let ((child-tsk (occ-make-tsk marker)))
          (when child-tsk
            (occ-induct-child tsk child-tsk)
            (if clock-in-p
                (occ-try-clock-in-with child-tsk ctx))))))))


(cl-defgeneric occ-capture (obj
                            &optional clock-in-p)
  "occ-capture")

(cl-defmethod occ-capture ((obj marker)
                           &optional clock-in-p)
  (org-capture+
   'entry
   `(marker ,obj)
   'occ-capture+-helm-select-template
   :empty-lines 1))

(cl-defmethod occ-capture ((obj occ-tsk)
                           &optional clock-in-p)
  (let ((mrk (occ-tsk-marker obj)))
    (occ-capture mrk)))

(cl-defmethod occ-capture ((obj occ-obj-ctx-tsk)
                           &optional clock-in-p)
  (let ((tsk        (occ-obj-tsk obj))
        (ctx        (occ-obj-ctx obj)))
    (occ-capture-with tsk ctx clock-in-p)))


(cl-defgeneric occ-procreate-child (obj)
  "occ-child")

(cl-defmethod occ-procreate-child ((obj marker))
  (if (not (occ-unnamed-p obj))
      (occ-capture obj helm-current-prefix-arg)
    (let ((title (occ-title obj 'captilize)))
     (error "%s is unnamed %s so can not create child "
           (occ-format obj 'captilize)
           title
           title))))

(cl-defmethod occ-procreate-child ((obj occ-obj-tsk))
  (if (not (occ-unnamed-p obj))
      (occ-capture obj helm-current-prefix-arg)
    (let ((title (occ-title obj 'captilize)))
      (error "%s is unnamed %s so can not create child "
             (occ-format obj 'captilize)
             title
             title))))

(cl-defmethod occ-procreate-child ((obj occ-obj-ctx-tsk))
  (let ((tsk        (occ-obj-tsk obj))
        (ctx        (occ-obj-ctx obj)))
   (if (not (occ-unnamed-p tsk))
       (occ-capture obj helm-current-prefix-arg)
     (let ((title (occ-title obj 'captilize)))
       (error "%s is unnamed %s so can not create child "
              (occ-format obj 'captilize)
              title
              title)))))


(cl-defmethod occ-procreate-child-with ((obj occ-obj-tsk)
                                        (ctx occ-ctx))
  (if (not (occ-unnamed-p obj))
      (occ-capture-with obj ctx helm-current-prefix-arg)
    (let ((title (occ-title obj 'captilize)))
      (error "%s is unnamed %s so can not create child "
             (occ-format obj 'captilize)
             title
             title))))


(cl-defgeneric occ-procreate-child-clock-in (obj)
  "occ-child-clock-in")

(cl-defmethod occ-procreate-child-clock-in ((obj marker))
  (occ-capture obj t))

(cl-defmethod occ-procreate-child-clock-in ((obj occ-obj-tsk))
  (occ-capture obj t))


(defun occ-confirm (fn new)
  (occ-y-or-n-timeout)
  (error "Implement it."))

;;; occ-obj-simple.el ends here
