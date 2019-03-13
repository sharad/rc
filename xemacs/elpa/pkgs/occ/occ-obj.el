;;; occ-obj.el --- occ-api               -*- lexical-binding: t; -*-
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

;; (require 'eieio)

(require 'occ-obj-common)


(provide 'occ-obj)


;; TODO org-base-buffer

;; https://stackoverflow.com/questions/12262220/add-created-date-property-to-todos-in-org-mode

;; https://stackoverflow.com/questions/40884764/lisp-get-all-slot-names-from-an-class-instance

;; "org tsks accss common api"
;; (defvar org-)

(defvar occ-verbose 0)

(cl-defstruct occ-obj
  name)

(cl-defstruct (occ-prop (:include occ-obj))
  value)

;; NOTE: Remember when adding new attributes, nned to destroy existing object, else it will cause miss-match.
(cl-defstruct (occ-tsk (:include occ-obj))
  ;; [[file:~/.repos/git/main/resource/info/doc/orgs/private/doc/contents/org/tasks/personal/works/emacs/todo.org::*Each%20task%20should%20have%20different%20types%20of%20actions%20associated%20to%20it,%20default%20is%20to%20clock-in%20to%20it][Each task should have different types of actions associated to it, default is to clock-in to it]]
  action
  ;; *** Each task should have different types of actions associated to it, default is to clock-in to it
  ;; - rest could be
  ;; to ignore this task continue where already clockin or not
  ;; which could be used to avoid non-interesting buffers
  ;; - etc many different could be thought
  heading
  heading-prop
  marker
  file
  point
  clock-sum
  plist)

(cl-defstruct (occ-tree-tsk (:include occ-tsk))
  subtree)

(cl-defstruct (occ-list-tsk (:include occ-tsk))
  )

(cl-defstruct (occ-ctx (:include occ-obj))
  buffer
  file)

(cl-defstruct (occ-ctxual-tsk (:include occ-obj))
  ctx
  tsk
  rank)

(cl-defstruct (occ-collection (:include occ-obj))
  roots
  files)

(cl-defstruct (occ-tree-collection (:include occ-collection))
  tree)

(cl-defstruct (occ-list-collection (:include occ-collection))
  list)

;; (when nil

;;   (cl-defstruct xpoint
;;     x y)

;;   (setf zpoint (make-xpoint :x 5 :y 3))

;;   (setf (cl-struct-slot-value 'xpoint 'x point) 3)

;;   (cl--find-class 'xpoint)

;;   (cl-defstruct base
;;     baseattr)

;;   (cl-defstruct (drived (:include base))
;;     drivedattr)

;;   (setf baseobj1 (make-base :baseattr "xbaseattr"))



;;   (setf drivedobj1
;;         (make-drived
;;          :baseattr "xbaseattr"
;;          :drivedattr "xdrivedattr")))

;;; occ-obj.el ends here
