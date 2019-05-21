;;; occ-prop-proper.el --- occ properties methods           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: sharad <sh4r4d _>
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

(provide 'occ-prop-proper)


(require 'org-misc-utils-lotus)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-prop)


(defun occ-match-method-args (ctx)
  (cl-method-sigs-matched-arg
   '(occ-readprop         (`((head ,val)  occ-ctx) val))
   '(occ-ctx-property-get (`((head ,val)) val))
   ctx))


(defun occ-org-set-property (prop value)
  (lotus-org-with-safe-modification
    (org-set-property prop value)))


(cl-defgeneric occ-writeprop-with (prop
                                   obj
                                   ctx
                                   value)
  "occ-writeprop-with")
(cl-defgeneric occ-readprop-with (prop
                                  obj
                                  ctx)
  "occ-readprop-with")
(cl-defgeneric occ-editprop-with (prop
                                  obj
                                  ctx)
  "occ-editprop-with")


(cl-defmethod occ-writeprop-with ((prop symbol)
                                  (obj occ-tsk)
                                  (ctx occ-ctx)
                                  value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (if value
      (progn
        (unless (org-get-property-block)
          ;;create property drawer
          (occ-debug :debug "occ-writeprop: property block not exist so creating it.")
          (let* ((range (org-get-property-block (point) 'force))
                 (start (when (consp range) (1- (car range)))))
            (when (numberp start)
              (goto-char start))))
        (occ-debug :debug
                   "occ-writeprop: adding prop: %s value: %s using (org-set-property)." prop value)
        (occ-org-set-property (symbol-name prop) value))
    (error "occ-writeprop value is nil")))
(cl-defmethod occ-readprop-with ((prop symbol)
                                 (obj occ-tsk)
                                 (ctx occ-ctx))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (occ-readprop prop obj ctx))
(cl-defmethod occ-editprop-with ((prop symbol)
                                 (obj occ-tsk)
                                 (ctx occ-ctx))
  (let ((value (occ-readprop prop obj ctx)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop-with prop obj ctx value)))


(cl-defgeneric occ-writeprop (prop
                              obj
                              value)
  "occ-writeprop")
(cl-defgeneric occ-readprop (prop
                             obj)
  "occ-readprop")
(cl-defgeneric occ-editprop (prop
                             obj)
  "occ-editprop")


(cl-defmethod occ-writeprop ((prop symbol)
                             (obj occ-obj-ctx-tsk)
                             value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-writeprop-with prop tsk ctx value)))
(cl-defmethod occ-readprop ((prop symbol)
                            (obj occ-obj-ctx-tsk))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-readprop-with prop tsk ctx)))
(cl-defmethod occ-editprop ((prop symbol)
                            (obj occ-obj-ctx-tsk))
  (let ((value (occ-readprop prop obj)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop prop obj value)))


(cl-defmethod occ-rank ((prop symbol)
                        (obj  occ-ctx-tsk))
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-rank-with prop tsk ctx)))

;;{{ file
(when nil                               ;rank calculation for org file name in which tsk aka entry not much useful
  (cl-defmethod occ-rank-with ((prop (eql 'file))
                               (obj occ-tsk)
                               (ctx occ-ctx))
   ;; file in which tsk aka org entry exists.
   "Predicate funtion to check if ctx matches to tsk's file attribute."
   (let* ((tsk-currfile (occ-get-property obj 'currfile))
          (tsk-currfile (if tsk-currfile (file-truename tsk-currfile))))
     (let* ((ctx-file (occ-ctx-file ctx))
            (ctx-file (when ctx-file (file-truename ctx-file))))
       (if tsk-currfile
           (progn
             (occ-debug :nodisplay "tsk %s tsk-currfile %s" (occ-format obj 'capitalize) tsk-currfile)
             (occ-debug :nodisplay "tsk %s ctx-file %s"     (occ-format obj 'capitalize) ctx-file))
         (occ-debug :nodisplay "tsk %s currfile %s not present."
                    (occ-format obj 'capitalize) tsk-currfile))
       (if (and tsk-currfile ctx-file
                (string-match tsk-currfile ctx-file))
           (* 2 (length tsk-currfile))     ;as exact match to file giving double matching points.
         0)))))
;;}}

;;{{ currfile
(cl-defmethod occ-rank-with ((prop (eql 'currfile))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  ;; file in which tsk aka org entry exists.
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((tsk-currfile (occ-get-property obj prop))
         (tsk-currfile (if tsk-currfile (file-truename tsk-currfile)))
         (ctx-file (occ-ctx-file ctx))
         (ctx-file (if ctx-file (file-truename ctx-file))))
    (if tsk-currfile
        (progn
          (occ-debug :nodisplay "tsk %s tsk-currfile %s" (occ-format obj 'capitalize) tsk-currfile)
          (occ-debug :nodisplay "tsk %s ctx-file %s"     (occ-format obj 'capitalize) ctx-file))
      (occ-debug :nodisplay "tsk %s tsk-currfile %s not present."
                 (occ-format obj 'capitalize) tsk-currfile))
    (if (and tsk-currfile ctx-file
             (string-match tsk-currfile ctx-file))
        (* 2 (length tsk-currfile))     ;as exact match to files giving double matching points.
      0)))

(cl-defmethod occ-ctx-property-get ((ctx-pair (head currfile)))
  "file of context"
  (let* ((ctx      (cdr ctx-pair))
         (currfile (occ-ctx-file ctx)))
    currfile))
(cl-defmethod occ-readprop-with ((prop (eql 'currfile))
                                 (obj occ-tsk)
                                 (ctx occ-ctx))
  "currfile property for tsk aka org entry"
  (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
         (ctx-dir      (when (stringp ctx-currfile)
                         (file-name-directory ctx-currfile)))
         (prompt       (concat (symbol-name prop) ": ")))
    (ido-read-currfile-name prompt ctx-dir ctx-currfile)))
;;}}

;;{{ root
(cl-defmethod occ-rank-with ((prop (eql 'root))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((tsk-root (occ-get-property obj prop))
         (tsk-root (when tsk-root (file-truename tsk-root)))
         (ctx-file (occ-ctx-file ctx))
         (ctx-file (when ctx-file (file-truename ctx-file))))
    (if tsk-root
        (progn
          (occ-debug :nodisplay "tsk %s tsk-root %s" (occ-format obj 'capitalize) tsk-root)
          (occ-debug :nodisplay "tsk %s ctx-file %s" (occ-format obj 'capitalize) ctx-file))
      (occ-debug :nodisplay "tsk %s tsk-root %s not present."
                 (occ-format obj 'capitalize) tsk-root))
    (if (and tsk-root ctx-file
             (string-match tsk-root ctx-file))
        (length tsk-root)
      0)))

(cl-defmethod occ-ctx-property-get ((ctx-pair (head root)))
  (let* ((ctx  (cdr ctx-pair))
         (file (occ-ctx-file ctx))
         (root (and file (dirname-of-file file))))
    root))
(cl-defmethod occ-readprop-with ((prop (eql 'root))
                                 (obj occ-tsk)
                                 (ctx occ-ctx))
  (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
         (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
         (prompt (concat (symbol-name prop) ": ")))
    (ido-read-directory-name prompt ctx-dir ctx-dir)))
;;}}

;;{{ sub-tree
(cl-defmethod occ-readprop-with ((prop (eql 'subtree))
                                 (obj occ-tsk)
                                 (ctx occ-ctx))
  (let ((prompt (concat (symbol-name prop) ": ")))
    (file-relative-name
     (ido-read-file-name ;; org-iread-file-name
      prompt
      default-directory default-directory))))
;;}}

;;{{ git-branch
(cl-defmethod occ-ctx-property-get ((ctx-pair (head git-branch)))
  (let* ((ctx (cdr ctx-pair))
         (file (occ-ctx-file ctx)))
    file))
;;}}

;;{{ git-branch
;;}}

;;{{ git-branch
;;}}

;;{{ git-branch
;;}}






(cl-defmethod occ-rank-with ((prop (eql 'status))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (let ((todo-type
         (occ-get-property obj 'todo-type))
        (closed
         (occ-get-property obj 'closed))
        (status
         (occ-get-property obj 'todo-keyword)))
    (if (or
         closed
         (eql todo-type 'done)
         (string-equal status "HOLD"))
        -30 0)))

(cl-defmethod occ-rank-with ((prop (eql 'key))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((key (occ-get-property obj 'KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod occ-rank-with ((prop (eql 'heading-level))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((level (occ-get-property obj 'level)))
    (if level level 0)))

(cl-defmethod occ-rank-with ((prop (eql 'timebeing))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  (let ((timebeing (occ-get-property obj 'timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property obj 'clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod occ-rank-with ((prop (eql 'current-clock))
                             (obj occ-tsk)
                             (ctx occ-ctx))
  (let* ((tsk-marker (occ-get-property obj 'marker)))
    (if (occ-marker= obj org-clock-marker)
        100
      0)))


(when nil
  (cl-method-first-arg 'occ-ctx-property-get)
  (occ-readprop-props)
  (cl-method-matched-arg 'occ-readprop 'occ-ctx-property-get (occ-make-ctx-at-point))
  (funcall 'occ-ctx-property-get (cons 'file (occ-make-ctx-at-point))))

(when nil
  (cl-method-sigs-matched-arg
   '(occ-readprop (`((head ,val) occ-ctx) val))
   '(occ-ctx-property-get (`((head ,val) val)))
   (occ-make-ctx-at-point)))



(defun occ-readprop-props ()
  (cl-method-param-case
   '(occ-readprop (`((head ,val) occ-ctx) val))))



;;; occ-prop-proper.el ends here
