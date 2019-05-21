;;; occ-prop.el --- occ properties methods           -*- lexical-binding: t; -*-

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

(provide 'occ-prop)


(require 'org-misc-utils-lotus)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-prop)


(defun occ-match-prop-method-args (ctx)
  (cl-method-sigs-matched-arg
   '(occ-readprop         (`((head ,val)  occ-ctx) val))
   '(occ-ctx-property-get (`((head ,val)) val))
   ctx))

(defun occ-match-prop-method-args (ctx)
  (cl-method-sigs-matched-arg
   '(occ-readprop-with (`(occ-tsk occ-ctx (eql ,val)) val))
   '(occ-get-property  (`(occ-ctx (eql ,val)) val))
   ctx))

;; move to occ-test.el
;; (cl-method-param-case-with-value-new '(occ-get-property  (`(occ-ctx (eql ,val)) val)) (occ-make-ctx-at-point))
;; (occ-match-prop-method-args (occ-make-ctx-at-point))

;; (cl-method-param-signs 'occ-readprop-with)

;; (cl-method-param-signs 'occ-get-property)


(defun occ-org-set-property (prop value)
  (lotus-org-with-safe-modification
    (org-set-property prop value)))


(cl-defgeneric occ-rank (obj
                         ctx)
  "occ-rank")

(cl-defmethod occ-rank (tsk-pair
                        ctx)
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk-pair=%s ctx=%s)" tsk-pair ctx)
  0)

;; ISSUE? should it return rank or occ-ctxual-tsk
(cl-defmethod occ-rank ((tsk occ-tsk)
                        (ctx occ-ctx))
  ;; too much output
  ;; (occ-debug :debug "occ-rank(tsk=%s ctx=%s)" tsk ctx)
  (let ((rank
         (reduce #'+
                 (mapcar #'(lambda (slot) ;;TODO: check if method exist or not, or use some default method.
                             (occ-rank (cons slot tsk) ctx))
                         (occ-class-slots tsk)))))
    rank))


(cl-defgeneric occ-writeprop-with (obj
                                   ctx
                                   prop
                                   value)
  "occ-writeprop-with")
(cl-defgeneric occ-readprop-with (obj
                                  ctx
                                  prop)
  "occ-readprop-with")
(cl-defgeneric occ-editprop-with (obj
                                  prop
                                  ctx)
  "occ-editprop-with")


(cl-defmethod occ-writeprop-with ((obj occ-tsk)
                                  (ctx occ-ctx)
                                  (prop symbol)
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
(cl-defmethod occ-readprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (occ-readprop prop obj ctx))
(cl-defmethod occ-editprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop symbol))
  (let ((value (occ-readprop prop obj ctx)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop-with prop obj ctx value)))


(cl-defgeneric occ-writeprop (obj
                              prop
                              value)
  "occ-writeprop")
(cl-defgeneric occ-readprop (obj
                             prop)
  "occ-readprop")
(cl-defgeneric occ-editprop (obj
                             prop)
  "occ-editprop")


(cl-defmethod occ-writeprop ((obj occ-obj-ctx-tsk)
                             (prop symbol)
                             value)
  (occ-debug :debug "occ-writeprop: prop: %s, value: %s"
             prop value)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-writeprop-with prop tsk ctx value)))
(cl-defmethod occ-readprop ((obj occ-obj-ctx-tsk)
                            (prop symbol))
  (occ-debug :debug "occ-readprop: prop: %s"
             prop)
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-readprop-with prop tsk ctx)))
(cl-defmethod occ-editprop ((obj occ-obj-ctx-tsk)
                            (prop symbol))
  (let ((value (occ-readprop prop obj)))
    (occ-debug :debug
               "occ-editprop: prop: %s, value: %s" prop value)
    (occ-writeprop prop obj value)))


;; (cl-defgeneric occ-ctx-property-get (prop
;;                                      ctx)
;;   "occ-ctx-property-get")

(cl-defmethod occ-rank ((obj  occ-obj-ctx-tsk)
                        (prop symbol))
  (let ((tsk (occ-ctsk-tsk obj))
        (ctx (occ-ctsk-ctx obj)))
    (occ-rank-with prop tsk ctx)))

;;{{ file
(when nil                               ;rank calculation for org file name in which tsk aka entry not much useful
  (cl-defmethod occ-rank-with ((obj occ-tsk)
                               (ctx occ-ctx)
                               (prop (eql file)))
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
(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql currfile)))
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


;; (cl-defmethod occ-ctx-property-get ((prop (eql currfile))
;;                                     (ctx occ-ctx))
;;   "occ-ctx-property-get"
;;   (let ((currfile (occ-ctx-file ctx)))
;;     currfile))

(cl-defmethod occ-get-property ((ctx occ-ctx)
                                (prop (eql currfile)))
  "occ-ctx-property-get"
  (occ-message "calling occ-get-property(ctx occ-ctx)")
  (let ((currfile (occ-ctx-file ctx)))
    currfile))

;; (occ-get-property (occ-make-ctx-at-point) 'currfile)

;; (cl-defmethod occ-ctx-property-get ((ctx-pair (head currfile)))
;;   "file of context"
;;   (let* ((ctx      (cdr ctx-pair))
;;          (currfile (occ-ctx-file ctx)))
;;     currfile))
(cl-defmethod occ-readprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop (eql currfile)))
  "currfile property for tsk aka org entry"
  (let* ((ctx-currfile (if ctx (occ-ctx-file ctx)))
         (ctx-dir      (when (stringp ctx-currfile)
                         (file-name-directory ctx-currfile)))
         (prompt       (concat (symbol-name prop) ": ")))
    (ido-read-currfile-name prompt ctx-dir ctx-currfile)))
;;}}

;;{{ root
(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql root)))
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

(cl-defmethod occ-get-property ((ctx occ-ctx)
                                (prop (eql root)))
  "occ-ctx-property-get"
  (let ((file (occ-ctx-file ctx)))
    (when file (dirname-of-file file))))

;; (cl-defmethod occ-ctx-property-get ((ctx-pair (head root)))
;;   (let* ((ctx  (cdr ctx-pair))
;;          (file (occ-ctx-file ctx))
;;          (root (and file (dirname-of-file file))))
;;     root))
(cl-defmethod occ-readprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop (eql root)))
  (let* ((ctx-file   (when ctx (occ-ctx-file ctx)))
         (ctx-dir    (when (stringp ctx-file) (file-name-directory ctx-file)))
         (prompt (concat (symbol-name prop) ": ")))
    (ido-read-directory-name prompt ctx-dir ctx-dir)))
;;}}

;;{{ sub-tree
(cl-defmethod occ-readprop-with ((obj occ-tsk)
                                 (ctx occ-ctx)
                                 (prop (eql subtree)))
  (let ((prompt (concat (symbol-name prop) ": ")))
    (file-relative-name
     (ido-read-file-name ;; org-iread-file-name
      prompt
      default-directory default-directory))))
;;}}

;;{{ git-branch
(cl-defmethod occ-get-property ((ctx occ-ctx)
                                (prop (eql git-branch)))
  "occ-ctx-property-get"
  (let ((file (occ-ctx-file ctx)))
    file))
;; (cl-defmethod occ-ctx-property-get ((ctx-pair (head git-branch)))
;;   (let* ((ctx (cdr ctx-pair))
;;          (file (occ-ctx-file ctx)))
;;     file))
;;}}

;;{{ git-branch
;;}}

;;{{ git-branch
;;}}

;;{{ git-branch
;;}}






(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql status)))
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

(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql key)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((key (occ-get-property obj 'KEY)))
    (if key (string-to-number key) 0)))

(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql heading-level)))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let* ((level (occ-get-property obj 'level)))
    (if level level 0)))

(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql timebeing)))
  (let ((timebeing (occ-get-property obj 'timebeing)))
    (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
          (clocked-time   (occ-get-property obj 'clock-sum)))
      (if (and
           (numberp clocked-time)
           (numberp timebeing-time)
           (> timebeing-time clocked-time))
          (- timebeing-time clocked-time)
        0))))

(cl-defmethod occ-rank-with ((obj occ-tsk)
                             (ctx occ-ctx)
                             (prop (eql current-clock)))
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



;;; occ-prop.el ends here
