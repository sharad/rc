;;; occ-prop.el --- occ properties methods           -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

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

(provide 'occ-prop)


(require 'occ-obj-common)
(require 'occ-tree)
(require 'occ-obj-accessor)
(require 'occ-util-common)
(require 'occ-obj-method)
(require 'occ-prop)


(cl-defmethod occ-readprop ((prop symbol) (ctx occ-ctx))
  (occ-readprop (cons prop (occ-make-tsk nil)) ctx))
(cl-defmethod occ-writeprop ((prop symbol) value)
  (if value
      (org-set-property (symbol-name prop) value)
    (error "occ-writeprop value is nil")))
(cl-defmethod occ-editprop ((prop symbol) (ctx occ-ctx))
  (let ((value (occ-readprop prop ctx)))
    (occ-writeprop prop value)))

;;{{ file
(when nil                               ;rank calculation for org file name in which tsk aka entry not much useful
  (cl-defmethod occ-rank ((tsk-pair (head file)))
                         (ctx occ-ctx)
   ;; file in which tsk aka org entry exists.
   "Predicate funtion to check if ctx matches to tsk's file attribute."
   (let ((prop (car tsk-pair))
         (tsk  (cdr tsk-pair)))
     (let* ((currfile (occ-get-property tsk 'currfile))
            (currfile (if currfile (file-truename currfile))))
      (let* ((file (occ-ctx-file ctx))
             (file (if file (file-truename file))))
        (if currfile
            (progn
              (occ-debug :nodisplay "tsk %s currfile %s" (occ-tsk-heading tsk) currfile)
              (occ-debug :nodisplay "tsk %s file %s"     (occ-tsk-heading tsk) file))
          (occ-debug :nodisplay "tsk %s currfile %s not present."
                     (occ-tsk-heading tsk) currfile))
        (if (and currfile file
                 (string-match currfile file))
            (* 2 (length currfile))     ;as exact match to file giving double matching points.
          0))))))
;;}}

;;{{ currfile
(cl-defmethod occ-rank ((tsk-pair (head currfile))
                        (ctx occ-ctx))
  ;; file in which tsk aka org entry exists.
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let* ((currfile (occ-get-property tsk 'currfile))
           (currfile (if currfile (file-truename currfile))))
      (let* ((file (occ-ctx-file ctx))
             (file (if file (file-truename file))))
        (if currfile
            (progn
              (occ-debug :nodisplay "tsk %s currfile %s" (occ-tsk-heading tsk) currfile)
              (occ-debug :nodisplay "tsk %s file %s"     (occ-tsk-heading tsk) file))
          (occ-debug :nodisplay "tsk %s currfile %s not present."
                     (occ-tsk-heading tsk) currfile))
        (if (and currfile file
                 (string-match currfile file))
            (* 2 (length currfile))     ;as exact match to file giving double matching points.
          0)))))

(cl-defmethod occ-ctx-property-get ((ctx-pair (head currfile)))
  "file of context"
  (let* ((ctx (cdr ctx-pair))
         (currfile (occ-ctx-file ctx)))
    currfile))
(cl-defmethod occ-readprop ((tsk-pair (head currfile))
                            (ctx occ-ctx))
  "currfile property for tsk aka org entry"
  (let* ((currfile (if ctx (occ-ctx-file ctx)))
         (dir (when (stringp currfile)
                  (file-name-directory currfile) (dirname-of-file currfile)))
         (prompt (concat (symbol-name (car tsk-pair)) ": ")))
    (ido-read-currfile-name prompt dir currfile)))
;;}}

;;{{ root
(cl-defmethod occ-rank ((tsk-pair (head root))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let* ((root (occ-get-property tsk 'root))
           (root (if root (file-truename root))))
     (let* ((file (occ-ctx-file ctx))
            (file (if file (file-truename file))))
       (if root
           (progn
             (occ-debug :nodisplay "tsk %s root %s" (occ-tsk-heading tsk) root)
             (occ-debug :nodisplay "tsk %s file %s" (occ-tsk-heading tsk) file))
         (occ-debug :nodisplay "tsk %s root %s not present."
                    (occ-tsk-heading tsk) root))
       (if (and root file
                (string-match root file))
           (length root)
         0)))))
(cl-defmethod occ-ctx-property-get ((ctx-pair (head root)))
  (let* ((ctx (cdr ctx-pair))
         (file (occ-ctx-file ctx))
         (root (and file (dirname-of-file file))))
    root))
(cl-defmethod occ-readprop ((tsk-pair (head root))
                            (ctx occ-ctx))
  (let* ((file (if ctx (occ-ctx-file ctx)))
         (dir (if (stringp file) (file-name-directory file) (dirname-of-file file)))
         (prompt (concat (symbol-name (car tsk-pair)) ": ")))
    (ido-read-directory-name prompt dir dir)))
;;}}

;;{{ sub-tree
(cl-defmethod occ-readprop ((tsk-pair (head subtree))
                            (ctx occ-ctx))
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let ((prompt (concat (symbol-name (car tsk-pair)) ": ")))
     (file-relative-name
       (ido-read-file-name ;; org-iread-file-name
        prompt
        default-directory default-directory)))))

(cl-defmethod occ-writeprop ((tsk-pair (head subtree)) value))
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






(cl-defmethod occ-rank ((tsk-pair (head status))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's status attribute."
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let ((todo-type
           (occ-get-property tsk 'todo-type))
          (closed
           (occ-get-property tsk 'closed))
          (status
           (occ-get-property tsk 'todo-keyword)))
      (if (or
           closed
           (eql todo-type 'done)
           (string-equal status "HOLD"))
          -30 0))))

(cl-defmethod occ-rank ((tsk-pair (head key))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let* ((key (occ-get-property tsk 'KEY)))
      (if key (string-to-number key) 0))))

(cl-defmethod occ-rank ((tsk-pair (head heading-level))
                        (ctx occ-ctx))
  "Predicate funtion to check if ctx matches to tsk's file attribute."
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let* ((level (occ-get-property tsk 'level)))
      (if level level 0))))

(cl-defmethod occ-rank ((tsk-pair (head timebeing))
                        (ctx occ-ctx))
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
   (let ((timebeing (occ-get-property tsk 'timebeing)))
     (let ((timebeing-time (if timebeing (org-duration-string-to-minutes timebeing) 0))
           (clocked-time   (occ-get-property tsk 'clock-sum)))
       (if (and
            (numberp clocked-time)
            (numberp timebeing-time)
            (> timebeing-time clocked-time))
           (- timebeing-time clocked-time)
         0)))))

(cl-defmethod occ-rank ((tsk-pair (head current-clock))
                        (ctx occ-ctx))
  (let ((prop (car tsk-pair))
        (tsk  (cdr tsk-pair)))
    (let* ((tsk-marker (occ-get-property tsk 'marker)))
      (if (and
           (markerp org-clock-hd-marker)
           (markerp tsk-marker)
           (equal org-clock-hd-marker org-clock-hd-marker))
          100
        0))))


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
