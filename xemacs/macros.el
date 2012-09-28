;;
;; macros.el
;; Login : <s@taj>
;; Started on  Fri Nov 26 00:35:02 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;
(eval-when-compile
  (require 'cl))


;; http://stackoverflow.com/questions/4283899/lisp-macro-set-or-nconc-not-able-to-get-working
(defmacro set-or-nconc (var &rest args)
  `(if (and (boundp ',var) (not (null ,var)))
       (nconc ,var ,@args)
     (setq ,var ,@args)))


(defmacro set-assoc (key val alist)
  `(progn
     (when (null (assoc ,key ,alist))
       (setq ,alist (acons ,key nil ,alist)))
     (setcdr (assoc ,key ,alist) ,val)))
;;{{{ start: http://emacs-fu.blogspot.com/2008/12/using-packages-functions-only-if-they.html


(defmacro require-maybe (feature &optional file)
  "*Try to require FEATURE, but don't signal an error if `require' fails."
  `(require ,feature ,file 'noerror))
(defmacro when-available (func foo)
  "*Do something if FUNCTION is available."
  `(when (fboundp ,func) ,foo))
;;;}}}

(eval-when-compile
 (defmacro GNUEmacs (&rest x)
  `(if (string-match "GNU Emacs 20" (version)) x))

(defmacro XEmacs (&rest x)
  (list 'if (string-match "XEmacs 20" (version)) (cons 'progn x)))
(defmacro Xlaunch (&rest x)
  (list 'if (eq window-system 'x)(cons 'progn x))))


;;{{{ http://www.emacswiki.org/emacs/dot-emacs-helper.el
;; Excellent
;; (add-to-list 'load-path "~/.xemacs/pkgrepos/world/deh")

(eval-when-compile
;;   (unless (require 'dot-emacs-helper nil t)
    ;; (defmacro deh-require-maybe (feature &rest forms)
    ;;   (declare (indent 1))
    ;;   `(progn
    ;;      (when ,(if (consp feature)
    ;;                 (cond
    ;;                   ((or (equal (car feature) 'or)
    ;;                        (equal (car feature) 'and))
    ;;                    `(,(car feature) ,@(mapcar (lambda (f) `(require ',f nil t)) (cdr feature))))
    ;;                   (t feature))
    ;;                 `(require ',feature nil t))
    ;;        ,@(if (stringp (car forms))
    ;;              (cdr forms)
    ;;              forms))))

  (defmacro deh-featurep (feature &rest forms)
      (declare (indent 1))
      (labels ((refine (feature)
                 (if (consp feature)
                     (cond
                       ((or (equal (car feature) 'or)
                            (equal (car feature) 'and))
                        `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                       (t feature))
                     `(featurep ',feature))))
        `(progn
           (when ,(refine feature)
             ,@(if (stringp (car forms))
                   (cdr forms)
                   forms)))))


    (defmacro deh-require-maybe (feature &rest forms)
      (declare (indent 1))
      (labels ((refine (feature)
                 (if (consp feature)
                     (cond
                       ((or (equal (car feature) 'or)
                            (equal (car feature) 'and))
                        `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                       (t feature))
                     `(require ',feature nil t))))
        `(progn
           (when ,(refine feature)
             ,@(if (stringp (car forms))
                   (cdr forms)
                   forms)))))

    (defmacro deh-require-mustbe (feature &rest forms)
      (declare (indent 1))
      (labels ((refine (feature)
                 (if (consp feature)
                     (cond
                       ((or (equal (car feature) 'or)
                            (equal (car feature) 'and))
                        `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                       (t feature))
                     `(require ',feature nil nil))))
        `(progn
           (when ,(refine feature)
             ,@(if (stringp (car forms))
                   (cdr forms)
                   forms)))))

    (defalias 'deh-require 'deh-require-maybe)

    (put 'deh-require 'lisp-indent-function 1)

    (defmacro deh-section (section &rest forms)
      (declare (indent 1))
      `(progn ,@forms)))
;;))
;; (deh-require 'feature-name
;;   configuration-for-the-feature)
;; (deh-section "section-name"
;;   some-configuration)
;;}}}

