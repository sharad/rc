;;; basic-macros.el --- Basic macros                 -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <spratap@merunetworks.com>
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

;; (eval-when-compile
;;   (require 'cl nil nil)


;;   (unless (require 'dot-emacs-helper nil t)
;;     (defmacro deh-require-maybe (feature &rest forms)
;;       (declare (indent 1))
;;       `(progn
;;          (when ,(if (consp feature)
;;                     (cond
;;                       ((or (equal (car feature) 'or)
;;                            (equal (car feature) 'and)
;;                            (equal (car feature) 'progn))
;;                        `(,(car feature) ,@(mapcar (lambda (f) `(require ',f nil t)) (cdr feature))))
;;                       (t feature))
;;                     `(require ',feature nil t))
;;            ,@(if (stringp (car forms))
;;                  (cdr forms)
;;                  forms))))

;;;###autoload
(defmacro deh-featurep (feature &rest forms)
  "Dot Emacs featurep"
  (declare (indent 1))
  (cl-labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(featurep ',feature))))
    `(progn
       (when ,(refine feature)
         ,@(if (stringp (car forms))
               (cdr forms)
               forms)))))

;;;###autoload
(defmacro deh-require-or-act (feature act &rest forms)
  "Dot Emacs require or act"
  ;; master
  (declare (indent 1))
  (cl-labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(unless (require ',feature nil t)
                    (funcall ,act ',feature)))))
    `(progn
       (if ,(refine feature)
           (,@(if (stringp (car forms))
                  (cdr forms)
                  forms))
           ))))

;;;###autoload
(defmacro deh-require-or-package-install (feature &rest forms)
  "Dot Emacs require or package install"
  (declare (indent 2))
  `(deh-require-or-act ,feature
     (lambda (p) (package-install p))
     forms))

;;;###autoload
(defmacro deh-require-todo (feature todo-if-no-feature &rest forms)
  "Dot Emacs require TODO"
  (declare (indent 1))
  (cl-labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(require ',feature nil t))))
    `(progn
       (if ,(refine feature)
           (,@(if (stringp (car forms))
                  (cdr forms)
                  forms))
           ,todo-if-no-feature))))

;;;###autoload
(defmacro deh-require-maybe (feature &rest forms)
  "Dot Emacs require Maybe"
  (declare (indent 1))
  (cl-labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(require ',feature nil t))))
    `(progn
       (when ,(refine feature)
         ,@(if (stringp (car forms))
               (cdr forms)
               forms)))))

;;;###autoload
(defmacro deh-require-mustbe (feature &rest forms)
  "Dot Emacs require Must"
  (declare (indent 1))
  (cl-labels ((refine (feature)
             (if (consp feature)
                 (cond
                   ((or (equal (car feature) 'or)
                        (equal (car feature) 'and)
                        (equal (car feature) 'progn))
                    `(,(car feature) ,@(mapcar #'refine (cdr feature))))
                   (t feature))
                 `(require ',feature nil nil))))
    `(progn
       (when ,(refine feature)
         ,@(if (stringp (car forms))
               (cdr forms)
               forms)))))

;;;###autoload
(defalias 'deh-require 'deh-require-maybe)
(put 'deh-require 'lisp-indent-function 1)

;;;###autoload
(defmacro deh-section (section &rest forms)
  "Dot Emacs Section"
  (declare (indent 1))
  `(progn ,@forms))
;;  )
;;))
;; (deh-require 'feature-name
;;   configuration-for-the-feature)
;; (deh-section "section-name"
;;   some-configuration)
;;}}}


;; (defmacro with-report-error (msg &rest body)
;;   (declare (debug t) (indent 0))
;;   ;;(unwind-protect BODYFORM UNWINDFORMS...)
;;     (let ((err (make-symbol "err"))
;;           (form (make-symbol "form")))
;;       ;; `(condition-case-no-debug ,err
;;       ;; `(condition-case ,err
;;       ;;      (progn ,@body)
;;       ;;    (error (message "Error: %s - %s in %s" msg ,err ,body)
;;       ;;           nil))

;;       `(dolist (,form ,body)
;;          `(condition-case ,,err
;;               ,,form
;;             (error (message "Error: %s - %s in %s" ,,msg ,,err ,,form)
;;                    nil)))

;;       )
;;     ;; `(condition-case e
;;     ;;    ,@forms
;;     ;;  (error "%s" ,forms))
;;     )


;;;###autoload
;; (defmacro with-report-error (msg &rest body)
;;   "run body and report error in echo area and message buffer"
;;   (declare (debug t) (indent 4))
;;   ;;(unwind-protect BODYFORM UNWINDFORMS...)
;;   (let ((err  (make-symbol "err"))
;;         (form (make-symbol "form")))
;;     `(progn
;;        ,@(mapcar
;;           `(lambda (,,form)
;;              ;; `(condition-case-no-debug ,err
;;              `(condition-case ,err
;;                   ,,form
;;                 (error (message "Error: %s - %s in %s" ,,msg ,,err ',,,form)
;;                    nil)))
;;           body))))


;;;###autoload
(defmacro with-report-error (msg &rest body)
  "run body and report error in echo area and message buffer"
  (declare (debug t) (indent 4))
  ;;(unwind-protect BODYFORM UNWINDFORMS...)
  (let ((err  (make-symbol "err"))
        (form (make-symbol "form")))
    `(progn
       ,@(mapcar
          (lambda (form)
            ;; `(condition-case-no-debug ,err
            `(condition-case ,err
                 ,form
               (error
                (message "Error: %s - %s in %s" ,msg ,err ',form)
		      nil)))
          body))))

'(testing
  (with-report-error "check"
      (message "tset")
      (message "test"))

  (macroexpand '(with-report-error "check" (x) (y))))



;; (defmacro with-lock (lock lock-body block-repeat-interval block-maxtime)
;;   `(if
;;     (progn
;;       (setq ,lock t)
;;       ,lock-body
;;       (setq ,lock nil)))))






(provide 'basic-macros)
;;; basic-macros.el ends here
