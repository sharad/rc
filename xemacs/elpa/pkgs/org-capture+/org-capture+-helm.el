;;; org-capture+-helm.el --- org capture+ helm       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'org-capture+-helm)


(require 'helm)


;; * Dynamic Match based templates

(defvar org-capture+-helm-templates-alist nil)


(defun org-capture+-helm-template-add (scope heading template)
  (unless (assoc heading org-capture+-helm-templates-alist)
    (pushnew (list heading) org-capture+-helm-templates-alist))
  (pushnew template
           (cdr (assoc heading org-capture+-helm-templates-alist))))

(org-capture+-helm-template-add 'test "TODO" "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "TODO" "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


(t (pred1 (pred2 "y" "k") "x") (pred3 "z") "a")

(defun max-depth (tree)
  (if (atom tree)
      0
    (1+ (reduce #'max (mapcar #'max-depth tree)))))

(setq xmatch-kk '(t  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))

(defun x-match-preds (depth preds list)
  (if (<= depth (max-depth list))
      ()))

(max-depth xmatch-kk)


(defun collect-elem (list)
  (if (and
       (listp list)
       (consp (cadr list)))
      (mapcar #'(lambda (e) (collect-elem e))
              (cdr list))
    (if (listp list) (cdr list) list)))

(setq xmatch-kk '(t "i" "l"  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))

(defun collect-elem (list)
  (if (listp list)
      (mapcar #'(lambda (e) (collect-elem e))
              (cdr list))
    list))

(defun collect-elem-cond (list nodep predicate)
  (if (funcall nodep list)
      list
    (when (funcall predicate list)
      (remove nil
              (mapcar #'(lambda (e) (collect-elem-cond e nodep predicate))
                      (cdr list))))))


(collect-elem xmatch-kk)

(collect-elem-cond xmatch-kk
                   #'(lambda (x) (not (listp x)))
                   #'(lambda (list)
                       (or
                        (<= (max-depth list) 0)
                        (memq (car list) '(t)))))

(max-depth '(t "i" "l"  (pred1 (pred2 "y" "k") "n" "x") (pred3 "z") "a"))
(max-depth '())


(defun collect-with-depth (tree depth)
  (collect-elem-cond tree
                     #'(lambda (x) (not (listp x)))
                     #'(lambda (list)
                         (or
                          (<= (max-depth list) depth)
                          (memq (car list) '(t))))))

(collect-with-depth xmatch-kk 3)



;;;###autoload
(defun org-capture+-build-helm-template-source (name attrib-list &rest templates)
  `((name . ,name)
    (multiline)
    (candidates ,@templates)
    ,@attrib-list))

;;;###autoload
(defun org-capture+-build-helm-template-sources (attrib-list alist)
  (mapcar
   #'(lambda (e)
       (apply #'org-capture+-build-helm-template-source
              (car e)
              attrib-list
              (cdr e)))
   alist))

;;;###autoload
(defun org-capture+-helm-select-template (&optional attrib-list alist)
  (let ((attrib-list (or attrib-list '((action . identity))))
        (alist       (or alist org-capture+-helm-templates-alist)))
    (helm :sources
          (org-capture+-build-helm-template-sources attrib-list alist))))

;; (org-capture+-helm-select-template)



(defmacro with-org-capture-plus (marker type target template plist before-body &rest after-body)
  `(let* ((before-finalize #'(lambda (,marker) ,before-body))
          (after-finalize  #'(lambda (,marker) ,@after-body))
          (plist (append
                  (list :before-finalize before-finalize
                        :after-finalize  after-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'with-org-capture-plus 'lisp-indent-function 6)

(defmacro with-org-capture+ (marker type target template plist before-body &rest after-body)
  `(with-org-capture-plus ,marker ,type ,target ,template ,plist ,before-body ,@after-body))
(put 'with-org-capture+ 'lisp-indent-function 6)


(defmacro after-org-capture-plus (marker type target template plist &rest body)
  `(let* ((after-finalize #'(lambda (,marker) ,@body))
          (plist (append
                  (list :after-finalize after-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'after-org-capture-plus 'lisp-indent-function 5)

(defmacro after-org-capture+ (marker type target template plist &rest body)
  `(after-org-capture-plus ,marker ,type ,target ,template ,plist ,@body))
(put 'with-org-capture+ 'lisp-indent-function 5)


(defmacro before-org-capture-plus (marker type target template plist &rest body)
  `(let* ((before-finalize #'(lambda (,marker) ,@body))
          (plist (append
                  (list :before-finalize before-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'before-org-capture-plus 'lisp-indent-function 5)

(defmacro before-org-capture+ (marker type target template plist &rest body)
  `(before-org-capture-plus ,marker ,type ,target ,template ,plist ,@body))
(put 'with-org-capture+ 'lisp-indent-function 5)

;;; org-capture+-helm.el ends here
