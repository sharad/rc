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


(defvar org-capture+-helm-templates-alist
  '(("TODO"
     "* TODO %? %^g\n %i\n [%a]\n"
     "* MILESTONE %? %^g\n %i\n [%a]\n")
    ("MEETING"
     "* MEETING %? %^g\n %i\n [%a]\n")))

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



(defmacro with-org-capture-plus (type target template plist &rest body)
  `(let* ((finalize #'(lambda () ,@body))
          (plist (append
                  (list :finalize finalize)
                  ,plist)))
     (lwarn 'occ-capture+ :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'with-org-capture-plus 'lisp-indent-function 4)

(defmacro with-org-capture+ (type target template plist &rest body)
  `(with-org-capture-plus ,type ,target ,template ,plist ,@body))
(put 'with-org-capture+ 'lisp-indent-function 4)



(defmacro after-org-capture-plus (type target template plist &rest body)
  `(let* ((after-finalize #'(lambda () ,@body))
          (plist (append
                  (list :after-finalize after-finalize)
                  ,plist)))
     (lwarn 'occ-capture+ :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'after-org-capture-plus 'lisp-indent-function 4)

(defmacro after-org-capture+ (type target template plist &rest body)
  `(after-org-capture-plus ,type ,target ,template ,plist ,@body))
(put 'with-org-capture+ 'lisp-indent-function 4)


(defmacro before-org-capture-plus (type target template plist &rest body)
  `(let* ((before-finalize #'(lambda () ,@body))
          (plist (append
                  (list :before-finalize before-finalize)
                  ,plist)))
     (lwarn 'occ-capture+ :debug "plist %s \n" plist)
     (apply #'org-capture-plus
            ,type
            ,target
            ,template
            plist)))
(put 'before-org-capture-plus 'lisp-indent-function 4)

(defmacro before-org-capture+ (type target template plist &rest body)
  `(before-org-capture-plus ,type ,target ,template ,plist ,@body))
(put 'with-org-capture+ 'lisp-indent-function 4)

;;; org-capture+-helm.el ends here
