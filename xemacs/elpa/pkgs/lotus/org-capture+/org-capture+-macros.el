;;; org-capture+-macros.el --- Org capture plus macos  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
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

(provide 'org-capture+-macros)


(defmacro with-org-capture-run (marker type target template plist before-body &rest after-body)
  `(let* ((before-finalize #'(lambda (,marker) ,before-body))
          (after-finalize  #'(lambda (,marker) ,@after-body))
          (plist (append
                  (list :before-finalize before-finalize
                        :after-finalize  after-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-run
            ,type
            ,target
            ,template
            plist)))
(put 'with-org-capture-run 'lisp-indent-function 6)

;; (defmacro with-org-capture-run (marker type target template plist before-body &rest after-body)
;;   `(with-org-capture-run ,marker ,type ,target ,template ,plist ,before-body ,@after-body))
;; (put 'with-org-capture-run 'lisp-indent-function 6)


(defmacro after-org-capture-run (marker type target template plist &rest body)
  `(let* ((after-finalize #'(lambda (,marker) ,@body))
          (plist (append
                  (list :after-finalize after-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-run
            ,type
            ,target
            ,template
            plist)))
(put 'after-org-capture-run 'lisp-indent-function 5)

;; (defmacro after-org-capture-run (marker type target template plist &rest body)
;;   `(after-org-capture-run ,marker ,type ,target ,template ,plist ,@body))
;; (put 'with-org-capture-run 'lisp-indent-function 5)


(defmacro before-org-capture-run (marker type target template plist &rest body)
  `(let* ((before-finalize #'(lambda (,marker) ,@body))
          (plist (append
                  (list :before-finalize before-finalize)
                  ,plist)))
     (org-capture+-debug :debug "plist %s \n" plist)
     (apply #'org-capture-run
            ,type
            ,target
            ,template
            plist)))
(put 'before-org-capture-run 'lisp-indent-function 5)

;; (defmacro before-org-capture-run (marker type target template plist &rest body)
;;   `(before-org-capture-run ,marker ,type ,target ,template ,plist ,@body))
;; (put 'with-org-capture-run 'lisp-indent-function 5)

;;; org-capture+-macros.el ends here
