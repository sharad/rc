;;; occ-tree.el --- occ-api               -*- lexical-binding: t; -*-
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



(provide 'occ-tree)


;; tree api
(defun tree-mapcar-nodes (subtreefn fn tree args)
  "Tree mapcar return result for FN for all TREE nodes with ARGS, function SUBTREEFN require to find nonleaf node"
  (list
   (funcall fn tree args)
   :subtree
   (mapcar
    #'(lambda (e)
        (tree-mapcar-nodes subtreefn fn e args))
    (funcall subtreefn tree))))

(defun tree-mapc-nodes (subtreefn fn tree args)
  "Tree mapc run FN for all TREE nodes with ARGS, function SUBTREEFN require to find nonleaf node"
  (funcall fn tree args)
  (mapc
   #'(lambda (e)
       (tree-mapc-nodes subtreefn fn e args))
   (funcall subtreefn tree)))

(defun tree-remove-if-not-nodes (subtreefn predicate tree args)
  "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE, function SUBTREEFN require to find nonleaf node"
  (if (funcall subtreefn tree)
      (let ((rootele
             (if (funcall predicate tree args) tree))
            (subtree
             (remove
              nil
              (mapcar
               #'(lambda (e)
                   (tree-remove-if-not-nodes subtreefn predicate e args))
               (funcall subtreefn tree)))))
        (if (or rootele subtree)
            (plist-put tree :subtree subtree)))
    (if (funcall predicate tree args) tree)))

;; (testing
;;  (setq
;;   testxx-remove
;;   (tree-remove-if-not-tsks
;;    #'(lambda (e) (eq (plist-get e :pre-blank) 4))
;;    testxx))

;;  (setq testxxmapcar
;;        (tree-mapcar-nodes #'(lambda (tx) (plist-get tx :subtree))
;;                           #'(lambda (tx) (plist-get tx :title))
;;                           ;; testxx
;;                           (car (plist-get testxx :subtree))
;;                           ))

;;  (setq testxxmapc
;;        (tree-mapc-nodes #'(lambda (tx) (plist-get tx :subtree))
;;                         #'(lambda (tx) (plist-get tx :title))
;;                         ;; testxx
;;                         (car (plist-get testxx :subtree))
;;                         )))

;;; occ-tree.el ends here
