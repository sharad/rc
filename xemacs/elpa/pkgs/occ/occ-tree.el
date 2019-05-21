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


(progn ;; "tree api"
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
  )



(progn
  (defun occ-tree-tsk-node-p (tx)
    "Test org TX is org tsks tree non-leaf node"
    (occ-get-property tx 'subtree))

  (defun occ-tree-tsk-subtree (tx)
    "Test org TX is org tsks tree non-leaf node"
    (occ-get-property tx 'subtree))

  (defun occ-mapcar-tree-tsks (fn tree args)
    "Tree mapcar return result for FN for all TREE nodes with ARGS"
    (tree-mapcar-nodes
     'occ-tree-tsk-subtree fn tree args))

  (defun occ-mapc-tree-tsks (fn tree args)
    "Tree mapc run FN for all TREE nodes with ARGS"
    (tree-mapc-nodes
     'occ-tree-tsk-subtree fn tree args))

  (defun occ-remove-if-not-tree-tsks (fn tree args)
    "Tree remove if return TREE with all node and its subtree removed if node return nil for PREDICATE"
    (tree-remove-if-not-nodes
     'occ-tree-tsk-subtree fn tree args)))


(defun occ-org-map-subheading (fun)
  "Call FUN for every heading underneath the current heading"
  ;; (org-back-to-heading)
  (let ((level (funcall outline-level))
        (collection nil))
    (save-excursion
      (while (and (progn
                    (outline-next-heading)
                    (> (funcall outline-level) level))
                  (not (eobp)))
        (if (= (funcall outline-level) (1+ level))
            (push (funcall fun) collection))))
    (nreverse collection)))

(defun occ-tree-tsk-build (tsk-builder &optional file subtree-level)
  "Build recursive org tsks from org FILE (or current buffer) using TSK-BUILDER function e.g. occ-collect-tsk"
  (with-current-buffer (if file
                           (find-file-noselect file)
                         (current-buffer))
    (if file (goto-char (point-min)))
    (let ((entry         (funcall tsk-builder))
          (subtree-level (if subtree-level subtree-level 0)))
      (when (numberp subtree-level)
        (occ-set-property entry 'subtree-level subtree-level))
      (cl-assert (numberp subtree-level))
      (when entry
        (let* ((sub-tree
                (append
                 (occ-org-map-subheading #'(lambda ()
                                             (occ-tree-tsk-build tsk-builder nil subtree-level)))
                 (let ((subtree-file-prop (occ-get-property entry :SUBTREEFILE)))
                   (when subtree-file-prop
                     (let* ((file (if file file (buffer-file-name)))
                            (subtree-file
                             (if (and subtree-file-prop
                                      (file-relative-name subtree-file-prop))
                                 (expand-file-name subtree-file-prop
                                                   (if file
                                                       (file-name-directory file)
                                                     default-directory))
                               subtree-file)))
                       (if (and
                            subtree-file
                            (file-readable-p subtree-file))
                           (list
                            (occ-tree-tsk-build tsk-builder subtree-file (+ (or (occ-get-property entry 'level) 0)
                                                                            (or subtree-level 0)))))))))))
          (when sub-tree      (occ-set-property entry 'subtree sub-tree))
          entry)))))

;; (defun occ-tree-tsk-build (tsk-builder &optional file)
;;   "Build recursive org tsks from org FILE (or current buffer) using TSK-BUILDER function e.g. occ-collect-tsk"
;;   (with-current-buffer (if file
;;                            (find-file-noselect file)
;;                          (current-buffer))
;;     (if file (goto-char (point-min)))
;;     (let ((entry (funcall tsk-builder)))
;;       (when entry
;;         (let* ((sub-tree
;;                 (append
;;                  (occ-org-map-subheading #'(lambda ()
;;                                              (occ-tree-tsk-build tsk-builder nil)))
;;                  (let ((subtree-file-prop (occ-get-property entry :SUBTREEFILE)))
;;                    (when subtree-file-prop
;;                      (let* ((file (if file file (buffer-file-name)))
;;                             (subtree-file
;;                              (if (and subtree-file-prop
;;                                       (file-relative-name subtree-file-prop))
;;                                  (expand-file-name subtree-file-prop
;;                                                    (if file
;;                                                        (file-name-directory file)
;;                                                      default-directory))
;;                                subtree-file)))
;;                        (if (and
;;                             subtree-file
;;                             (file-readable-p subtree-file))
;;                            (list
;;                             (occ-tree-tsk-build tsk-builder subtree-file)))))))))
;;           (when sub-tree      (occ-set-property entry 'subtree sub-tree))
;;           entry)))))

;;; occ-tree.el ends here
