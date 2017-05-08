;;; org-context-clocking-api-recursive.el --- org-context-clocking-api               -*- lexical-binding: t; -*-

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


;; "org entries access api for recursive task"

(defvar org-entry-tree-task-infos nil "org entry task infos")
(defun org-entry-get-task-infos (files)
  )
(defvar org-entry-tree-task-info-root-org-file nil "org-entry-tree-task-info-root-org-file")

(setq org-entry-tree-task-info-root-org-file
      (expand-file-name "start.org" (task-party-base-dir)))

(defun org-entry-tree-update-task-infos (&optional force)
  (interactive "P")
  (if org-entry-tree-task-info-root-org-file
      (if (file-exists-p org-entry-tree-task-info-root-org-file)
          (unless (and (not force)
                       org-entry-tree-task-infos)
            (setq org-entry-tree-task-infos
                  (org-entry-tree-get-task-infos
                   org-entry-tree-task-info-root-org-file)))
          (message "file %s not exists." org-entry-tree-task-info-root-org-file))
      (message "org-entry-tree-task-info-root-org-file is nil"))
  org-entry-tree-task-infos)

(defun org-entry-tree-map-subheading (fun)
  "Call FUN for every heading underneath the current one."
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
    collection))

(defun org-entry-tree-build (collector &optional file)
  "org-collect-task-info"
  (with-current-buffer (if file
                           (find-file-noselect file)
                           (current-buffer))
    (if file (goto-char (point-min)))
    (let* ((entry (funcall collector))
           (sub-tree
            (append
             (org-entry-tree-map-subheading 'org-entry-tree-collect-task-info)
             (let* ((file (if file file (buffer-file-name)))
                    (subtree-file
                     (org-entry-task-info-get-property entry :SUBTREEFILE))
                    (subtree-file
                     (if (and subtree-file
                              (file-relative-name subtree-file))
                         (expand-file-name subtree-file
                                           (if file
                                               (file-name-directory file)
                                               default-directory))
                         subtree-file)))
               (if (and
                    subtree-file
                    (file-readable-p subtree-file))
                   (list
                    (org-entry-tree-collect-task-info subtree-file)))))))
      (if sub-tree
          (org-entry-task-info-set-property entry :sub-tree sub-tree)
          entry))))

(defun org-entry-tree-collect-task-info (&optional file)
  (org-entry-tree-build 'org-entry-collect-task-info file))

(defun org-entry-tree-task-infos-tree (file)
  (org-entry-tree-collect-task-info file))

(defun org-entry-tree-get-task-infos (file)
  (let ()
    (org-entry-tree-collect-task-info file)))

(defun org-entry-tree-task-node-p (tx)
  (org-entry-task-info-get-property tx :sub-tree))

(progn ;; "tree api"
  (defun tree-mapcar-nodes (nodep fn tree args)
    (list
     (funcall fn tree args)
     :sub-tree
     (mapcar
      '(lambda (e)
        (tree-mapcar-nodes nodep fn e args))
      (funcall nodep tree))))

  (defun tree-mapc-nodes (nodep fn tree args)
    (funcall fn tree args)
    (mapc
     '(lambda (e)
       (tree-mapc-nodes nodep fn e args))
     (funcall nodep tree)))

  (defun tree-remove-if-not-nodes (nodep fn tree args)
    (if (funcall nodep tree)
        (let ((rootele
               (if (funcall fn tree args) tree))
              (subtree
               (remove
                nil
                (mapcar
                 '(lambda (e)
                   (tree-remove-if-not-nodes nodep fn e args))
                 (funcall nodep tree)))))
          (if (or rootele subtree)
              (plist-put tree :sub-tree subtree)))
        (if (funcall fn tree args) tree)))

  (defun tree-mapcar-task-infos (fn tree args)
    (tree-mapcar-nodes
     'org-entry-tree-task-node-p fn tree args))

  (defun tree-mapc-task-infos (fn tree args)
    (tree-mapc-nodes
     'org-entry-tree-task-node-p fn tree args))

  (defun tree-remove-if-not-task-infos (fn tree args)
    (tree-remove-if-not-nodes
     'org-entry-tree-task-node-p fn tree args))

  ;; (testing
  ;;  (setq
  ;;   testxx-remove
  ;;   (tree-remove-if-not-task-infos
  ;;    '(lambda (e) (eq (plist-get e :pre-blank) 4))
  ;;    testxx))

  ;;  (setq testxxmapcar
  ;;        (tree-mapcar-nodes '(lambda (tx) (plist-get tx :sub-tree))
  ;;                           '(lambda (tx) (plist-get tx :title))
  ;;                           ;; testxx
  ;;                           (car (plist-get testxx :sub-tree))
  ;;                           ))

  ;;  (setq testxxmapc
  ;;        (tree-mapc-nodes '(lambda (tx) (plist-get tx :sub-tree))
  ;;                         '(lambda (tx) (plist-get tx :title))
  ;;                         ;; testxx
  ;;                         (car (plist-get testxx :sub-tree))
  ;;                         )))
  )


(provide 'org-context-clocking-api-recursive)
;;; org-context-clocking-api-recursive.el ends here
