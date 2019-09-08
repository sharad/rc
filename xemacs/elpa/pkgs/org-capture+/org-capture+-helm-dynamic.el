;;; org-capture+-helm-dynamic.el --- Org capture helm dynamic  -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <spratap@merunetworks.com>
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

(provide 'org-capture+-helm-dynamic)


(require 'dash)
(require 'helm)


(require 'lotus-tree-manager)


;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

;;;###autoload
(defvar org-capture+-helm-templates-plist nil)
;;;###autoload
(defvar org-capture+-helm-templates-tree  '(t))


;;;###autoload
(defun org-capture+-add-heading-template (keys heading &rest templates)
  (tree-add keys
            (list :template (cons heading templates))
            org-capture+-helm-templates-tree))

(defun org-capture+-template-p (template)
  (eql :template (car template)))

(defun org-capture+-tree-gen-predicate (predicate arg)
  #'(lambda (key)
      (funcall predicate key arg)))

(defun org-capture+-collect-template-alist (predicate arg level)
  (let* ((level (or level 0))
         (templates-tree
          (collect-elem-cond-depth org-capture+-helm-templates-tree
                                   #'org-capture+-template-p
                                   (org-capture+-tree-gen-predicate predicate arg)
                                   level)))
    (tree-flatten #'org-capture+-template-p
                  templates-tree)))

;; TODO: keyword replacement
(defun org-capture+-collect-templates-alist (fn arg level)
  (let* ((fn    (or fn #'org-capture+-tree-predicate))
         (alist (mapcar #'cadr
                        (org-capture+-collect-template-alist fn arg level))))
    (let ((templates-alist (collect-alist alist)))
      ;; (delete-dups-alist templates-alist)
      templates-alist)))

(defun org-capture+-collect-templates (fn arg level)
  (let* ((fn    (or fn   #'org-capture+-tree-predicate))
         (alist (org-capture+-collect-templates-alist fn arg level)))
    (let ((templates (apply #'append
                            (mapcar #'cdr alist))))
      templates)))

(defun org-capture+-collect-template-classes ()
  (mapcar #'car
          (org-capture+-collect-templates-alist #'(lambda (key-tree arg) t)
                                                '(t)
                                                0)))

(defun org-capture+-tree-predicate (key-tree arg)
  (memq (car key-tree) arg))


;;;###autoload
(org-capture+-add-heading-template '(xx) "TODO"    "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(zz) "TODO"    "* WRITING %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(yy) "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


(defun helm-template-gen-source (predicate arg level &optional action noclass)
  (let* ((action          (or action #'indentity))
         (list            (if noclass
                              (org-capture+-collect-templates predicate arg level)
                            (org-capture+-collect-templates-alist predicate arg level)))
         (default-level   (or level     0))
         (arg             (or arg       '(t)))
         (default-arg     arg)
         (classes         (org-capture+-collect-template-classes))
         (calculate-list  (if noclass
                              #'(lambda ()
                                  (setq list (org-capture+-collect-templates predicate arg level)))
                              #'(lambda ()
                                  (let ((xlist (org-capture+-collect-templates-alist predicate arg level)))
                                    (if list
                                        (when (car xlist)
                                          (setcar list (car xlist))
                                          (setcdr list (cdr xlist)))
                                      (setq list xlist))))))

         (predicate       (or predicate #'org-capture+-tree-predicate))
         (level-reset-fn  #'(lambda ()
                              (interactive)
                              (setf level default-level)
                              (funcall calculate-list)
                              (helm-refresh)))
         (level-inc-fn    #'(lambda ()
                              (interactive)
                              (setf level (1+ level))
                              (funcall calculate-list)
                              (helm-refresh)))
         (level-dec-fn    #'(lambda ()
                              (interactive)
                              (setf level (1- level))
                              (funcall calculate-list)
                              (helm-refresh)))
         (h-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map helm-map)
            (define-key map (kbd "M-<up>")     level-inc-fn)
            (define-key map (kbd "M-<down>")   level-dec-fn)
            map))
         (h-action-transformer    #'(lambda (actions candidate)
                                      (list (cons "Select" action))))
         (h-candidate (if noclass
                          #'(lambda ()
                              (funcall calculate-list)
                              list)
                        #'(lambda ()
                            (funcall calculate-list)
                            (let* ((name        (cdr (assoc 'name source)))
                                   (ncandidates (cdr (assoc name list))))
                              ncandidates))))
         ;; (h-candidate-transformer (if noclass
         ;;                              #'(lambda (candidates source)
         ;;                                  candidates)
         ;;                            #'(lambda (candidates source)
         ;;                                (message "candidates length %d" (length candidates))
         ;;                                candidates)))
         (sources (if noclass
                      (helm-build-sync-source           "templates"
                        :keymap                         h-map
                        ;; :requires-pattern t
                        ;; :match (list #'(lambda (c) t))
                        :candidates                     h-candidate ;; list
                        :multiline                      t
                        :filtered-candidate-transformer h-candidate-transformer
                        ;; :filter-one-by-one #'h-candidate-transformer
                        :action-transformer             h-action-transformer)
                      (mapcar #'(lambda (class)
                                  (helm-build-sync-source           class
                                    :keymap                         h-map
                                    ;; :requires-pattern t
                                    ;; :match (list #'(lambda (c) t))
                                    :candidates                     #'(lambda () (cdr (assoc class list)))
                                    :multiline                      t
                                    ;; :filtered-candidate-transformer h-candidate-transformer
                                    ;; :filter-one-by-one #'h-candidate-transformer
                                    :action-transformer             h-action-transformer))
                              classes))))
    sources))


;; (setq testzxx (helm-build-sync-source "Templates"))
;; (assoc 'name testzxx)

;; (funcall (helm-template-gen-selector #'org-capture+-tree-predicate
;;                                      '(t occ tsk todo meeting)
;;                                      0))



(when nil

  org-capture+-helm-templates-tree


  (org-capture+-collect-templates-alist #'(lambda (fn arg) t)
                                        '(t)
                                        0)

  (collect-alist
   (mapcar #'cadr
           (org-capture+-collect-template-alist #'org-capture+-tree-predicate
                                                '(t xx yy)
                                                0)))

  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t xx yy)
                               0))

  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t xx yy)
                               0
                               t))

  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t occ tsk todo meeting)
                               0))

  (org-capture+-collect-templates-alist #'org-capture+-tree-predicate
                                        '(t xx yy)
                                        1))




;;; org-capture+-helm-dynamic.el ends here
