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


(require 'tree-lib)


;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(defvar org-capture+-helm-templates-plist nil)
(defvar org-capture+-helm-templates-tree  '(t))


(setq org-capture+-helm-templates-plist '(:todo))

(setq org-capture+-helm-templates-tree   (list t))

;; (defun org-capture+-add-template (keys template)
;;   (tree-add keys
;;             (list :template template)
;;             org-capture+-helm-templates-tree))

(defun org-capture+-add-heading-template (keys heading &rest templates)
  (tree-add keys
            (list :template (cons heading templates))
            org-capture+-helm-templates-tree))

(defun org-capture+-template-p (template)
  (eql :template (car template)))

(defun org-capture+-tree-predicate (key-tree arg)
  (memq (car key-tree) arg))

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
    (-flatten-n 1 templates-tree)))

(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair (copy-tree alist))
      (if (assoc (car pair) ulist)
          (nconc (assoc (car pair) ulist) (cdr pair))
        ;; (pushnew pair ulist)
        (setf ulist (append ulist (list pair)))))
    ulist))


(org-capture+-add-heading-template '(xx) "TODO"    "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-add-heading-template '(zz) "TODO"    "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-add-heading-template '(yy) "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


;; TODO: keyword replacement
(defun org-capture+-collect-templates-alist (fn arg level)
  (let* ((fn    (or fn #'org-capture+-tree-predicate))
         (arg   (or arg '(t xx yy)))
         (alist (mapcar #'cadr
                        (org-capture+-collect-template-alist fn arg level))))
    (let ((templates-alist (collect-alist alist)))
      templates-alist)))

(defun org-capture+-collect-templates (fn arg level)
  (let* ((fn    (or fn #'org-capture+-tree-predicate))
         (arg   (or arg '(t xx yy)))
         (alist (mapcar #'cadr
                        (org-capture+-collect-template-alist fn arg level))))
    (let ((templates (apply #'append (mapcar #'cdr (collect-alist alist)))))
      templates)))


;; (org-capture+-collect-templates-alist nil nil 0)
;; (org-capture+-collect-templates nil nil 0)


(defun helm-template-gen-selector (predicate arg level)
  (let* ((level        (or level     0))
         (arg          (or arg       '(t xx yy)))
         (predicate    (or predicate #'org-capture+-tree-predicate))
         (level-inc-fn #'(lambda ()
                           (interactive)
                           (setf level (1+ level))
                           (helm-refresh)))
         (level-dec-fn #'(lambda ()
                           (interactive)
                           (setf level (1- level))
                           (helm-refresh)))
         (h-map
          (let ((map (make-sparse-keymap)))
            (set-keymap-parent map helm-map)
            (define-key map (kbd "M-<up>")     level-inc-fn)
            (define-key map (kbd "M-<down>")   level-dec-fn)
            map))
         (h-action-transformer    #'(lambda (actions candidate)
                                      '(("Even" . identity))))
         (h-candidates            #'(lambda ()
                                      (org-capture+-collect-templates predicate arg level)))
         (h-candidate-transformer #'(lambda (candidates source)
                                      (org-capture+-collect-templates predicate arg level)))
         (source (helm-build-sync-source "Templates"
                   :keymap                         h-map
                   ;; :requires-pattern nil
                   ;; :match (list #'(lambda (c) t))
                   :candidates                     h-candidates
                   :multiline                      t
                   :filtered-candidate-transformer h-candidate-transformer
                   ;; :filter-one-by-one #'h-candidate-transformer
                   :action-transformer             h-action-transformer)))
    #'(lambda ()
        (helm :sources source))))

;; (funcall (helm-template-gen-selector #'org-capture+-tree-predicate '(t xx yy) nil))
(when nil
  (helm :sources (list (helm-build-sync-source "Templates0" :candidates '(c d))
                       (helm-build-sync-source "Templates1" :candidates nil)
                       (helm-build-sync-source "Templates2" :candidates '(a b))))

  (helm :sources (list (helm-build-sync-source "Templates1" :candidates nil))))

;;; org-capture+-helm-dynamic.el ends here
