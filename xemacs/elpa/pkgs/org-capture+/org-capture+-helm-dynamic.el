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

;;;###autoload
(defvar org-capture+-helm-templates-plist nil)
;;;###autoload
(defvar org-capture+-helm-templates-tree  '(t))


;; (setq org-capture+-helm-templates-plist '(:todo))

;; (setq org-capture+-helm-templates-tree   (list t))

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
    (tree-flatten #'org-capture+-template-p templates-tree)))

(defun collect-alist (alist)
  (let ((ulist nil))
    (dolist (pair (copy-tree alist))
      (if (assoc (car pair) ulist)
          (nconc (assoc (car pair) ulist) (cdr pair))
        (setf ulist (append ulist (list pair)))))
    ulist))

(defun delete-dups-alist (alist)
  (dolist (pair alist)
    (setcdr pair (delete-dups (cdr pair))))
  alist)


;;;###autoload
(org-capture+-add-heading-template '(xx) "TODO"    "* TODO %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(zz) "TODO"    "* WRITING %? %^g\n %i\n [%a]\n")
;;;###autoload
(org-capture+-add-heading-template '(yy) "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


;; TODO: keyword replacement
(defun org-capture+-collect-templates-alist (fn arg level)
  (let* ((fn    (or fn #'org-capture+-tree-predicate))
         (alist (mapcar #'cadr
                        (org-capture+-collect-template-alist fn arg level))))
    (let ((templates-alist (collect-alist alist)))
      (delete-dups-alist templates-alist))))

(defun org-capture+-collect-templates (fn arg level)
  (let* ((fn    (or fn   #'org-capture+-tree-predicate))
         (alist (org-capture+-collect-templates-alist fn arg level)))
    (let ((templates (apply #'append
                            (mapcar #'cdr alist))))
      templates)))


(defun org-capture+-tree-predicate (key-tree arg)
  (memq (car key-tree) arg))


;; (org-capture+-collect-templates-alist nil nil 0)
;; (org-capture+-collect-templates nil nil 0)


(defun helm-template-gen-selector (predicate arg level &optional noclass)
  (let* ((list            (if noclass
                              (org-capture+-collect-templates predicate arg level)
                              (org-capture+-collect-templates-alist predicate arg level)))
         (level           (or level     0))
         (classes         (mapcar #'car
                                  (org-capture+-collect-templates-alist #'(lambda (fn arg) t)
                                                                        '(t)
                                                                        0)))
         (arg             (or arg       '(t)))
         (calculate-list  (if noclass
                              #'(lambda ()
                                  (setq list (org-capture+-collect-templates predicate arg level)))
                              #'(lambda ()
                                  (setq list (org-capture+-collect-templates-alist predicate arg level)))))
         (predicate       (or predicate #'org-capture+-tree-predicate))
         (level-inc-fn    #'(lambda ()
                              (interactive)
                              (setf level (1+ level))
                              (helm-refresh)))
         (level-dec-fn    #'(lambda ()
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
         (h-candidate-transformer (if noclass
                                      #'(lambda (candidates source)
                                          (funcall calculate-list)
                                          list)
                                    #'(lambda (candidates source)
                                        (funcall calculate-list)
                                        (let ((name (cdr (assoc 'name source))))
                                          (cdr (assoc name list))))))
         (sources (if noclass
                      (helm-build-sync-source           "templates"
                        :keymap                         h-map
                        ;; :requires-pattern nil
                        ;; :match (list #'(lambda (c) t))
                        :candidates                     list
                        :multiline                      t
                        :filtered-candidate-transformer h-candidate-transformer
                        ;; :filter-one-by-one #'h-candidate-transformer
                        :action-transformer             h-action-transformer)
                      (mapcar #'(lambda (class)
                                  (helm-build-sync-source           class
                                    :keymap                         h-map
                                    ;; :requires-pattern nil
                                    ;; :match (list #'(lambda (c) t))
                                    :candidates                     (cdr (assoc class list))
                                    :multiline                      t
                                    :filtered-candidate-transformer h-candidate-transformer
                                    ;; :filter-one-by-one #'h-candidate-transformer
                                    :action-transformer             h-action-transformer))
                              classes))))
    #'(lambda ()
        (helm :sources sources))))


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

  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t xx yy)
                               0))
  (funcall
   (helm-template-gen-selector #'org-capture+-tree-predicate
                               '(t occ tsk todo meeting)
                               0))

  (org-capture+-collect-templates-alist #'org-capture+-tree-predicate
                                        '(t xx yy)
                                        1))





;; (funcall (helm-template-gen-selector #'org-capture+-tree-predicate '(t xx yy) nil))
(when nil
  (helm :sources (list (helm-build-sync-source "Templates0" :candidates '(c d))
                       (helm-build-sync-source "Templates1" :candidates nil)
                       (helm-build-sync-source "Templates2" :candidates '(a b))))

  (helm :sources (list (helm-build-sync-source "Templates1" :candidates nil))))

;;; org-capture+-helm-dynamic.el ends here
