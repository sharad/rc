;;; org-capture+-helm.el --- org capture+ helm       -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad
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


(require 'dash)
(require 'helm)


(defvar org-capture+-helm-templates-alist nil)

(defun org-capture+-helm-template-add (scope heading template)
  (unless (assoc heading org-capture+-helm-templates-alist)
    (pushnew (list heading) org-capture+-helm-templates-alist))
  (pushnew template
           (cdr (assoc heading org-capture+-helm-templates-alist))))

(org-capture+-helm-template-add 'test "TODO" "* TODO %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "TODO" "* MILESTONE %? %^g\n %i\n [%a]\n")
(org-capture+-helm-template-add 'test "MEETING" "* MEETING %? %^g\n %i\n [%a]\n")


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


;; * Dynamic Match based templates
;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(defvar org-capture+-helm-templates-plist nil)
(defvar org-capture+-helm-templates-tree  nil)


(setq org-capture+-helm-templates-plist '(:todo))

(setq org-capture+-helm-templates-tree
      (list t))

(defun org-capture+-add-template (keys template)
  (tree-add keys
            (list :template template)
            org-capture+-helm-templates-tree))

(defun org-capture+-template-predicate (template)
  (eql :template (car template)))

(defun org-capture+-tree-predicate (key)
  (cond))

(defun org-capture+-collect-template ()
  (let ((templates-tree
         (collect-elem-cond-depth org-capture+-helm-templates-tree
                                  #'org-capture+-template-predicate
                                  #'org-capture+-collect-predicate)))
    (-flatten templates-tree)))


(defvar h-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>")   'h-sort)
    map)
  "keymap for a helm source.")

(defvar h-sort-fn nil)

(defun h-sort ()
  (interactive))

(defun h-candidates ()
  '("aaaa" "bbb" "ccc"))

(defun h-candidate-transformer (candidates source)
  (reverse (h-candidates)))

(defun h-action-transformer (actions candidate)
  '(("Even" . identity)))

(setq h-source
      (helm-build-sync-source "number-selector"
        :keymap h-map
        ;; :requires-pattern nil
        :match (list #'(lambda (c) t))
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        ;; :filter-one-by-one #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)

;;; org-capture+-helm.el ends here
