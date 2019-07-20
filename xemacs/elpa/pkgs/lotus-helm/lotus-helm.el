;;; lotus-helm.el --- Lotus Helm                     -*- lexical-binding: t; -*-

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

(provide 'lotus-helm)


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

;;; lotus-helm.el ends here
