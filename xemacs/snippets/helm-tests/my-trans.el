;;; my-trans.el --- my trans                         -*- lexical-binding: t; -*-

;; Copyright (C) 2019  s

;; Author: s <sh4r4d@gmail.com>
;; Keywords: abbrev

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



;; Check for C-h f helm-source-sync

(defun h-candidates ()
  '("aaaa" "bbb"))


(defun h-candidate-transformer (candidates source)
  (message "called")
  ;; (mapcar #'(lambda (x) (concat "xx" helm-pattern x)) candidates)
  (mapcar #'(lambda (x) (concat "xx" helm-pattern x)) candidates))

(defun h-action-transformer (actions candidate)
  '(("Even" . identity)))

(setq h-source
      (helm-build-sync-source "number-selector"
        ;; :keymap h-map
        ;; :requires-pattern nil
        ;; :match (list #'(lambda (c) t))
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)


(defun h-candidates ()
  '("aaaa" "bbb" "ccc"))


(defun h-candidate-transformer (candidate)
  (concat "xx" helm-pattern candidate))

(defun h-action-transformer (actions candidate)
  '(("Even" . identity)))

(setq h-source
      (helm-build-sync-source "number-selector"
        ;; :keymap h-map
        ;; :requires-pattern nil
        :match (list #'(lambda (c) t))
        :candidates #'h-candidates
        :filter-one-by-one #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)



(defun h-candidates ()
  '("aaaa" "bbb" "ccc"))

(defun h-candidate-transformer (candidates source)
  (reverse (h-candidates)))

(defun h-action-transformer (actions candidate)
  '(("Even" . identity)))

(setq h-source
      (helm-build-sync-source "number-selector"
        ;; :keymap h-map
        ;; :requires-pattern nil
        ;; :match (list #'(lambda (c) t))
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        ;; :filter-one-by-one #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)

;;; my-trans.el ends here
