;;; kitchen-dyntrans.el --- Kitchen Transform        -*- lexical-binding: t; -*-

;; Copyright (C) 2019  sharad

;; Author:
;; Keywords:

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




;; https://kitchingroup.cheme.cmu.edu/blog/2016/01/24/Modern-use-of-helm-sortable-candidates/

(setq h-data '((:num 1 :key "apple")
               (:num 9 :key "berry")
               (:num 2 :key "cactus")
               (:num 5 :key "dog")
               (:num 4 :key "frog")))

(defun h-candidates ()
  "Returns candidates for the helm source."
  (loop for cand in h-data
        collect (cons (format "%s %s"
                              (plist-get cand :num)
                              (plist-get cand :key))
                      cand)))

(print (h-candidates))

(defvar h-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map helm-map)
    (define-key map (kbd "M-<down>")   'h-sort)
    map)
  "keymap for a helm source.")

(defvar h-sort-fn nil)

(defun h-sort ()
  (interactive)
  (let ((action (read-char "#decreasing (d) | #increasing (i) | a-z (a) | z-a (z: ")))
    (cond
     ((eq action ?d)
      (setq h-sort-fn (lambda (c1 c2) (> (plist-get (cdr c1) :num) (plist-get (cdr c2) :num)))))
     ((eq action ?i)
      (setq h-sort-fn (lambda (c1 c2) (< (plist-get (cdr c1) :num) (plist-get (cdr c2) :num)))))
     ((eq action ?a)
      (setq h-sort-fn (lambda (c1 c2) (string< (plist-get (cdr c1) :key) (plist-get (cdr c2) :key)))))
     ((eq action ?z)
      (setq h-sort-fn (lambda (c1 c2) (string> (plist-get (cdr c1) :key) (plist-get (cdr c2) :key)))))
     (t (setq h-sort-fn nil)))
    (helm-refresh)
    (setq h-sort-fn nil)))

(defun h-candidate-transformer (candidates source)
  (if h-sort-fn
      (progn (message "Sorting with %s" h-sort-fn)
             (-sort h-sort-fn candidates))
    candidates))

(defun h-action-transformer (actions candidate)
  "Candidate is the result selected."
  (if (evenp (plist-get candidate :num))
      '(("Even" . identity))
    '(("Odd" . identity))))

(setq h-source
      (helm-build-sync-source "number-selector"
        :keymap h-map
        :candidates #'h-candidates
        :filtered-candidate-transformer #'h-candidate-transformer
        :action-transformer #'h-action-transformer))

(helm :sources 'h-source)

;;; kitchen-dyntrans.el ends here
