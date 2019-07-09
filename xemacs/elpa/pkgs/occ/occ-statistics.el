;;; occ-statistics.el --- statistics                 -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
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

(provide 'occ-statistics)


;; https://www.khanacademy.org/math/statistics-probability/summarizing-quantitative-data/mean-median-basics/a/mean-median-and-mode-review
;; https://www150.statcan.gc.ca/n1/edu/power-pouvoir/ch12/5214891-eng.htm

(defun occ-stats-max (&rest nums)
  (apply #'max nums))

(defun occ-stats-min (&rest nums)
  (apply #'min nums))

(defun occ-stats-range (&rest nums)
  (-
   (apply #'occ-stats-max nums)
   (apply #'occ-stats-min nums)))

(defun occ-stats-aggregate (&rest nums)
  (apply #'+ nums))

(defun occ-stats-mean (&rest nums)
  (if (> (length nums) 0)
      (/
       (apply #'occ-stats-aggregate nums)
       (length nums))
    0))
(defun occ-stats-average (&rest nums)
  (apply #'occ-stats-mean nums))

(defun occ-stats-median (&rest nums)
  (let ((nums   (sort nums #'<))
        (length (length nums)))
    (if (evenp length)
        (occ-stats-mean
         (nth (1- (/ length 2)) nums)
         (nth (/ length 2)      nums))
      (nth (/ (1- length) 2) nums))))

(defun occ-stats-mode (&rest nums)
  ;; https://stackoverflow.com/questions/6050033/elegant-way-to-count-items
  (let ((num-pairs
         (reduce
          #'(lambda (r e)
              (if (and r (= (caar r) e))
                  (cons
                   (cons (caar r) (1+ (cdar r)))
                   (cdr r))
                (cons (cons e  1) r)))
          (sort nums #'>)
          :initial-value nil)))
    (let ((num-pairs
           (sort num-pairs
                 #'(lambda (a b)
                     (> (cdr a) (cdr b))))))
      (mapcar
       #'car
       (remove-if-not
        #'(lambda (pair)
            (= (cdr pair) (cdar num-pairs)))
        num-pairs)))))

(defun occ-stats-variance-internal (average &rest nums)
  (if (> (length nums) 0)
      (sqrt
       (/
        (apply #'occ-stats-aggregate
               (mapcar #'(lambda (rank) (expt (- rank average) 2)) nums))
        (length nums)))
    0))

(defun occ-stats-variance (&rest nums)
  (apply #'occ-stats-variance-internal (apply #'occ-stats-average nums) nums))

(defun occ-stats-stddev-internal (average &rest nums)
  (if (> (length nums) 0)
      (sqrt
       (/
        (apply #'occ-stats-aggregate
               (mapcar #'(lambda (rank) (expt (- rank average) 2)) nums))
        (length nums)))
    0))

(defun occ-stats-stddev (&rest nums)
  (apply #'occ-stats-stddev-internal (apply #'occ-stats-average nums) nums))


(ert-deftest occ-test-stats-mode ()
  (should (equal
           '(3 5)
           (occ-stats-mode 3 5 5 3 3 3 3 4 5 5 5 5 1 1 2 3)))
  (should (equal
           (sort
            '((5 . 3) (4 . 1) (3 . 3) (2 . 1) (1 . 2)) #'(lambda (a b) (> (cdr a) (cdr b))))
           '((5 . 3) (3 . 3) (1 . 2) (4 . 1) (2 . 1))))
  (should (equal
           (reduce (lambda (r e)
                     (if (and r (= (caar r) e))
                         (cons
                          (cons (caar r) (1+ (cdar r)))
                          (cdr r))
                       (cons (cons e  1) r)))
                   (sort (list 1 1 2 3 3 3 4 5 5 5 ) #'>)
                   :initial-value nil)
           '((1 . 2) (2 . 1) (3 . 3) (4 . 1) (5 . 3)))))

;;; occ-statistics.el ends here
