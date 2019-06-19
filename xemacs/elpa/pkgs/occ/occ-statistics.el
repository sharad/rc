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

(defun occ-stats-max (num &rest nums)
  (apply #'max num nums))

(defun occ-stats-min (num &rest nums)
  (apply #'min num nums))

(defun occ-stats-range (num &rest nums)
  (let ((nums (cons num nums)))
    (-
     (apply #'occ-stats-max nums)
     (apply #'occ-stats-min nums))))

(defun occ-stats-aggregate (num &rest nums)
  (let ((nums (cons num nums)))
    (reduce #'+ nums)))

(defun occ-stats-mean (num &rest nums)
  (let ((nums (cons num nums)))
    (if (> (length nums) 0)
        (/
         (apply #'occ-stats-aggregate nums)
         (length nums))
      0)))

(defun occ-stats-median (num &rest nums)
  (let ((nums (cons num nums)))
    (let ((nums   (sort nums #'<))
          (length (length nums)))
      (if (evenp length)
          (occ-stats-mean
           (nth (1- (/ length 2)) nums)
           (nth (/ length 2)      nums))
        (nth (/ (1- length) 2) nums)))))

(defun occ-stats-mode (num &rest nums)
  ;; https://stackoverflow.com/questions/6050033/elegant-way-to-count-items
  (let ((nums (cons num nums)))
    (let ((nums-pair
           (reduce
            #'(lambda (r e)
                (if (and r (= (caar r) e))
                    (cons
                     (cons (caar r) (1+ (cdar r)))
                     (cdr r))
                  (cons (cons e  1) r)))
            (sort nums #'<)
            :initial-value nil)))
      (let ((nums-pair
             (sort nums-pair
                   #'(lambda (a b)
                       (> (cdr a) (cdr b))))))
        (mapcar
         #'car
         (remove-if-not
          #'(lambda (pair) (= (cdr pair)(cdar nums-pair)))
          nums-pair))))))

(when nil
  (reduce (lambda (r e)
            (if (and r (= (caar r) e))
                (cons
                 (cons (caar r) (1+ (cdar r)))
                 (cdr r))
              (cons (cons e  1) r)))
          (sort (list 1 1 2 3 3 3 4 5 5 5 ) #'<)
          :initial-value nil)

  (occ-stats-mode 1 1 2 3 3 3 4 5 5 5 5 5 5)

  (sort
   '((5 . 3) (4 . 1) (3 . 3) (2 . 1) (1 . 2)) #'(lambda (a b) (> (cdr a) (cdr b)))))

(defun occ-stats-variance (num &rest nums)
  (let ((nums (cons num nums)))
   (let ((average (occ-stats-average nums)))
     (if (> (length nums) 0)
         (sqrt
          (/
           (apply #'occ-stats-aggregate
                  (mapcar #'(lambda (rank) (expt (- rank average) 2)) nums))
           (length nums)))
       0))))

(defun occ-stats-stddev (num &rest nums)
  (let ((nums (cons num nums)))
   (let ((average (occ-stats-average nums)))
     (if (> (length nums) 0)
         (sqrt
          (/
           (apply #'occ-stats-aggregate
                  (mapcar #'(lambda (rank) (expt (- rank average) 2)) nums))
           (length nums)))
       0))))

;;; occ-statistics.el ends here
