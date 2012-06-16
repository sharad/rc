;;; d-number-sayer.el --- An system for converting numbers into spoken text

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: improved appt.el 
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; The function d-say-number-improved takes in integer argument and
;; speaks it through your computer's speakers.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-number-sayer

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-number-sayer)

;;; Known Bugs:

;; None!

;; (length d-random-play-emacs-midi--file-list)

(defun d-say-number-inner-inner (number)
  (assert (and (>= number 0) (<= number 99)))
  (let (file file1 file2)
    (cond
     ((or (<= number 20) (=  number 30) (=  number 40) (=  number 50)
          (=  number 60) (=  number 70) (=  number 80) (=  number 90)
          )
      (setq file (format "c:/sound-samples/emacs/numbers/%d.wav" number))
      (play-sound (list 'sound :file file :volume 1.0)))
     ((and (> number 20) (< number 30))
      (setq file1 "c:/sound-samples/emacs/numbers/20.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 20)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 30) (< number 40))
      (setq file1 "c:/sound-samples/emacs/numbers/30.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 30)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 40) (< number 50))
      (setq file1 "c:/sound-samples/emacs/numbers/40.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 40)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 50) (< number 60))
      (setq file1 "c:/sound-samples/emacs/numbers/50.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 50)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 60) (< number 70))
      (setq file1 "c:/sound-samples/emacs/numbers/60.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 60)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 70) (< number 80))
      (setq file1 "c:/sound-samples/emacs/numbers/70.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 70)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 80) (< number 90))
      (setq file1 "c:/sound-samples/emacs/numbers/80.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 80)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     ((and (> number 90) (< number 100))
      (setq file1 "c:/sound-samples/emacs/numbers/90.wav")
      (setq file2 (format "c:/sound-samples/emacs/numbers/%d.wav" (- number 90)))
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0)))
     (t
      (play-sound (list 'sound :file "c:/sound-samples/emacs/numbers/many.wav" :volume 1.0))
      )
     )
    )
  )

;; (setq number 19)
;; (d-say 33)
(defun d-say-number-inner (number)
  (assert (and (>= number 0) (<= number 999)))
  (let (file file1 file2 file3)
    (cond
     ((or (= number 100) (= number 200) (= number 300) (= number 400)
          (= number 500) (= number 600) (= number 700) (= number 800)
          (= number 900))
      (progn
        (setq file1 (format "c:/sound-samples/emacs/numbers/%d.wav" (/ number 100)))
        (setq file2 (format "c:/sound-samples/emacs/numbers/hundred.wav"))
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        )
      )
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((and (> number 100) (< number 200))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/1.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 100)))
     ((and (> number 200) (< number 300))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/2.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 200)))
     ((and (> number 300) (< number 400))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/3.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 300)))
     ((and (> number 400) (< number 500))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/4.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
        (d-say-number-inner-inner (- number 400)))
     ((and (> number 500) (< number 600))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/5.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 500)))
     ((and (> number 600) (< number 700))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/6.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 600)))
     ((and (> number 700) (< number 800))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/7.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 700)))
     ((and (> number 800) (< number 900))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/8.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 800)))
     ((and (> number 900) (< number 1000))
      (progn
        (setq file1 "c:/sound-samples/emacs/numbers/9.wav")
        (setq file2 "c:/sound-samples/emacs/numbers/hundred.wav")
        (setq file3 "c:/sound-samples/emacs/numbers/and.wav")
        (play-sound (list 'sound :file file1 :volume 1.0))
        (play-sound (list 'sound :file file2 :volume 1.0))
        (play-sound (list 'sound :file file3 :volume 1.0))
        )
      (d-say-number-inner-inner (- number 900)))
     (t
      (d-say-number-inner-inner number)
      )
     )
    )
  )

;; (d-say-number 1234)
(d-quote defun d-say-number (number)
  (assert (>= number 0)) ;; (<= number 9999)))
  (let (file file1 file2)
    (cond
     ((or (= number 1000) (= number 2000) (= number 3000) (= number 4000)
          (= number 5000) (= number 6000) (= number 7000) (= number 8000)
          (= number 9000))
      (setq file (format "c:/sound-samples/emacs/numbers/%d.wav" number))
      (play-sound (list 'sound :file file :volume 1.0)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
     ((and (> number 1000) (< number 2000))
      (setq file1 "c:/sound-samples/emacs/numbers/1000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 1000)))
     ((and (> number 2000) (< number 3000))
      (setq file1 "c:/sound-samples/emacs/numbers/2000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 2000)))
     ((and (> number 3000) (< number 4000))
      (setq file1 "c:/sound-samples/emacs/numbers/3000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 3000)))
     ((and (> number 4000) (< number 5000))
      (setq file1 "c:/sound-samples/emacs/numbers/4000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 4000)))
     ((and (> number 5000) (< number 6000))
      (setq file1 "c:/sound-samples/emacs/numbers/5000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 5000)))
     ((and (> number 6000) (< number 7000))
      (setq file1 "c:/sound-samples/emacs/numbers/6000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 6000)))
     ((and (> number 7000) (< number 8000))
      (setq file1 "c:/sound-samples/emacs/numbers/7000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 7000)))
     ((and (> number 8000) (< number 9000))
      (setq file1 "c:/sound-samples/emacs/numbers/8000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 8000)))
     ((and (> number 9000) (< number 10000))
      (setq file1 "c:/sound-samples/emacs/numbers/9000.wav")
      (setq file2 "c:/sound-samples/emacs/numbers/and.wav")
      (play-sound (list 'sound :file file1 :volume 1.0))
      (play-sound (list 'sound :file file2 :volume 1.0))
      (d-say-number-inner (- number 9000)))
     ((< number 1000)
      (d-say-number-inner number))
     (t
      (play-sound (list 'sound :file "c:/sound-samples/emacs/numbers/many.wav" :volume 1.0)))
     )
    )
  )

;; (d-split-3 123456)
(defun d-split-3 (number)
  (let ((ans nil))
    (while (> number 0)
      (setq ans (cons (mod number 1000) ans))
      (setq number (/ number 1000)))
    ans))

;; (d-say-number-improved (+ (* 123 1000000) (* 0 1000) 123))
;; (d-say-number-improved (+ (* 123 1000000) (* 456 1000) 789))
;; (d-say-number-improved (+ (* 123 1000000) 789))
;; (d-say-number-improved 54)
;; (progn (d-say-number-improved 123) (d-random-play-emacs-midi "c:/sound-samples/emacs/numbers/minutes-to-go.wav"))
(defun d-say-number-improved (number)
  ;(setq number 123456789)
  (let (file)
    (if (= 0 number)
        (progn
          (setq file "c:/sound-samples/emacs/numbers/0.wav")
          (play-sound (list 'sound :file file :volume 1.0)))
      ;; (d-split-3 -1)
      (let* ((ptr (d-split-3 number))
             (i (length ptr))
             (a)
             (alist '((1 . nil)
                      (2 . "thousand")
                      (3 . "million")
                      (4 . "billion"))))
        (while (> i 0)
          (when (/= 0 (car ptr))
            (d-say-number-inner (car ptr))
            (setq a (cdr (assq i alist)))
            (when a
              (setq a (concat "c:/sound-samples/emacs/numbers/" a ".wav"))
              (if (file-exists-p a)
                  (play-sound (list 'sound :file a :volume 1.0))
                (ding))))
          (decf i)
          (setq ptr (cdr ptr)))))))

(provide 'd-number-sayer)



