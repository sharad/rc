;;; reader-config.el --- Reader Config

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

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



(deh-require-maybe centered-cursor-mode
;; To activate do:
;;     M-x centered-cursor-mode
;; for buffer local or
;;     M-x global-centered-cursor-mode
;; for global minor mode.
  )


;;{{ from: http://www.gnu.org/software/emacs/manual/html_node/cl/
(require 'cl)
(defun* call-at-steps (&key (count 100) (micros 100) (fn 'next-line))
  (loop repeat count do
       (progn
         (funcall fn)
         (sit-for 0 micros)
              )))

(defun smooth-next-line ()
  (interactive)
  (call-at-steps :fn 'next-line))

(defun smooth-forward-char ()
  (interactive)
  (call-at-steps :count 10000 :fn 'forward-char))

(defun smooth-step (num key micros)
  (interactive "p num: \nkkey: \nnmicrosecs: ")
  ;; (let ((cmd (key-binding (read-key-sequence "safds: ") t)))
  (let ((cmd (key-binding key t))
        (num (if (> num 1) num 100))
        (micros (if (> micros 1) micros 100))
        )

    ;; (message num)
    (call-at-steps :count num :micros micros :fn cmd)))



(defun smooth-read ()
  (interactive)
  (call-at-steps :micros 800 :fn '(lambda ()
                       (forward-sentence)
                       (speechd-speak-read-sentence)
                                   )))



;;}}



(provide 'reader-config)
;;; reader-config.el ends here
