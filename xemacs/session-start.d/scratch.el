;;
;; scratch.el
;; Login : <spratap@spratap>
;; Started on  Fri Jun 11 17:18:56 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

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


;; (defn (key-binding key t))

;; (setq x (read-key-sequence "safds: "))
;; (funcall (key-binding x t))



(defun dividebymb ()
  "divide by 1 mb"
  (interactive)
  (let* ((number (number-at-point))
         (res (/ number 1048576)))
    (when res
        (forward-word)
        (insert ?\  (number-to-string res) "MB"))))


(user-provide 'scratch)
