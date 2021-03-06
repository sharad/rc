;;; inline-arithmetic.el --- Functions to perform inline arithmetic.
;;
;; Filename: inline-arithmetic.el
;; Description: Functions to perform inline arithmetic.
;; Author: Brian Kavanagh
;; Keywords: number
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;;
;; Functions to perform inline arithmetic.
;;
;; A set of cute functions which perform arithmetic operations on
;; numbers at 'point' replacing the number with the result of the
;; operation.  Addition and subtraction will default to +1, -1
;; multiplication and division will default to *2, /2.
;;
;; Suggested bindings (CTRL + Numeric Keypad Operations)
;;
;; (global-set-key [C-kp-add]      'inline-arithmetic-add)
;; (global-set-key [C-kp-divide]   'inline-arithmetic-divide)
;; (global-set-key [C-kp-multiply] 'inline-arithmetic-multiply)
;; (global-set-key [C-kp-subtract] 'inline-arithmetic-subtract)
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(defun inline-arithmetic-prompt (num &optional operation arg)
  (if (not operation) (setq operation (read-from-minibuffer "Operation (+, -, *, /): ")))
  (if (not arg)       (setq arg       (string-to-number (read-from-minibuffer "Arg: "))))
  (cond ((string-equal operation "+") (+ num arg))
        ((string-equal operation "-") (- num arg))
        ((string-equal operation "*") (* num arg))
        ((string-equal operation "/") (/ num arg))
        (t                            (error (concat "Invalid operation: " operation)))))

(defun inline-arithmetic (arg operation)
  "Performs OPERATION on the number at point and ARG replacing the number with the result."
  (interactive (list current-prefix-arg
                     (read-from-minibuffer "Operation (+, -, *, /): ")))
  (skip-chars-backward "-0123456789")
  (or (looking-at "[-0123456789]+")
      (error "No number at point"))
  (let ((num    (string-to-number (match-string 0)))
        (result))
    (setq result (cond ((null arg)
                        (cond ((string-equal operation "*") (inline-arithmetic-prompt num operation 2))
                              ((string-equal operation "/") (inline-arithmetic-prompt num operation 2))
                              (t                            (inline-arithmetic-prompt num operation 1))))
                       ((integerp arg) (inline-arithmetic-prompt num operation arg))
                       ((consp arg)    (inline-arithmetic-prompt num operation))
                       (t              result)))
    (replace-match (number-to-string result))))

(defun inline-arithmetic-add (arg)
  "Add ARG to the number at point replacing the number with the result."
  (interactive "P")
  (inline-arithmetic arg "+"))

(defun inline-arithmetic-subtract (arg)
  "Subtract ARG to the number at point replacing the number with the result."
  (interactive "P")
  (inline-arithmetic arg "-"))

(defun inline-arithmetic-multiply (arg)
  "Multiply the number at point by ARG replacing the number with the result."
  (interactive "P")
  (inline-arithmetic arg "*"))

(defun inline-arithmetic-divide (arg)
  "Divide the number at point by ARG replacing the number with the result."
  (interactive "P")
  (inline-arithmetic arg "/"))

(provide 'inline-arithmetic)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; inline-arithmetic.el ends here
