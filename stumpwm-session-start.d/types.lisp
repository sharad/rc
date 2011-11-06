;; -*-lisp-*-
;;
;; Copyright 2009 Sharad Pratap
;;
;; Maintainer: Sharad Pratap
;;
;; This file is part of stumpwm.
;;
;; stumpwm is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; stumpwm is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this software; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place, Suite 330,
;; Boston, MA 02111-1307 USA

(in-package :stumpwm)

 ;; -- Macro: define-stumpwm-type type (input prompt) &body body
 ;;     Create a new type that can be used for command arguments. TYPE can
 ;;     be any symbol.

 ;;     When BODY is evaluated INPUT is bound to the argument-line. It is
 ;;     passed to `argument-pop', `argument-pop-rest', etc. PROMPT is the
 ;;     prompt that should be used when prompting the user for the
 ;;     argument.

;; test
;; (completing-read (current-screen)
;;                  "sdgsdfg: "
;;                  '("asf" "afrasdf" "sdfsdf"))


(define-stumpwm-type :symbol (input prompt)
  (or (find-symbol (string-upcase
                    (or (argument-pop input)
                        ;; Whitespace messes up find-symbol.
                        (string-trim " "
                                     (completing-read (current-screen)
                                                      prompt
                                                      ;; find all symbols in the
                                                      ;;  stumpwm package.
                                                      (let (acc)
                                                        (do-symbols (s (find-package "STUMPWM"))
                                                          (push (string-downcase (symbol-name s)) acc))
                                                        acc)))
                        (throw 'error "Abort.")))
                   "STUMPWM")
      (throw 'error "Symbol not in STUMPWM package")))

;; (define-stumpwm-type :cmdline (input prompt)
;;   (or (find-symbol (string-upcase
;;                     (or (argument-pop input)
;;                         ;; Whitespace messes up find-symbol.
;;                         (string-trim " "
;;                                      (completing-read (current-screen)
;;                                                       ;prompt
;;                                                       "cli: "
;;                                                       ;; find all symbols in the
;;                                                       ;;  stumpwm package.
;;                                                       ;;   (remove-duplicates
;;                                                       (remove-if #'null
;;                                                                  (mapcar #'(lambda (win)
;;                                                                              (xproperty 'cli win))
;;                                                                          (screen-windows (current-screen))))))
;;                         (throw 'error "Abort.")))
;;                    "STUMPWM")
;;       (throw 'error "Symbol not in STUMPWM package")))

;; (defcommand "symbol" (sym) ((:symbol "Pick a symbol: "))
;;   (message "~a" (with-output-to-string (s)
;;                   (describe sym s))))

;;      ;; This code creates a new type called `:symbol' which finds the
;;      ;; symbol in the stumpwm package. The command `symbol' uses it and
;;      ;; then describes the symbol.

;; ;; (string-trim "\s" "    sdf sdf sdf   sdf         ds        ")
;; ;; (string-trim )




;; testing uncomment all

;; (in-package :stumpwm)
;; (screen-groups (current-screen))

;; (select-group (current-screen) "Default")

;; (string-equal
;;  (princ-to-string
;;   (group-map-number
;;    (car (screen-groups (current-screen))))) "2")

;; (group-map-number
;;  (find-group (current-screen) ".scratchpad"))

;; (setf gp  (group-number (find-group (current-screen) ".hold")))

;; (abs (group-number (find-group (current-screen) ".hold")))


(defun group-map-number (group)
  (let* ((num (group-number group))
         (index (abs num)))
    (if (and (>= index 0)
             (< index (length *group-number-map*)))
        (format nil "~:[~;-~]~a" (minusp num) (elt *group-number-map* index))
        num)))

;; (1- gp)

;; (1- (abs gp))

;; (minusp gp)

;; (elt *group-number-map* -1)

;; (abs 10)


;; (group-number (find-group (current-screen) ".hold"))
;; -1
;; (group-number (find-group (current-screen) ".scratchpad"))
;; 0
;; (mapcar #'(lambda (g)
;;             (format nil "~a ~a~%"
;;                     (group-number g)
;;                     (group-name g)
;;                     )) (screen-groups (current-screen)))

;; *group-number-map*

