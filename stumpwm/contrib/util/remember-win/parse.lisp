;;; -*- Mode: Lisp -*-
;;;
;;; $Header: /home/gene/library/website/docsrc/pcm/src/RCS/parse.lisp,v 395.1 2008/04/20 17:25:47 gene Exp $
;;;
;;; Copyright (c) 2006 Gene Michael Stover.  All rights reserved.
;;;
;;; This program is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 2 of the
;;; License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public
;;; License along with this program; if not, write to the Free Software
;;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301
;;; USA
;;;


(in-package #:remember-win)
(export '(parse))

(defvar *double-quote* #\")
(defvar *single-quote* #\')
(defvar *escape* #\\)
(defvar *spaces* '(#\Space #\Tab #\Newline))

(defun is-space-lex0 (x) (member x *spaces*))

(defun discard-space-lex0 (lst)
  (do ()
      ((not (is-space-lex0 (first lst))) lst)
      (pop lst)))

(defun lex0 (seq)
  "Perform level-0 lexical analysis on the sequence of characters.
Return list of tokens.  Most characters become themselves as tokens.
Quotes become :DOUBLE-QUOTE unless escaped.  Contiguous spaces become
:SPACE.  Escape character is itself unless it was used to escape a
quote."
  (let ((x nil)
	(is-double-quoted nil)
        (is-single-quoted nil))
    (do ((lst (coerce seq 'list) (rest lst)))
	((endp lst) (nreverse x))
	(cond ((and (equal *double-quote* (first lst))
                    (not is-single-quoted))
	       ;; An un-escaped quote character becomes a :DOUBLE-QUOTE token.
	       (setq is-double-quoted (not is-double-quoted))
	       (push :double-quote x))

              ((and (equal *single-quote* (first lst))
                    (not is-double-quoted))
	       ;; An un-escaped quote character becomes a :SINGLE-QUOTE token.
	       (setq is-single-quoted (not is-single-quoted))
	       (push :single-quote x))

	      ((and (equal *escape* (first lst))
		    (equal *double-quote* (second lst)))
	       ;; An escaped quote becomes a quote character.
	       (if (not is-single-quoted)
	       ;; An escaped quote becomes a quote character.
                   (progn
                     (push *double-quote* x);
                     (pop lst))
                   (progn
                     ;; (push (first lst) x)
                     (push (second lst) x)
                     (pop lst)))) ; discard the escape
              ((and (equal *escape* (first lst))
		    (equal *single-quote* (second lst)))
               (if (not is-single-quoted)
	       ;; An escaped quote becomes a quote character.
                   (progn
                     (push *single-quote* x);
                     (pop lst))
                   (progn
                     (push (first lst) x)
                     (push (second lst) x)
                     (pop lst)))) ; discard the escape
	      ((and (not is-double-quoted)
                    (not is-single-quoted)
                    (is-space-lex0 (first lst)))
	       ;; The sequence begins with a white-space character.
	       ;; So we push :SPACE onto the tokens, then discard all
	       ;; of the spaces.
	       (push :space x)
	       (setq lst (cons :will-be-discarded (discard-space-lex0 lst))))
	      (t
	       ;; Most characters are literals.
	       (push (first lst) x))))))

(defun next-single-quoted-token (lst0)
  (assert (eq :single-quote (first lst0)))
  (pop lst0)                            ; discard :DOUBLE-QUOTE
  (do ((token nil (cons (first lst) token))
       (lst lst0 (rest lst)))
      ((eq :single-quote (first lst)) (coerce (nreverse token) 'string))))

(defun after-single-quoted-token (lst)
  (assert (eq :single-quote (first lst)))
  (pop lst)                             ; discard beginning :DOUBLE-QUOTE
  (loop until (member (first lst) '(:single-quote nil))
	do (pop lst))
  (pop lst)                             ; discard ending :DOUBLE-QUOTE
  lst)

(defun next-double-quoted-token (lst0)
  (assert (eq :double-quote (first lst0)))
  (pop lst0)                            ; discard :DOUBLE-QUOTE
  (do ((token nil (cons (first lst) token))
       (lst lst0 (rest lst)))
      ((eq :double-quote (first lst)) (coerce (nreverse token) 'string))))

(defun after-double-quoted-token (lst)
  (assert (eq :double-quote (first lst)))
  (pop lst)                             ; discard beginning :DOUBLE-QUOTE
  (loop until (member (first lst) '(:double-quote nil))
	do (pop lst))
  (pop lst)                             ; discard ending :DOUBLE-QUOTE
  lst)

(defun next-raw-token (lst0)
  (do ((token nil (cons (first lst) token))
       (lst lst0 (rest lst)))
      ((member (first lst) '(:space nil))
       (coerce (nreverse token) 'string))))

(defun after-raw-token (lst)
  (loop until (member (first lst) '(:space nil))
	do (pop lst))
  (pop lst)                             ; discard the :SPACE
  lst)

(defun is-space-lex1 (x) (member x '(:space)))

(defun is-first-space-lex1 (list)
  (if list
      (member (first list) '(:space nil))))

;; (defun discard-space-lex1 (lst)
;;   (do ()
;;       ((not (and (is-space-lex1 (first lst)))) lst)
;;     (pop lst)))

(defun discard-space-lex1 (lst)
  (loop while (is-first-space-lex1 lst)
     do (pop lst))
  lst)

(defun next-token (lst)
  "Return the next token as a string."
  (setq lst (discard-space-lex1 lst))
  (cond ((endp lst) nil)
	((eq :double-quote (first lst)) (next-double-quoted-token lst))
        ((eq :single-quote (first lst)) (next-single-quoted-token lst))
	(t (next-raw-token lst))))

(defun after-token (lst)
  (setq lst (discard-space-lex1 lst))
  (cond ((endp lst) nil)
	((eq :double-quote (first lst)) (after-double-quoted-token lst))
        ((eq :single-quote (first lst)) (after-single-quoted-token lst))
	(t (after-raw-token lst))))

(defun lex1 (seq0)
  "Given a sequence as returned by LEX0, produce a list of
tokens.  These tokens are strings."
  ;; (format t "~%IN ~s~%" seq0)
  (do ((lst nil (cons (next-token seq) lst))
       (seq (coerce seq0 'list) (after-token seq)))
      ((or (null (next-token seq)) (endp seq)) (nreverse lst))))

(defun parse (string)
  (lex1 (lex0 (coerce string 'list))))


(defun print-parse (str)
  (format t "In: ~s, Out: ~s" str (parse str)))

;;;
;;;
;;;

;; (defun test0000 () 'test0000)

;; (defun test0002 ()
;;   "Test IS-SPACE on some hard-coded inputs."
;;   (and (is-space #\Space)
;;        (is-space #\Tab)
;;        (is-space #\Newline)
;;        (not (is-space #\a))
;;        (not (is-space nil))
;;        (not (is-space 32))))

;; (defun test0005 ()
;;   "Test LEX0 on a simple hard-coded input."
;;   (equal (lex0 "abc") '(#\a #\b #\c)))

;; (defun test0007 ()
;;   "Test LEX0 on a hard-coded input that contains a single space
;; between two non-space characters."
;;   (equal (lex0 "a c") '(#\a :space #\c)))

;; (defun test0008 ()
;;   "Test LEX0 on a hard-coded input that contains contiguous
;; spaces between two non-space characters."
;;   (equal (lex0 "a      c") '(#\a :space #\c)))

;; (defun test0009 ()
;;   "Test LEX0 on a hard-coded input that contains leading &
;; trailing spaces."
;;   (equal (lex0 "  a c ") '(:space #\a :space #\c :space)))

;; (defun test0013 ()
;;   "Test LEX0 on a hard-coded input that contains a quoted token.
;; That token contains an embedded space."
;;   (equal (lex0 "\"a c\"") '(:double-quote #\a #\Space #\c :double-quote)))

;; (defun test0015 ()
;;   "Test LEX0 on a hard-coded input that contains some spaced
;; which are quoted, some which are not, & some escaped quotes."
;;   (equal (lex0 " \"\\\" \"")
;; 	 '(:space :double-quote #\" #\Space :double-quote)))

;; (defun test0051 ()
;;   "Test LEX1 on a simple input.  This input has one token, no
;; quotes, no spaces, not escapes."
;;   (equal (lex1 '(#\w #\o #\r #\d)) '("word")))

;; (defun test0053 ()
;;   "Test LEX1 on a two-token input.  The tokens are plain, have no
;; quotes, escapes, or embedded spaces."
;;   (equal (lex1 '(#\b #\l #\u #\e :SPACE #\s #\h #\o #\e))
;; 	 '("blue" "shoe")))

;; (defun test0055 ()
;;   "Test LEX1 on a one-token input.  The token contains a space.
;; The token is quoted so it should include the space."
;;   (equal (lex1 (lex0 "\"a c\"")) '("a c")))

;; (defun test0056 ()
;;   "Test LEX1 on a one-token input which contains an escaped
;; quote."
;;   (equal (lex1 (lex0 "\\\"a")) '("\"a")))

;;; --- end of file ---
