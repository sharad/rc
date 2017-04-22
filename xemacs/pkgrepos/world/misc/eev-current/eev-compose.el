;;; eev-compose.el -- typing accents and mathematical chars using a compose key.

;; Copyright (C) 2001,2002,2003,2004,2005,2008 Free Software Foundation, Inc.
;;
;; This file is part of GNU eev.
;;
;; GNU eev is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; GNU eev is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU eev; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2008jul07
;; Keywords:   i18n, mathematical chars, glyphs
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-compose.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-compose.el.html>

;;; Comment:
;;
;; This file implements a very primitive way to insert accented
;; characters and glyphs - including mathematical glyphs, like the
;; ones defined in "eev-math-glyphs.el". Here's how it works: by
;; default, when `eev-mode' is on, the key `M-,' is bound to
;; `eev-compose-two-keys'; when we type, for example, `M-, ^ o', we
;; get an "ô".
;;
;; Here's how to type the main glyphs that I use in my e-script files:
;;
;;   "":   C-q C-o
;;   "«":   M-, < <
;;   "»":   M-, > >
;;
;; The plist that says how pairs of characters are to be composed by
;; `M-,' is stored in the variable `eev-composes-all'. That variable
;; is not set directly; rather, the function `eev-composes-update'
;; sets it to:
;;   
;;   (append eev-composes-localmath     ; default: nil
;;           eev-composes-globalmath    ; default: nil
;;           eev-composes-accents
;;           eev-composes-otheriso)
;;
;; When the function `eev-math-glyphs-edrx' from "eev-math-glyphs.el"
;; sets compose pairs for mathematical characters it sets just
;; `eev-composes-localmath' and `eev-composes-globalmath', not
;; `eev-composes-accents' or `eev-composes-otheriso'.
;;
;; See: <http://angg.twu.net/eev-current/eev-glyphs.el.html>,
;;      <http://angg.twu.net/eev-current/eev-math-glyphs.el.html>,
;;      <http://angg.twu.net/eev-article.html#glyphs>.
;;      <http://angg.twu.net/eev-article.html#compose-pairs>.
;;      (find-elnode "Reading One Event")
;;      (find-es "emacs" "key_name")

;; Known bugs: this only works reliably in unibyte/raw-text buffers;
;; as the lisp function invoked to insert the new character is
;; `insert' the "composed character" is always inserted, even in
;; `overwrite-mode' and `picture-mode'; and this does not always work
;; for inserting special characters in the minibuffer.


(defun eev-compose-pair (pair) (interactive "sTwo-character code: ")
"Convert PAIR (a two-character string) to a single character and insert it.
The conversion is done by looking up PAIR in the the plist `eev-composes-all'.
If an entry for PAIR is not found, raise an error."
  (let ((sublist (member pair eev-composes-all)))
    (if sublist (insert (nth 1 sublist))
      (error "Pair \"%s\" not in `eev-composes-all'" pair))))

(defun eev-compose-two-keys ()
"Read two characters with `read-event' and insert their \"composition\".
For example: `\\[eev-compose-two-keys] ^ A' inserts an \"A\" with a hat.
The list of composable pairs is stored in the variable `eev-composes-all'.
See also `eev-composes-update'."
  (interactive)
  (eev-compose-pair (format "%c%c"
			   (read-event "Compose key 1: " t)
			   (read-event "Compose key 2: " t))))

(defvar eev-composes-accents '(
   "`A" ?À   "`E" ?È   "`I" ?Ì   "`O" ?Ò   "`U" ?Ù
   "`a" ?à   "`e" ?è   "`i" ?ì   "`o" ?ò   "`u" ?ù
   "'A" ?Á   "'E" ?É   "'I" ?Í   "'O" ?Ó   "'U" ?Ú
   "'a" ?á   "'e" ?é   "'i" ?í   "'o" ?ó   "'u" ?ú
   "^A" ?Â   "^E" ?Ê   "^I" ?Î   "^O" ?Ô   "^U" ?Û
   "^a" ?â   "^e" ?ê   "^i" ?î   "^o" ?ô   "^u" ?û
   "~A" ?Ã                       "~O" ?Õ
   "~a" ?ã                       "~o" ?õ
  "\"A" ?Ä  "\"E" ?Ë  "\"I" ?Ï  "\"O" ?Ö  "\"U" ?Ü
  "\"a" ?ä  "\"e" ?ë  "\"i" ?ï  "\"o" ?ö  "\"u" ?ü
   "'C" ?Ç   "CC" ?Ç   "~N" ?Ñ
   "'c" ?ç   "cc" ?ç   "~n" ?ñ
))
(defvar eev-composes-otheriso '(
   "_a" ?ª   "_o" ?º   "AE" ?Æ   "ae" ?æ   "ss" ?ß
   "!!" ?¡   "??" ?¿   "SS" ?§   "<<" ?«   ">>" ?»
   "00" ?°   "11" ?¹   "22" ?²   "33" ?³
   "14" ?¼   "12" ?½   "34" ?¾
   "+-" ?±   ":-" ?÷   "cd" ?·   "xx" ?×   "nt" ?¬
))
(defvar eev-composes-globalmath nil)
(defvar eev-composes-localmath nil)
(defvar eev-composes-all nil)

(defun eev-composes-update ()
  "Update the variable `eev-composes-all'. See the source code."
  (setq eev-composes-all
	(append eev-composes-localmath eev-composes-globalmath
		eev-composes-accents   eev-composes-otheriso)))

(eev-composes-update)

(provide 'eev-compose)


;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "«%s»"
;; ee-comment-prefix: ";;"
;; End:
