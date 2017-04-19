;;; eev-math-glyphs.el -- mathematical glyphs for editing TeX texts in X
;; Copyright (C) 2005,2006,2007,2008 Free Software Foundation, Inc.
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
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.
;;
;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2008jul03
;; Keywords:   e-scripts, display, TeX, glyphs.

;;; Commentary:

;; This is immature code, and it is being included in the eev package
;; as an example ONLY! This is only for the addicts to the
;; unibyte/raw-text "encodings", like me. I have tons of TeX'able
;; notes using glyphs like those, and _I KNOW_ that unibyte and glyphs
;; is not the modern way to do things, that this glyphs trick is
;; non-standard as hell, etc etc, you don't need to tell me, _thanks_!
;;
;; By the way: I used to use glyphs like those in (VGA) text mode,
;; using hacked console fonts - I only adapted this thing to X in the
;; beginning of May, 2005. Which means: I haven't even played with it
;; for long enough!
;;
;; See: (find-eev "eev-glyphs.el")
;;      (find-eev "eev-compose.el")
;;      (find-angg "vtutil4/mathchars.lua")
;; To reload: (load-file "eev-math-glyphs.el")

(require 'eev-glyphs)
(require 'eev-compose)
(if (not window-system)
    (error "eev-math-glyphs.el is for X only"))



;;;      _ _             _        _     _           
;;;   __| (_)___ _ __   | |_ __ _| |__ | | ___  ___ 
;;;  / _` | / __| '_ \  | __/ _` | '_ \| |/ _ \/ __|
;;; | (_| | \__ \ |_) | | || (_| | |_) | |  __/\__ \
;;;  \__,_|_|___/ .__/   \__\__,_|_.__/|_|\___||___/
;;;             |_|                                 
;;
;; We want to make adding new glyphs a reversible operation, so...

(defvar eev-math-glyphs-standard-display-table-backup nil
  "A backup of standard-display-table (before adding user-defined glyphs)")

;; Save a backup of `standard-disply-table' now.
(if (not eev-math-glyphs-standard-display-table-backup)
    (setq eev-math-glyphs-standard-display-table-backup
	  (copy-sequence standard-display-table)))

(defun eev-math-glyphs-reset ()
  "Restore the standard-display-table and empties eev-composes-localmath."
  (interactive)
  (setq standard-display-table
	(copy-sequence eev-math-glyphs-standard-display-table-backup))
  (setq eev-composes-localmath nil)
  (eev-composes-update))


;;;   __                     
;;;  / _| __ _  ___ ___  ___ 
;;; | |_ / _` |/ __/ _ \/ __|
;;; |  _| (_| | (_|  __/\__ \
;;; |_|  \__,_|\___\___||___/
;;;                          

;; Define several faces for glyphs in a few lines of code.
;; This is too rigid, but whatever.
;; (find-ecolors)
;;
(defun eev-glyphs-set-face (face fg bg)
  (make-face face)
  (set-face-foreground face fg)
  (set-face-background face bg))

(eev-glyphs-set-face 'eev-glyph-face-Greek   "orange"        "gray20")
(eev-glyphs-set-face 'eev-glyph-face-greek   "coral"         "gray20")
(eev-glyphs-set-face 'eev-glyph-face-logical "SteelBlue1"    "gray20")
(eev-glyphs-set-face 'eev-glyph-face-math    "RoyalBlue2"    "gray20")
(eev-glyphs-set-face 'eev-glyph-face-linear  "PaleVioletRed" "gray20")
(eev-glyphs-set-face 'eev-glyph-face-graphic "red"           "gray20")
(eev-glyphs-set-face 'eev-glyph-face-font    "gold"          "DarkOrange4")

;; Now defining glyphs can be as simple as this:
;; (eev-set-glyph ?Þ 332664 'eev-glyph-face-Greek)
;; but having many lines like that is not very convenient -
;; it is too verbose and it does not handle compose pairs...
;; See: (find-eev "eev-glyphs.el" "eev-set-glyph")


;;;                                 __       _                
;;;  _ __   __ _ _ __ ___   ___     \ \  ___| |__   __ _ _ __ 
;;; | '_ \ / _` | '_ ` _ \ / _ \_____\ \/ __| '_ \ / _` | '__|
;;; | | | | (_| | | | | | |  __/_____/ / (__| | | | (_| | |   
;;; |_| |_|\__,_|_| |_| |_|\___|    /_/ \___|_| |_|\__,_|_|   
;;;                                                           

;; Old Emacs: (find-echarsetchars 'mule-unicode-0100-24ff)
;;            (find-echarsetchars 'mule-unicode-2500-33ff)
;; New Emacs: (find-einsert '((128 14000)))
;; (find-eleimfile "quail/sgml-input.el")
;; (find-eleimfile "quail/latin-ltx.el")

;; A high-level way to set glyphs and compose pairs.
(defvar eev-math-glyphs-name-to-char
  '(("Theta"   . 332664)
    ("Pi"      . 332704)
    ("Sigma"   . 332707)
    ("Omega"   . 332713)
    ;;
    ("delta"   . 332724)
    ("epsilon" . 332725)
    ("theta"   . 332728)
    ("lambda"  . 343339)
    ("nu"      . 332733)
    ("pi"      . 332736)
    ("rho"     . 332737)
    ("sigma"   . 332739)
    ("tau"     . 332740)
    ("omega"   . 343541)
    ;;
    ("top"     . 343268)
    ("bot"     . 343269)
    ("land"    . 343111)
    ("lor"     . 343112)
    ("supset"  . 343235)
    ("forall"  . 343072)
    ("exists"  . 343075)
    ("box"     . 299376)
    ("thin:"   . 343126)
    ("in"      . 343082)
    ("circ"    . 343096)
    ("cap"     . 343113)
    ("cup"     . 343114)
    ("Int"     . 343115)
    ("nabla"   . 343079)
    ("infty"   . 343102)
    ("ge"      . 343205)
    ("^1"      .    185)
    ("sqcap"   . 343251)
    ("sqcup"   . 343252)
    ("ud&"     . 342827)
    ("oplus"   . 343253)
    ("otimes"  . 343259)
    ("to"      . 342898)
    ("dotli"   . 343375)
    ("nat"     . 299502)
    ("seblock" . 299223)
    ("neblock" . 299229)
    ("b"       .     ?b)
    ("r"       .     ?r)
    ("t"       .     ?t)
    ("s"       .     ?s))
  "An alist that translates char names\
 (for mathematical glyphs) to char codes.")

;; 2008feb22: This is an emergency hack to make my math glyphs
;; work in CVS Emacs after the merge of the unicode-2 branch...
;; This - or the previous table - will disappear soon, I hope...
;;
(when (fboundp 'make-glyph-code)
(setq eev-math-glyphs-name-to-char
  '(("Theta"   .  920)
    ("Pi"      .  928)
    ("Sigma"   .  931)
    ("Omega"   .  937)
    ;;
    ("delta"   .  948)
    ("epsilon" .  949)
    ("theta"   .  952)
    ("lambda"  .  955)
    ("nu"      .  957)
    ("pi"      .  960)
    ("rho"     .  961)
    ("sigma"   .  963)
    ("tau"     .  964)
    ("omega"   .  969)
    ;;
    ("top"     . 8868)
    ("bot"     . 8869)
    ("land"    . 8743)
    ("lor"     . 8744)
    ("supset"  . 8835)
    ("forall"  . 8704)
    ("exists"  . 8707)
    ;; ("box"     . 9633)
    ("box"     . 9744)
    ("thin:"   . 8758)
    ("in"      . 8712)
    ("circ"    . 9675)
    ("cap"     . 8745)
    ("cup"     . 8746)
    ("Int"     . 8747)
    ("nabla"   . 8711)
    ("infty"   . 8734)
    ("ge"      . 8805)
    ("^1"      .  185)
    ("sqcap"   . 8851)
    ("sqcup"   . 8852)
    ("ud&"     . 8523)
    ("oplus"   . 8853)
    ("otimes"  . 8855)
    ("to"      . 8594)
    ("lolli"   . 8888)
    ("dotli"   . 9480)
    ("nat"     . 9838)
    ("seblock" . 9623)
    ("neblock" . 9629)
    ("b"       .   ?b)
    ("r"       .   ?r)
    ("t"       .   ?t)
    ("s"       .   ?s)
    ;;
    ("ulcorn"  . 8988)
    ("urcorn"  . 8989))
  )
)

;; To inspect eev-math-glyphs-name-to-char:
' (progn (find-estring "")
         (mapc (lambda (p) (insert (format "%c %s\n" (cdr p) (car p))))
	       eev-math-glyphs-name-to-char))


;;;                                             _       
;;;   __ _ _ __ __ _ _   _ _ __ ___   ___ _ __ | |_ ___ 
;;;  / _` | '__/ _` | | | | '_ ` _ \ / _ \ '_ \| __/ __|
;;; | (_| | | | (_| | |_| | | | | | |  __/ | | | |_\__ \
;;;  \__,_|_|  \__, |\__,_|_| |_| |_|\___|_| |_|\__|___/
;;;            |___/                                    
;;
;; Functions to split packed argument lists

(defun ee-map-split (seq)
"Split with `ee-split' the strings in SEQ.
The original SEQ is not modified; this function returns a new list.
Example: (ee-map-split '(foo \"ab cd\" \"00 11\"))
            ==> (foo (\"ab\" \"cd\") (\"00\" \"11\"))"
  (mapcar (lambda (x) (if (stringp x) (ee-split x) x)) seq))

(defun ee-map-nth (n seq)
"Substitute each list in SEQ by its N-th element.
The original SEQ is not modified; this function returns a new list.
  (ee-map-nth 0 '(foo (a b c) (1 2) bar))
     ==> (foo a 1 bar)
  (ee-map-nth 1 '(foo (a b c) (1 2) bar))
     ==> (foo b 2 bar)
  (ee-map-nth 2 '(foo (a b c) (1 2) bar))
     ==> (foo c nil bar)"
  (mapcar (lambda (x) (if (consp x) (nth n x) x)) seq))

(defun ee-chop1 (n bigseq)
"Return BIGSEQ split in N-element chunks.
Example: (ee-chop1 4 (number-sequence 1 10))
            ==> ((1 2 3 4) (5 6 7 8) (9 10))"
  (setq bigseq (copy-sequence bigseq))
  (let ((rest nil) (result nil))
    (while bigseq
      (setq rest (nthcdr n bigseq))
      (if (nthcdr (- n 1) bigseq)
	  (setcdr (nthcdr (- n 1) bigseq) nil))
      (setq result (cons bigseq result))
      (setq bigseq rest))
    (reverse result)))

(defun ee-chop2 (ref seq)
"Use `ee-map-nth' to return (length (nth REF SEQ)) \"sections\" of SEQ.
Examples:
  (ee-chop2 1 '(foo (a b c) (1 2) bar))
     ==> ((foo a 1 bar) (foo b 2 bar) (foo c nil bar))
  (ee-chop2 2 '(foo (a b c) (1 2) bar))
     ==> ((foo a 1 bar) (foo b 2 bar))"
  (let ((ns (number-sequence 0 (- (length (nth ref seq)) 1))))
    (mapcar (lambda (n) (ee-map-nth n seq)) ns)))

(defun ee-chop3 (n ref bigseq)
"Combine `ee-map-split', `ee-chop1', and `ee-chop2'.
Example: 
  (ee-chop3 4 1 '(Greek \"Pi Sigma Theta\" \"Pi Si Th\" (P S T)
                  greek \"delta epsilon\"  \"dd ee\"    (d e)))
            ==> ((Greek \"Pi\"      \"Pi\" P)
                 (Greek \"Sigma\"   \"Si\" S)
                 (Greek \"Theta\"   \"Th\" T)
                 (greek \"delta\"   \"dd\" d)
                 (greek \"epsilon\" \"ee\" e))"
  (let* ((seqs (ee-chop1 n (ee-map-split bigseq)))
	 (columns (mapcar (lambda (seq) (ee-chop2 ref seq)) seqs)))
    (apply 'append columns)))


;;;                  _   _             _             _         
;;;  _ __ ___   __ _| |_| |__     __ _| |_   _ _ __ | |__  ___ 
;;; | '_ ` _ \ / _` | __| '_ \   / _` | | | | | '_ \| '_ \/ __|
;;; | | | | | | (_| | |_| | | | | (_| | | |_| | |_) | | | \__ \
;;; |_| |_| |_|\__,_|\__|_| |_|  \__, |_|\__, | .__/|_| |_|___/
;;;                              |___/   |___/|_|              

(defun eev-math-glyphs-set (face names pairs chars &rest rest)
  "This is hard to explain, but there's an example at:...
    (find-efunction 'eev-math-glyphs-edrx)"
  (setq names (split-string names " +"))
  (setq pairs (split-string pairs " +"))
  (setq chars (split-string chars " +"))
  (while names
    (let ((n (car names)) (p (car pairs)) (c (car chars)))
      (if (= 1 (length c))
	  (let ((c (string-to-char c)))
	    (eev-set-glyph c (ee-aref eev-math-glyphs-name-to-char n) face)
	    (setq eev-composes-localmath
		  (lax-plist-put eev-composes-localmath p c)))))
    (setq names (cdr names) pairs (cdr pairs) chars (cdr chars)))
  (if rest (apply 'eev-math-glyphs-set rest)
    (eev-composes-update)))

;; A demo: this function sets up the glyphs that I (Edrx)
;; use for most of my TeXed notes.
(defun eev-math-glyphs-edrx ()
  "Sets up a certain set of mathematical glyphs for TeX, with compose pairs."
  (interactive)
  (eev-math-glyphs-set
   'eev-glyph-face-Greek
   "Theta Pi Sigma Omega"
   "Th    Pi Si    Om   "
   "Þ     å  Æ     Ø    "
   'eev-glyph-face-greek
   "delta epsilon theta lambda nu pi rho sigma tau omega"
   "dd    ee      te    ll     nu pi ro  si    tt  ww   "
   "                 ð      Û               Ï    "
   'eev-glyph-face-logical
   "top bot land lor supset forall exists box thin: in circ"
   "TT  bt  la   lo  im     fa     ex     bo  ::    in oo  "
   "§   ®   ´       ¶      ý      Î      ñ   ¨     Ý  ¢   "
   'eev-glyph-face-math
   "cap cup Int nabla infty ge ^1"
   "ca  cu  In  na    88    >= -1"
   "Ì   þ   Å   ¿     ‚     ©  ³ "
   'eev-glyph-face-linear
   "sqcap sqcup ud& oplus otimes lolli"
   "ka    ku    &&  o+    ox     -o"
   "     ÷     Ñ   ¥     ¤      ¸ "
   'eev-glyph-face-graphic
   "dotli nat seblock neblock"
   "..    bq  bl      ^^     "
   "(÷)   î   ­       £      "
   'eev-glyph-face-font
   "b  r  t  s "
   "bf rm tx ss"
   "¦  ¯  Ë  Ð "))

;; ÞåÆØðÛÏ
;; §®´¶ýÎñ¨Ý¢ÌþÅ¿‚©
;; ÷Ñ¥¤¸î­£¦¯ËÐ

;; To set up a certain set of TeX glyphs:
;;   (require 'eev-math-glyphs)
;;   (eev-math-glyphs-edrx)
;; To undo (i.e., to remove all local math glyphs):
;;   (eev-math-glyphs-reset)
;; An example of TeX code using glyphs:
"
%
% (ee-once (eelatex-bounded))
\catcode`¦=13 \def¦{\mathbf}

A natural transformation from $F:¦A \to ¦B$ to $G:¦A \to ¦B$ is...
%
"
;; See: (find-texbookpage 'appendixI)
;;      (find-texbookpage (+ 11 39))

;; 2007nov09: note: this is very far from obvious, but
;; `eev-math-glyphs-set' can also be used to change individual glyphs.
;; It is convenient to add a name for the character that they use to
;; `eev-math-glyphs-name-to-char'. Here's an example:
;;   (add-to-alist 'eev-math-glyphs-name-to-char '("Delta" . 332660))
;;   (eev-math-glyphs-set 'eev-glyph-face-math  "nabla" "na" "¿")
;;   (eev-math-glyphs-set 'eev-glyph-face-math  "Delta" "DD" "¿")
;;   (find-glyphashtml-links "na")
;;   (find-glyphashtml-links "DD")
;; That example has some problems that I will try to fix later...
;; (1) `add-to-alist' is not standard. Links:
;;       (find-angg ".emacs" "add-to-alist")
;;       http://angg.twu.net/.emacs.html#add-to-alist
;;       http://lists.gnu.org/archive/html/bug-gnu-emacs/2001-02/msg00066.html
;; (2) `find-glyphashtml-links' is even less standard... it's
;;     currently only at:
;;       (find-angg ".emacs" "find-glyphashtml-links")
;;       http://angg.twu.net/.emacs.html#find-glyphashtml-links
;;     and I created it (in a hurry) because I wanted a quick way to
;;     htmlize a glyph, for:
;;       (find-angg "blogme3/anggdefs.lua")
;;       (find-angg "blogme3/anggdefs.lua" "eev_math_glyphs_edrx")



(provide 'eev-math-glyphs)

;; Local Variables:
;; coding:               raw-text-unix
;; ee-anchor-format:     "«%s»"
;; End:
