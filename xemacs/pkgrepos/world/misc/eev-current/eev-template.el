;;; eev-template.el --- create and insert Elisp hyperlinks

;; Load with: (load "eev-template.el")

;; Copyright (C) 2012 Free Software Foundation, Inc.
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
;; Version:    2012may06
;; Keywords:   e-scripts, help, hyperlinks, hypertext

;;; Commentary:

;; 2012jan15: Major rewrite!
;; This is a replacement for eev-insert.el.
;;
;; This file is divided in four parts:
;;   Part 1: basic support functions
;;   Part 2: workhorse functions: `find-elinks', `ee-wrapc', `ee-wrapt'
;;   Part 3: applications
;;   Part 4: etc

;; The "applications" are of two main kinds: "wrappings", that
;; transform the contents of the current line to produce hyperlinks,
;; and "hyperlinks to temporary buffers"...
;;   Wrappings:
;;     (find-efunctiondescr 'eev-mode "Commands to convert")
;;   Hyperlinks to temporary buffers:
;;     (find-efunctiondescr 'eev-mode "Commands to generate pages")



;; (find-efunctiondescr 'find-file-links)


;; This file defines three workhorse functions - `ee-wrapc',
;; `ee-wrapt' and `find-elinks'

;; Quick index:
;; «.basic-tools»		(to "basic-tools")
;; «.ee-links-to-string»	(to "ee-links-to-string")
;; «.find-elinks»		(to "find-elinks")







;;;  ____            _     _   
;;; |  _ \ __ _ _ __| |_  / |_ 
;;; | |_) / _` | '__| __| | (_)
;;; |  __/ (_| | |  | |_  | |_ 
;;; |_|   \__,_|_|   \__| |_(_)
;;;                            
;;;  ____            _        _              _     
;;; | __ )  __ _ ___(_) ___  | |_ ___   ___ | |___ 
;;; |  _ \ / _` / __| |/ __| | __/ _ \ / _ \| / __|
;;; | |_) | (_| \__ \ | (__  | || (_) | (_) | \__ \
;;; |____/ \__,_|___/_|\___|  \__\___/ \___/|_|___/
;;;                                                
;; «basic-tools»  (to ".basic-tools")

(defun ee-splitn (n str)
"Example: (ee-splitn 3 \"aa bb cc dd ee\")
             --> (\"aa\" \"bb\" \"cc dd ee\")"
  (if (= n 1) (list str)
    (if (string-match "^\\`[ \t]*\\([^ \t]+\\)[ \t]*" str)
	(cons (match-string 1 str)
	      (ee-splitn (- n 1) (substring str (match-end 0))))
      (cons "" (ee-splitn (- n 1) "")))))

(defun ee-zip (list1 list2)
"Example: (ee-zip '(a b c) '(aa bb cc))
            --> ((a aa) (b bb) (c cc))"
  (if list1
      (cons (list (car list1) (car list2))
	    (ee-zip (cdr list1) (cdr list2)))))

(defun ee-delete-and-extract-line ()
  (delete-and-extract-region (point-at-bol) (point-at-eol)))

(defun ee-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)

;; ee-gformat is hard to explain. See:
;;   (find-es "emacs" "ee-gformat")
;;
(defun ee-gformat (fmt)
  `(lambda (str) (replace-regexp-in-string
		  "\\`\\(.\\|\n\\)*\\'" ,fmt str 'fixedcase)))

(defun ee-gmapconcat-split (fmt sep str)
  (mapconcat (ee-gformat fmt) (ee-split str) (or sep "")))


;; "pp0" -> "pretty-print a Lisp object in a very compact way".
;; See: (find-elnode "Output Functions" "prin1-to-string")
;;      (find-elnode "Output Variables" "print-escape-newlines")
;;
(defun ee-pp0 (object &optional tick)
  "Convert OBJECT (usually a sexp) into a string, for use in hyperlinks.
Quote newlines to make it fit in a single line.
If TICK is non-nil and OBJECT is a list then precede it with a \"'\".
The result of this function is always a string that can be `read' as Lisp."
  (let ((str (let ((print-escape-newlines t)
		   (print-escape-nonascii t) ; isn't escaping esc, \r, etc
		   (print-quoted t))
	       (prin1-to-string object))))
    (setq str (replace-regexp-in-string "\r" "\\\\r" str))
    (if (and tick (consp object))
	(setq str (concat "'" str)))
    str))


;; "pph" -> "pretty-print a hyperlink"
;; Hyperlink lines start with ee-hyperlink-prefix.
;; Example: (ee-pph '(find-efunction 'ee-pp0))
;;
(defun ee-addhp (string)
  "Prepend `ee-hyperlink-prefix' to STRING"
  (concat ee-hyperlink-prefix string))

(defun ee-pph (object &optional tick)
  "Pretty-print OBJECT into a hyperlink line."
  (ee-addhp (ee-pp0 object tick)))

(defun ee-pph0 (object &optional tick nil-nl nl)
  "A low-level function used by `ee-pphlist0'."
  (if (null object) nil-nl
    (if (stringp object) (concat object nl)
      (concat (ee-pph object) nl))))

(defun ee-symbol-to-pair (o)
  "When O is a symbol return its name and value; otherwise return O itself.
This is an internal function used by `ee-template'."
  (if (symbolp o)
      (list (symbol-name o) (symbol-value o))
    o))

(defun ee-template (pairs templatestr)
  "Substitute all ocurrences of \"{tagname}\"s in TEMPLATESTR.
PAIRS is a list of pairs of strings, in the form (tagname replacement),
but with a hack: each symbol in PAIRS is treated as a variable,
and substituted by the pair (varname varvalue).
Examples (try them!):\n
\(ee-template
  '((\"a\" \"AA\") (\"b\" \"CC\") (\"c\" \"CC\"))
  \"foo{a}bar{bla}poo{b}bletch\")\n
\(ee-template
  '((\"http\" \"https\") (\"a/b/\" \"foo/bar/\") (\"c\" \"index.html\"))
\" (eepitch-shell)
mkdir -p $S/{http}/{a/b/}
cd       $S/{http}/{a/b/}
wget     '{http}://{a/b/}{c}'
echo     '{http}://{a/b/}{c}' >> ~/.psne.log\")"
  (setq pairs (mapcar 'ee-symbol-to-pair pairs))
  (let ((f (lambda (match)
	     (or (cadr (assoc (match-string 1 match) pairs))
		 match))))
    (replace-regexp-in-string "{\\([^{}]+\\)}" f templatestr 'fixedcase)))

(defun ee-template1 (pairs templatestr)
  "Like `ee-template', but adds a substitution for \"{# }\" to PAIRS."
  (ee-template (cons `("# " ,ee-hyperlink-prefix) pairs) templatestr))

(defun ee-format-kbd-macro (key)
  "Example: (ee-format-kbd-macro [down])  --> \"<down>  ;; next-line\""
  (replace-regexp-in-string "[ \t][ \t]+" "  " (format-kbd-macro key t)))

(defun ee-setqs (vars values)
"Example: (let (a b) (ee-setqs '(a b) '(22 33)) (+ a b)) --> 55"
  (when vars
     (set (car vars) (car values))
     (ee-setqs (cdr vars) (cdr values))))




;;;  ____            _     ____    
;;; |  _ \ __ _ _ __| |_  |___ \ _ 
;;; | |_) / _` | '__| __|   __) (_)
;;; |  __/ (_| | |  | |_   / __/ _ 
;;; |_|   \__,_|_|   \__| |_____(_)
;;;                                
;;;                     _    _                         
;;; __      _____  _ __| | _| |__   ___  _ __ ___  ___ 
;;; \ \ /\ / / _ \| '__| |/ / '_ \ / _ \| '__/ __|/ _ \
;;;  \ V  V / (_) | |  |   <| | | | (_) | |  \__ \  __/
;;;   \_/\_/ \___/|_|  |_|\_\_| |_|\___/|_|  |___/\___|
;;;                                                    
;;;   __                  _   _                 
;;;  / _|_   _ _ __   ___| |_(_) ___  _ __  ___ 
;;; | |_| | | | '_ \ / __| __| |/ _ \| '_ \/ __|
;;; |  _| |_| | | | | (__| |_| | (_) | | | \__ \
;;; |_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|___/
;;;                                             



;;;   __ _           _            _ _       _        
;;;  / _(_)_ __   __| |       ___| (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _ \ | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  __/ | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___|_|_|_| |_|_|\_\___/
;;;                                                  
;; «ee-links-to-string»  (to ".ee-links-to-string")
;; «find-elinks»  (to ".find-elinks")

(defun ee-links-to-string0 (tick nil-nl nl sep list)
  "Convert LIST into a multi-line string (a list of hyperlinks).
This is a low-level function used by `ee-links-to-string'.
The parameter TICK is passed to `ee-pph'.
If NIL-NL is \"\", nils in LIST disappear;
if NIL-NL is \"\\n\", nils in LIST become empty lines.
if NL is \"\\n\" and SEP is \"\" the result terminates with a newline.
if NL is \"\" and SEP is \"\\n\" the result does not have a terminating
newline."
  (mapconcat (lambda (obj) (ee-pph0 obj tick nil-nl nl))
	     (ee-non-nil-items list)
	     sep))

(defun ee-links-to-string (links)
  "Convert a list of (raw) hyperlinks to a string.
Each element of LINKS becomes a line in the result - except for
nils, that are ignored.
Non-string objects in LINKS are \"quoted\" with `ee-pph'.
Strings in LINKS become lines by themselves.
The result of this function is not newline-terminated (usually)."
  ;; (ee-links-to-string0 nil "\n" "\n" "" links)
  ;; (ee-links-to-string0 nil "" "" "\n" links)
  (ee-links-to-string0 nil nil "" "\n" links)
  )

(defun find-elinks (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*")))
    (apply 'find-estring (ee-links-to-string links) pos-spec-list)))

(defun find-elinks-elisp (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines.
The buffer is put in Emacs Lisp mode."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*")))
    (apply 'find-estring-elisp (ee-links-to-string links) pos-spec-list)))


;; (ee-links-for-debpkg "adduser")
;; (find-elinks '(foo (bar) "plic"))
;; (find-elinks (list (ee-links-for-debpkg "adduser")))


;;;                                               
;;;   ___  ___     __      ___ __ __ _ _ __   ___ 
;;;  / _ \/ _ \____\ \ /\ / / '__/ _` | '_ \ / __|
;;; |  __/  __/_____\ V  V /| | | (_| | |_) | (__ 
;;;  \___|\___|      \_/\_/ |_|  \__,_| .__/ \___|
;;;                                   |_|         
;;
;; A low-level helper function for building `ee-wrap-*' functions.
;; The argument CODE is evaluated in a context in which each of
;; the symbols in ARGLIST has a value as a variable.
;; For a typical use, see the definition of `ee-wrap-sh'.

(defun ee-wrapc (arglist code)
"Eval CODE in a let block that sets the variables in ARGLIST.
Example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(defun f () (interactive)
  (ee-wrapc '(a b c)
	    '(insert (format \"a=%S b=%S c=%S\" a b c))))
\(eek \"2*<down> 5*<<f>>\")\n
aa bb cc dd ee
aa bb cc dd
aa bb cc
aa bb
aa"
  (eval
   (let* ((n       (length arglist))
	  (line    (ee-delete-and-extract-line))
	  (data0   (ee-splitn n line))
	  (data    (mapcar 'ee-no-properties data0))
	  (pairs   (ee-zip arglist data))
	  (newcode `(let ,pairs ,code)))
     newcode))
  (ee-next-line))

;;;                                          _   
;;;   ___  ___     __      ___ __ __ _ _ __ | |_ 
;;;  / _ \/ _ \____\ \ /\ / / '__/ _` | '_ \| __|
;;; |  __/  __/_____\ V  V /| | | (_| | |_) | |_ 
;;;  \___|\___|      \_/\_/ |_|  \__,_| .__/ \__|
;;;                                   |_|        
;;
;; This is similar to `ee-wrapc', but using templates.
;; For typical uses, see the definitions for `ee-wrap-eepitch' and
;; `ee-wrap-debian', below.

(defun ee-wrapt (argnames templatestr)
"Replace the current line by TEMPLATESTR, expanding the tags in ARGNAMES.
Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(defun f () (interactive) (ee-wrapt \"a b c\" \"a={a} b=({b}) c=<{c}>\"))
\(eek \"2*<down> 5*<<f>>\")\n
aa bb cc dd ee
aa bb cc dd
aa bb cc
aa bb
aa"
  (let* ((argnamess   (ee-split argnames))
         (n           (length   argnamess))
         (line        (ee-delete-and-extract-line))
         (args        (ee-splitn n line))
         (pairs       (ee-zip argnamess args))
	 (replacement (ee-template1 pairs templatestr)))
    (insert replacement)
    (ee-next-line)))






;;;  ____            _     _____         
;;; |  _ \ __ _ _ __| |_  |___ /  __ _ _ 
;;; | |_) / _` | '__| __|   |_ \ / _` (_)
;;; |  __/ (_| | |  | |_   ___) | (_| |_ 
;;; |_|   \__,_|_|   \__| |____/ \__,_(_)
;;;                                      
;;;                                 _                 
;;; __      ___ __ __ _ _ __  _ __ (_)_ __   __ _ ___ 
;;; \ \ /\ / / '__/ _` | '_ \| '_ \| | '_ \ / _` / __|
;;;  \ V  V /| | | (_| | |_) | |_) | | | | | (_| \__ \
;;;   \_/\_/ |_|  \__,_| .__/| .__/|_|_| |_|\__, |___/
;;;                    |_|   |_|            |___/     


(defun ee-wrap-file ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> <<ee-wrap-file>>\")\n
/etc/\n" 
  (interactive)
  (ee-wrapc '(str) '(insert (ee-pph `(find-fline ,str)))))

(defun ee-wrap-man ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> <<ee-wrap-man>>\")\n
1 tac\n" 
  (interactive)
  (ee-wrapc '(str) '(insert (ee-pph `(find-man   ,str)))))

(defun ee-wrap-sh ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> 3*<<ee-wrap-sh>>\")\n
{ echo \"pwd: $(pwd)\"; date }
{ echo \"pwd: $(pwd)\"; date } | tac
seq 2 10 | awk '{print $1 \"->\" $1*$1}'"
  (interactive)
  (ee-wrapc '(str) '(insert (ee-pph `(find-sh    ,str)))))

(defun ee-wrap-sh0  () (interactive)
  (ee-wrapc '(str) '(insert (ee-pph `(find-sh0   ,str)))))

(defun ee-wrap-zsh  () (interactive)
  (ee-wrapc '(str) '(insert (ee-pph `(find-zsh   ,str)))))

(defun ee-wrap-eepitch ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> <<ee-wrap-eepitch>>\")\n
shell\n" 
  (interactive)
  (ee-wrapt "str" "\
 (eepitch-{str})
 (eepitch-kill)
 (eepitch-{str})"))

(defun ee-wrap-debian ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> <<ee-wrap-debian>>\")\n
bash\n"
  (interactive)
  (ee-wrapt "stem" "\
{# }(find-status   \"{stem}\")
{# }(find-vldifile \"{stem}.list\")
{# }(find-udfile   \"{stem}/\")"))

(defun ee-wrap-code-c-d ()
  "Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(eek \"2*<down> <<ee-wrap-debian>>\")\n
bash\n"
  (interactive)
  (ee-wrapt "c d" "\
\(code-c-d \"{c}\" \"{d}\"\)
;; (find-{c}file \"\")"))




;;;  _         _____   __  __                _   _                 
;;; | |    __ |_   _|__\ \/ /  ___  ___  ___| |_(_) ___  _ __  ___ 
;;; | |   / _` || |/ _ \\  /  / __|/ _ \/ __| __| |/ _ \| '_ \/ __|
;;; | |__| (_| || |  __//  \  \__ \  __/ (__| |_| | (_) | | | \__ \
;;; |_____\__,_||_|\___/_/\_\ |___/\___|\___|\__|_|\___/|_| |_|___/
;;;                                                                

(defun ee-links-for-latexsection (symbol sectioncmd spaces title tag)
  "Used by `s' and friends; see the source code."
  ;; A test:
  ;; (find-estring (ee-links-for-latexsection 's "myslide" "  " "TIT" "TAG"))
  (ee-template `(symbol spaces sectioncmd title tag
		 ("sexp" ,(ee-pp0 (list symbol title tag)))) "\
% --------------------
% {spaces}«.{tag}»\t(to \"{tag}\")
% «{tag}»  (to \".{tag}\")
% {sexp}
\\{sectioncmd} {{title}} {{tag}}
"))

;; (find-efunction-links 's)
;;
(defun s (title tag)
  "Replace the current line by a LaTeX \"\\myslide\" header.
Try this example:\n
\(toggle-read-only 0)
\(buffer-enable-undo)
\(s \"Slide Title\" \"slide-tag\")"
  (ee-delete-and-extract-line)
  (insert (ee-links-for-latexsection 's "myslide" "  " title tag)))







;;;  ____            _     _____ _      
;;; |  _ \ __ _ _ __| |_  |___ /| |__ _ 
;;; | |_) / _` | '__| __|   |_ \| '_ (_)
;;; |  __/ (_| | |  | |_   ___) | |_) | 
;;; |_|   \__,_|_|   \__| |____/|_.__(_)
;;;                                     
;;;   __ _           _                       _ _       _        
;;;  / _(_)_ __   __| |       _____/\__     | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _ \    /_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  __/_  _\_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___| \/       |_|_|_| |_|_|\_\___/
;;;                                                             


;;;   __ _           _            _              
;;;  / _(_)_ __   __| |       ___| | _____ _   _ 
;;; | |_| | '_ \ / _` |_____ / _ \ |/ / _ \ | | |
;;; |  _| | | | | (_| |_____|  __/   <  __/ |_| |
;;; |_| |_|_| |_|\__,_|      \___|_|\_\___|\__, |
;;;                                        |___/ 
;; (find-ekey-links     "\C-x2")
;; (find-elongkey-links  "C-x 2")
;; (find-efunction-links 'split-window-vertically)
;; (find-ekey-links      [down])
;; (find-elongkey-links "<down>")
;; (find-efunction-links '+)
;; (find-efunction-links 'next-line)
;; (find-evariable-links 'default-directory)

(defun ee-links-for-eboundkey (key f)
  "From KEY and its binding, F, produce a list of hyperlinks.
This is an internal function used by `find-ekey-links'."
  `((where-is ',f)
    (describe-function ',f)
    (find-efunctiondescr ',f)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    ""
    (describe-key ,key)
    (describe-key-briefly ,key)
    (find-ekeydescr ,key)
    (Info-goto-emacs-key-command-node ,key)
    (Info-goto-emacs-command-node ',f)
    (find-enode "Command Index" ,(format "* %S:" f))
    (find-elnode "Index" ,(format "* %S:" f))
    ""
    (key-description ,key)
    (format-kbd-macro ,key)
    (format-kbd-macro ,key t)
    (ee-format-kbd-macro ,key)
    (key-binding ,key)
    ))

(defun find-ekey-links (key &rest rest)
"Visit a temporary buffer containing hyperlinks related to the key sequence KEY."
  (interactive "kElisp hyperlinks for key: ")
  (let ((longkey     (format-kbd-macro key))
	(longkey+ (ee-format-kbd-macro key))
	(binding          (key-binding key)))
    (apply 'find-elinks
	   `((find-ekey-links          ,key)
	     (eek ,(format "M-h M-k %s" longkey))
	     (eek ,(format "M-h M-k %s" longkey+))
	     ""
	     (find-elongkey-links      ,longkey)
	     (find-elongkey-links      ,longkey+)
	     (find-efunction-links    ',binding)
	     ""
	     ,@(ee-links-for-eboundkey key binding)
	     )
	   rest)))

;; Deleted entries:
;; ,@(eemakelinks-eboundkey key binding)
;; (eekill ,longkey)
;; ,longkey+

(defun find-elongkey-links (longkey &rest rest)
  "Like `find-ekey-links', but LONGKEY is a key sequence \"spelled out\".
Example: (find-elongkey-links \"M-h M-k\")
See `read-kbd-macro' and `edmacro-mode' for the format."
  (interactive "sElisp hyperlinks for key (long format): ")
  (let* ((key (read-kbd-macro longkey))
	 (binding (key-binding key)))
    (apply 'find-elinks
	   `((find-elongkey-links   ,longkey)
	     (find-ekey-links       ,key)
	     (find-efunction-links ',binding)
	     ""
	     ,@(ee-links-for-eboundkey key binding)
	     )
	   rest)))



;;;   __ _           _             __                  _   _             
;;;  / _(_)_ __   __| |       ___ / _|_   _ _ __   ___| |_(_) ___  _ __  
;;; | |_| | '_ \ / _` |_____ / _ \ |_| | | | '_ \ / __| __| |/ _ \| '_ \ 
;;; |  _| | | | | (_| |_____|  __/  _| |_| | | | | (__| |_| | (_) | | | |
;;; |_| |_|_| |_|\__,_|      \___|_|  \__,_|_| |_|\___|\__|_|\___/|_| |_|
;;;                                                                      

(defun ee-links-for-efunction (f)
  "Return a list of hyperlinks for F (a function symbol).
This is an internal function used by `find-efunction-links'."
  `((where-is ',f)
    (describe-function ',f)
    (find-efunctiondescr ',f)
    (find-efunction ',f)
    (find-efunctionpp ',f)
    (find-efunctiond ',f)
    (find-eCfunction ',f)
    (find-estring (documentation ',f))
    (find-estring (documentation ',f t))
    ""
    ,@(if (commandp f)
	  `((Info-goto-emacs-command-node ',f)
	    (find-enode "Command Index" ,(format "* %S:" f))
	    ))
    (find-elnode "Index" ,(format "* %S:" f))
    ))

(defun find-efunction-links (f &rest rest)
  "Visit a temporary buffer containing hyperlinks related to the function F."
  (interactive (find-function-read))
  (apply 'find-elinks
	 `((find-efunction-links ',f)
	   ""
	   ;; ,@(eemakelinks-efunction f)
	   ,@(ee-links-for-efunction f)
	   )
	 rest))


;;;   __ _           _                            _       _     _      
;;;  / _(_)_ __   __| |       _____   ____ _ _ __(_) __ _| |__ | | ___ 
;;; | |_| | '_ \ / _` |_____ / _ \ \ / / _` | '__| |/ _` | '_ \| |/ _ \
;;; |  _| | | | | (_| |_____|  __/\ V / (_| | |  | | (_| | |_) | |  __/
;;; |_| |_|_| |_|\__,_|      \___| \_/ \__,_|_|  |_|\__,_|_.__/|_|\___|
;;;                                                                    


(defun ee-links-for-evariable (var)
  "Return a list of hyperlinks for VAR (a variable, as a symbol).
This is an internal function used by `find-evariable-links'."
  `(,var
    (describe-variable ',var)
    (find-evardescr ',var)
    (find-evariable ',var)
    (find-eCvariable ',var)
    (find-epp ,var)
    ""
    (find-enode "Variable Index" ,(format "* %S:" var))
    (find-elnode "Index" ,(format "* %S:" var))
    ))

(defun find-evariable-links (var &rest rest)
  "Visit a temporary buffer containing hyperlinks related to the variable VAR."
  (interactive (find-function-read 'variable))
  (apply 'find-elinks
	 `((find-evariable-links ',var)
	   ""
	   ;; ,@(eemakelinks-evariable var)
	   ,@(ee-links-for-evariable var)
	   )
	 rest))



;;;   __ _           _            _        __       
;;;  / _(_)_ __   __| |       ___(_)_ __  / _| ___  
;;; | |_| | '_ \ / _` |_____ / _ \ | '_ \| |_ / _ \ 
;;; |  _| | | | | (_| |_____|  __/ | | | |  _| (_) |
;;; |_| |_|_| |_|\__,_|      \___|_|_| |_|_|  \___/ 
;;;                                                 

(defun ee-info-node   () (with-current-buffer "*info*" Info-current-node))
(defun ee-info-book+  () (with-current-buffer "*info*" Info-current-file))
(defun ee-info-book-  () (file-name-nondirectory (ee-info-book+)))
(defun ee-info-file-  () (file-name-nondirectory  ee-info-file))
(defun ee-info-shortp () (string= (ee-info-book-) (ee-info-file-)))
(defun ee-info-long-sexp (symbol)
  (list symbol (format "(%s)%s" (ee-info-book-) (ee-info-node))))
(defun ee-info-short-sexp ()
  (list (intern (format "find-%snode" ee-info-code)) (ee-info-node)))

(defun find-einfo-links (&rest rest)
  "Visit a temporary buffer containing hyperlinks to the current info page.
When possible, try to produce also a shorter hyperlink, like the last one in:
  (info \"(bashref)Pipelines\")
  (find-node \"(bashref)Pipelines\")
  (find-bashnode \"Pipelines\")
The hack for generating the shorter hyperlink uses the global
variables `ee-info-code' and `ee-info-file' - see:
  (progn
   (find-code-c-d \"bash\" \"/usr/share/doc/bash/examples/\" \"bashref\")
   (ee-goto-position \"ee-info-code\"))"
  (interactive)
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-einfo-links ,@rest)
    ;; Body:
    ""
    ,(ee-info-long-sexp 'info)
    ,(ee-info-long-sexp 'find-node)
    ,(if (ee-info-shortp) (ee-info-short-sexp))
    ) rest))



;;;   __ _           _        __ _ _            _ _       _        
;;;  / _(_)_ __   __| |      / _(_) | ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| |_| | |/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  _| | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|_|\___|     |_|_|_| |_|_|\_\___/
;;;                                                                

(defun ee-filter (function list)
  "Return a sublist of LIST with only the elements for which (FUNCTION elt) is true."
  (let (newlist)
    (while (consp list)
      (if (funcall function (car list))
	  (setq newlist (cons (car list) newlist)))
      (setq list (cdr list)))
    (nreverse newlist)))

(defun ee-non-nil-items (list)
  "Return a list like LIST, but without the `nil's."
  (ee-filter 'identity list))

(defun ee-prefixp (prefix str)
  "Return t if STR begins with PREFIX."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))))

(defun ee-remove-prefix (prefix str)
  "Example: (ee-remove-prefix \"ab\" \"abcde\") --> \"cde\""
  (substring str (length prefix)))

(defun code-c-d-prefixes (path)
  "Return the entries (C D) in `code-c-d-list' for which D is a prefix of PATH." 
  (ee-filter (lambda (c-d) (ee-prefixp (car (cdr c-d)) path))
	     code-c-d-list))

(defun ee-find-xxxfile-link (c d path)
  "Example: (ee-find-xxxfile-link \"cc\" \"/foo/\" \"/foo/bar\")
   --> (find-ccfile \"bar\")"
  (list (intern (format "find-%sfile" c))
	(ee-remove-prefix d path)))

(defun ee-find-xxxfile-links (path)
  "An internal function used by `find-file-links'."
  (mapcar (lambda (c-d) (ee-find-xxxfile-link (nth 0 c-d) (nth 1 c-d) path))
	  (code-c-d-prefixes path)))

(defun ee-find-file-extra-links (fname)
  "See `find-file-links'. This is just a proof of concept at the moment."
  (let ((home (ee-expand "$HOME/")))
    (if (ee-prefixp home fname)
	(let ((fname- (ee-remove-prefix home fname)))
	  `((find-file  ,(concat "~/" fname-))
	    (find-fline ,(concat "~/" fname-))
	    ,(format "http://angg.twu.net/%s"      fname-)
	    ,(format "http://angg.twu.net/%s.html" fname-)
	    )))))

;; To do: add features from:
;; (find-angg ".emacs" "find-file-links")
;;
(defun find-file-links (fname &rest rest)
  "Visit a temporary buffer containing hyperlinks to the current file.
The list of hyperlinks will contain all known shorter hyperlinks
that point to the current file. For example, after\n
  (code-c-d \"bashdoc\" \"/usr/share/doc/bash/\" \"bashref\")
  (code-c-d \"bash\"    \"/usr/share/doc/bash/examples/\" \"bashref\")\n
the temporary buffer generated by:\n
  (progn
   (find-bashfile \"functions/\")
   (eek \"<<find-file-links>>\"))\n
will have these hyperlinks, among several others:\n
  (find-bashdocfile \"examples/functions/\")
  (find-bashfile \"functions/\")\n
See:
  (find-evardescr 'code-c-d-list)
  (code-c-d-prefixes \"/usr/share/doc/bash/examples/functions/\")
  (ee-find-xxxfile-links \"/usr/share/doc/bash/examples/functions/\")"
  (interactive (list (or (buffer-file-name) default-directory)))
  (apply 'find-elinks
	 `((find-file-links ,fname)
	   ,@(if (fboundp 'ee-find-file-extra-links)
		 (ee-find-file-extra-links fname))
	   ""
	   (find-file ,fname)		; non-refinable
	   (find-fline ,fname)		; refinable
	   ,@(ee-find-xxxfile-links (ee-expand fname)))
	 rest))

;; (find-evardescr 'code-c-d-list)
;;                                     (ee-eevpuroddfile "foo")
;;              (ee-find-xxxfile-links (ee-eevpuroddfile "foo"))
;; (find-elinks (ee-find-xxxfile-links (ee-eevpuroddfile "foo")))
;; (find-file-links (ee-eevpuroddfile "foo"))

;; To do:
;;   shorten file names (/home/edrx -> ~)
;;   add links to code to change permissions

;;;  _                           _ _       _                         __ _      
;;; | |__  _   _ _ __   ___ _ __| (_)_ __ | | __     _ __  _ __ ___ / _(_)_  __
;;; | '_ \| | | | '_ \ / _ \ '__| | | '_ \| |/ /____| '_ \| '__/ _ \ |_| \ \/ /
;;; | | | | |_| | |_) |  __/ |  | | | | | |   <_____| |_) | | |  __/  _| |>  < 
;;; |_| |_|\__, | .__/ \___|_|  |_|_|_| |_|_|\_\    | .__/|_|  \___|_| |_/_/\_\
;;;        |___/|_|                                 |_|                        

(defun ee-hyperlink-prefix ()
  "A lispish interface for customizing the variable `ee-hyperlink-prefix'.
See the comments in the source code."
  (interactive)
  (find-elinks
   `((ee-hyperlink-prefix)
     ;; Convention: the first sexp always regenerates the buffer.
     (setq ee-hyperlink-prefix ,ee-hyperlink-prefix) ; current value
     ""
     (setq ee-hyperlink-prefix "# ")	; other common values
     (setq ee-hyperlink-prefix ";; ")
     (setq ee-hyperlink-prefix "-- ")
     (setq ee-hyperlink-prefix "% ")
     )))


;;;   __ _           _                     _                _ _       _        
;;;  / _(_)_ __   __| |      ___  ___ ___ | | ___  _ __    | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |____ / _ \/ __/ _ \| |/ _ \| '__|___| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |____|  __/ (_| (_) | | (_) | | |____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     \___|\___\___/|_|\___/|_|      |_|_|_| |_|_|\_\___/
;;;                                                                              
;; 2007sep29
;; (find-ecolor-links)
;; (find-ecolor-links "sienna")
;; (find-eev "eev-langs.el" "ee-choosecolor")
;;
(defun find-ecolor-links (&optional initialcolor)
  (interactive)
  (find-elinks
   `((find-ecolor-links ,initialcolor)
     ""
     (find-ecolor-links (ee-choosecolor ,(or initialcolor "gray")))
     (find-ecolor-links ,(or initialcolor "gray"))
     (find-ecolors)
     (find-ecolors ,initialcolor)
     ,initialcolor
     ,`(insert (propertize " Sample " 'face '(:background ,initialcolor)))
     ,`(apply 'format "#%02x%02x%02x"
	      (mapcar (lambda (c) (lsh c -8)) (color-values ,initialcolor)))
     (find-efunction 'find-ecolor-links)
     )))









;;;  ____            _     _____        
;;; |  _ \ __ _ _ __| |_  |___ /  ___ _ 
;;; | |_) / _` | '__| __|   |_ \ / __(_)
;;; |  __/ (_| | |  | |_   ___) | (__ _ 
;;; |_|   \__,_|_|   \__| |____/ \___(_)
;;;                                     
;;;      _        _               _                       _       _            
;;;  ___| |_ _ __(_)_ __   __ _  | |_ ___ _ __ ___  _ __ | | __ _| |_ ___  ___ 
;;; / __| __| '__| | '_ \ / _` | | __/ _ \ '_ ` _ \| '_ \| |/ _` | __/ _ \/ __|
;;; \__ \ |_| |  | | | | | (_| | | ||  __/ | | | | | |_) | | (_| | ||  __/\__ \
;;; |___/\__|_|  |_|_| |_|\__, |  \__\___|_| |_| |_| .__/|_|\__,_|\__\___||___/
;;;                       |___/                    |_|                         

;;;   __ _           _           _      _           _         
;;;  / _(_)_ __   __| |       __| | ___| |__  _ __ | | ____ _ 
;;; | |_| | '_ \ / _` |_____ / _` |/ _ \ '_ \| '_ \| |/ / _` |
;;; |  _| | | | | (_| |_____| (_| |  __/ |_) | |_) |   < (_| |
;;; |_| |_|_| |_|\__,_|      \__,_|\___|_.__/| .__/|_|\_\__, |
;;;                                          |_|        |___/ 
;;
(defun ee-links-for-debpkg (pkgname)
  "Return the three main links for the debian package PKGNAME."
  (list (ee-template1 `(pkgname) "\
{# }(find-status   \"{pkgname}\")
{# }(find-vldifile \"{pkgname}.list\")
{# }(find-udfile   \"{pkgname}/\")")))

(defun ee-dfs0 (pkg ext)
  "If the file /var/lib/dpkg/info/PKG.EXT exists, return a link to it."
  (let ((fname (concat pkg "." ext)))
    (if (file-exists-p (ee-vldifile fname))
	`(find-vldifile ,fname))))

(defun ee-links-for-debpkg-extra-vldi (pkg)
  "Return a list of links for files in /var/lib/dpkg/info/ belonging to PKG.
This is an internal function used by `find-debpkg-links'."
   (list (ee-dfs0 pkg "preinst")   (ee-dfs0 pkg "postinst")
	 (ee-dfs0 pkg "prerm")     (ee-dfs0 pkg "postrm")
	 (ee-dfs0 pkg "conffiles") (ee-dfs0 pkg "config")
	 (ee-dfs0 pkg "templates")
	 (ee-dfs0 pkg "md5sums")   (ee-dfs0 pkg "shlibs")
	 ))

(defun ee-debian-pooldir (pkg)
  "Used by `find-debpkg-links'; \"foo\" -> \"f\", \"libfoo\" -> \"libf\"."
  (if (string-match "^\\(lib\\)?." pkgname)
      (match-string 0 pkgname)))

(defun find-debpkg-links (&optional pkgname &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a Debian package.
Try this: (find-debpkg-links \"bash\")"
  (interactive (list (ee-debpkgname-ask)))
  (setq pkgname (or pkgname "{pkgname}"))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-debpkg-links ,pkgname ,@rest)
    (find-available ,pkgname)
    ""
    ,@(ee-links-for-debpkg pkgname)
    ""
    ,@(ee-links-for-debpkg-extra-vldi pkgname)
    ""
    ,(ee-template1 `(pkgname ("p" ,(ee-debian-pooldir pkgname))) "\
{# }(find-sh \"apt-cache dump | grep-dctrl -P {pkgname}\")
{# }(find-sh \"apt-cache search {pkgname} | sort\")
{# }(find-sh \"apt-cache showpkg {pkgname}\")
{# }(find-sh \"grep-aptavail -P {pkgname}\")

http://packages.debian.org/{pkgname}
http://packages.debian.org/src:{pkgname}
http://ftp.debian.org/debian/pool/main/{p}/{pkgname}/
http://backports.org/debian/pool/main/{p}/{pkgname}/
http://bugs.debian.org/cgi-bin/pkgreport.cgi?which=pkg&data={pkgname}&archive=no

http://packages.ubuntu.org/{pkgname}
")
    ) rest))



;;;   __ _           _           _          _           _ _     _ 
;;;  / _(_)_ __   __| |       __| |___  ___| |__  _   _(_) | __| |
;;; | |_| | '_ \ / _` |_____ / _` / __|/ __| '_ \| | | | | |/ _` |
;;; |  _| | | | | (_| |_____| (_| \__ \ (__| |_) | |_| | | | (_| |
;;; |_| |_|_| |_|\__,_|      \__,_|___/\___|_.__/ \__,_|_|_|\__,_|
;;;                                                               
;; This is a VERY EARLY prototype (hi Marc!)
;; of a rewrite of something that was very ugly.

(defun ee-dsc-url-split (dsc-url)
  "Example:
  (ee-dsc-url-split
   \"http://ftp.debian.org/debian/pool/main/i/imagemagick/imagemagick_6.2.4.5.dfsg1-0.9.dsc\")
  -> (\"http\" \"ftp.debian.org/debian/pool/main/i/imagemagick/\"
      \"imagemagick\" \"6.2.4.5.dfsg1\" \"-0.9\")"
  (let ((prot://dir/ (file-name-directory dsc-url))
	(fname-dsc (file-name-nondirectory dsc-url))
	prot dir/ xxx vvv -sv)
    (if (string-match "^\\([a-z]+\\)://\\(.*\\)" prot://dir/)
	(setq prot (match-string 1 prot://dir/)
	      dir/ (match-string 2 prot://dir/)))
    (if (string-match "^\\([^_]+\\)_\\([^-]+\\)\\(-.*\\)?\\.dsc$" fname-dsc)
	(setq xxx (match-string 1 fname-dsc)
	      vvv (match-string 2 fname-dsc)
	      -sv (or (match-string 3 fname-dsc) "")))
    (list prot dir/ xxx vvv -sv)))

(defun ee-links-for-dscbuild (dsc-url)
  (apply 'ee-links-for-dscbuild0
	 (downcase (format-time-string "%Y%b%d"))
	 (ee-dsc-url-split dsc-url)))

(defun ee-links-for-dscbuild0 (date prot dir/ xxx vvv -sv)
  (ee-template
   '(date prot dir/ xxx vvv -sv) "\
#####
#
# {xxx} (from the debian sources)
# {date}
#
#####

# <{xxx}-deb-src>
# {prot}://{dir/}
# {prot}://{dir/}{xxx}_{vvv}{-sv}.dsc
# {prot}://{dir/}{xxx}_{vvv}{-sv}.diff.gz
# {prot}://{dir/}{xxx}_{vvv}.orig.tar.gz
#
rm -Rv ~/usrc/{xxx}/
mkdir  ~/usrc/{xxx}/
cd $S/{prot}/{dir/}
cp -v {xxx}_{vvv}* ~/usrc/{xxx}/
cd     ~/usrc/{xxx}/
dpkg-source -sn -x {xxx}_{vvv}{-sv}.dsc
cd     ~/usrc/{xxx}/{xxx}-{vvv}/
dpkg-buildpackage -us -uc -b -rfakeroot     2>&1 | tee odb

#
# (find-fline \"~/usrc/{xxx}/\")
 (eepitch-shell)
cd ~/usrc/{xxx}/
sudo dpkg -i *.deb

#
# (code-c-d \"{xxx}\" \"~/usrc/{xxx}/{xxx}-{vvv}/\")
# (find-{xxx}file \"\")"))



;;;   __ _           _ /\ ____       _ _       _       /\ ____  
;;;  / _(_)_ __   __| |/\|___ \     | (_)_ __ | | ____|/\|___ \ 
;;; | |_| | '_ \ / _` |    __) |____| | | '_ \| |/ / __|   __) |
;;; |  _| | | | | (_| |   / __/_____| | | | | |   <\__ \  / __/ 
;;; |_| |_|_| |_|\__,_|  |_____|    |_|_|_| |_|_|\_\___/ |_____|
;;;                                                             
;;; find-find-links-links:
;;; a way to generate skeletons for `find-xxx-links' functions.
;;; Original code:
;;;   (find-efunction 'find-find-links-links)
;;;   (find-eev "eev-insert.el" "find-find-links-links")

;;;
;; Tests:
;; (find-find-links-links)
;; (find-find-links-links "aaa")

;; (find-estring-elisp (ee-find-links-links-defun "{xxx}" ""))
;; (find-estring-elisp (ee-find-links-links-defun "{xxx}" "a b"))

(defun ee-find-links-links-defun (xxx args)
  (ee-template
   `(xxx
     args
     ("a b "   ,(ee-gmapconcat-split "\\& "  "" args))
     (",a ,b " ,(ee-gmapconcat-split ",\\& " "" args))
     ("setqs"  ,(ee-gmapconcat-split
		 "  (setq \\& (or \\& \"{\\&}\"))\n" "" args))
     ) "\
\(defun find-{xxx}-links (&optional {a b }&rest rest)
  \"Visit a temporary buffer containing hyperlinks for {xxx}.\"
  (interactive)
{setqs}\
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-{xxx}-links {,a ,b },@rest)
    ;; Body:
    \"\"
    ,(ee-template `({a b }) \"\\
abc{foo}def
ghi{foo}jkl\")
    ) rest))"))


(defun find-find-links-links (&optional xxx args &rest rest)
"Visit a temporary buffer containing a skeleton for a `find-xxx-links' function.
Examples:
  (find-find-links-links \"xxx\")
  (find-find-links-links \"xxx\" \"a b c\")"
  (interactive)
  (setq xxx (or xxx "{xxx}"))
  (setq args (or args ""))
  (apply 'find-elinks-elisp `(
    (find-find-links-links ,xxx ,args ,@rest)
     ;; Convention: the first sexp always regenerates the buffer.
    (find-efunction 'find-find-links-links)
    ""
    ,(ee-template
      `(xxx
	("a b "   ,(ee-gmapconcat-split "\\& "  "" args))
	(",a ,b " ,(ee-gmapconcat-split ",\\& " "" args))
	("setqs"  ,(ee-gmapconcat-split
		    "  (setq \\& (or \\& \"{\\&}\"))\n" "" args))
	) "\
\(defun find-{xxx}-links (&optional {a b }&rest rest)
  \"Visit a temporary buffer containing hyperlinks for {xxx}.\"
  (interactive)
{setqs}\
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-{xxx}-links {,a ,b },@rest)
    ;; Body:
    \"\"
    ,(ee-template `({a b }(\"foo\" ,\"AAA\")) \"\\
abc{foo}def
ghi{foo}jkl\")
    ) rest))

\(define-key eev-mode-map \"\\M-h\\M-u\" 'find-{xxx}-links)

;; Test:
;; (find-{xxx}-links)")
    ) rest))


;;;                                       _       _       
;;;   ___  _____   __     _   _ _ __   __| | __ _| |_ ___ 
;;;  / _ \/ _ \ \ / /____| | | | '_ \ / _` |/ _` | __/ _ \
;;; |  __/  __/\ V /_____| |_| | |_) | (_| | (_| | ||  __/
;;;  \___|\___| \_/       \__,_| .__/ \__,_|\__,_|\__\___|
;;;                            |_|                        
;;
;; (find-eev-update-links)
;; (find-find-links-links "eev-update" "dir")

(defun find-eev-update-links (&optional dir &rest rest)
  "Visit a temporary buffer containing hyperlinks for eev-update.
Warning (2012): I haven't used this in ages!"
  (interactive)
  (setq dir (or dir "{dir}"))
  (let ((edir (or dir (ee-expand ee-eevdir))))
    (apply 'find-elinks `(
     ;; Convention: the first sexp always regenerates the buffer.
     (find-eev-update-links ,dir ,@rest)
     (find-eev-update-links ,dir ,@rest)
     (find-eev-update-links "~/eev-current/" ,@rest)
     (find-eev-update-links ,edir ,@rest)
     (find-efunction 'find-eev-update-links)
     ""
     (ee-expand ee-eevdir)
     (find-eevfile "eev.el" "$EEVDIR")
     (find-eevfile "")
     ""
     ,(ee-template `(dir edir) "\
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

mkdir {edir}
cd    {edir}
mv -v eev-current.tar.gz eev-current-old.tar.gz
wget http://angg.twu.net/eev-current.tar.gz
tar -xvzf eev-current.tar.gz

\(progn
  (add-to-list 'load-path \"{edir}\")
  (require 'eev-all)
  (eev-mode 1)
  )

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

cd {dir}
./eev-rctool prepare
./eev-rctool install_rc")
     ""
     (find-eevfile "eev-rctool")
     (find-eevsh "./eev-rctool help")
     (find-eevsh "./eev-rctool notes")
     (find-eevtmpfile "")
     (find-eevtmpfile "backup/")
     (find-eevtmpfile "backup/do_install.sh")
     (find-eevtmpfile "backup/do_uninstall.sh")
     (find-fline "~/.emacs" "Beginning of the eev block:")
     ) rest)))










;;;  ____            _     _  _    
;;; |  _ \ __ _ _ __| |_  | || | _ 
;;; | |_) / _` | '__| __| | || |(_)
;;; |  __/ (_| | |  | |_  |__   _| 
;;; |_|   \__,_|_|   \__|    |_|(_)
;;;                                
;;;   _____  _| |_ _ __ __ _ ___ 
;;;  / _ \ \/ / __| '__/ _` / __|
;;; |  __/>  <| |_| | | (_| \__ \
;;;  \___/_/\_\\__|_|  \__,_|___/
;;;                              

;; I haven't cleaned this part yet.

(defun eemklinks-duplicate-this-line ()
  "Duplicate the current line (without any changes to the kill ring)."
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol))))
    (save-excursion (beginning-of-line) (insert-before-markers line "\n"))))

(defun eemklinks-yank-pos-spec ()
  "Append the top of the kill ring to a hyperlink sexp, as a Lisp string.
This command is useful for \"refining elisp hyperlinks\" by adding a
pos-spec argument to them.  Here's an example; if you are using the
default `eev-mode-map' keybindings then

  `M-h M-i' runs `find-einfo-links',
  `M-h M-2' runs `eemklinks-duplicate-this-line', and
  `M-h M-y' runs `eemklinks-yank-pos-spec'.

Suppose that you are visiting the info node below,

  (find-enode \"Lisp Eval\")

and you find some interesting information in that page, close to
an occurrence of the string \"`defvar'\". You mark that string,
add it to the kill-ring with `M-w', then type `M-h M-i', go to
the line that contains

  # (find-enode \"Lisp Eval\")

and then you type `M-h M-2 M-h M-y'; it becomes these two lines:

  # (find-enode \"Lisp Eval\")
  # (find-enode \"Lisp Eval\" \"`defvar'\")

Now you check that the second line points to where you wanted,
and you copy that hyperlink to a more permanent place."
  (interactive)
  (goto-char (1- (point-at-eol)))	; put point before the ")"
  (insert " " (ee-pp0 (ee-no-properties (car kill-ring))))) ; insert pos-spec


;; (find-efunctiondescr 'eemklinks-yank-pos-spec)

(defun flip-psne-ness ()
  (interactive)
  (if (search-forward-regexp "\\$S/\\(https?\\|ftp\\)/\\|\\(https?\\|ftp\\)://")
      (cond ((match-string 1) (replace-match "\\1://"))
            ((match-string 2) (replace-match "$S/\\2/")))))

(define-key eev-mode-map "\M-s" 'flip-psne-ness)











;; ------------------------------------------------------------
;; Old stuff:

;; The rest of this block of comments was cut & pasted straight from
;; eev-insert.el, but most of what they say still hold...
;;
;; This is the ugliest part of eev's code. It's being rewritten. Even
;; if work on it may seem stalled, it _is_ being rewritten. In some
;; sense.
;;
;; I got tired of writing all my hyperlinks by hand, so I created
;; these functions. The "new way of creating hyperlinks" (the first
;; block of this file) adds the following key bindings to
;; eev-mode-map:
;;
;;   M-h M-k  find-ekey-links
;;   M-h M-f  find-efunction-links
;;   M-h M-v  find-evariable-links
;;   M-h M-i  find-einfo-links
;;   M-h M-d  find-debpkg-links
;;   M-h f    find-file-links
;;   M-h m    find-last-manpage-links
;;   M-h M-m  find-manpage-links
;;
;; All of them work similarly. For example: type M-h M-k RET, and
;; `find-ekey-links' will create and display a buffer called "*Elisp
;; hyperlinks*", like this:
;;
;;    _____________________________________________________________ 
;;   |(find-ekey-links "\r")                                       |
;;   |(find-elongkey-links "RET")                                  |
;;   |(find-elongkey-links "RET  ;; newline")                      |
;;   |"RET  ;; newline"                                            |
;;   |                                                             |
;;   |(where-is 'newline)                                          |
;;   |(describe-function 'newline)                                 |
;;   |(find-efunctiondescr 'newline)                               |
;;   |(find-efunction 'newline)                                    |
;;   |(find-efunctionpp 'newline)                                  |
;;   |(find-efunctiond 'newline)                                   |
;;   |(find-eCfunction 'newline)                                   |
;;   |(find-estring (documentation 'newline))                      |
;;   |(find-estring (documentation 'newline t))                    |
;;   |                                                             |
;;   |(describe-key "\r")                                          |
;;   |(describe-key-briefly "\r")                                  |
;;   |(find-ekeydescr "\r")                                        |
;;   |(Info-goto-emacs-key-command-node "\r")                      |
;;   |(Info-goto-emacs-command-node 'newline)                      |
;;   |(find-enode "Command Index" "* newline:")                    |
;;   |(find-elnode "Index" "* newline:")                           |
;;   |                                                             |
;;   |(key-description "\r")                                       |
;;   |(format-kbd-macro "\r")                                      |
;;   |(format-kbd-macro "\r" t)                                    |
;;   |(key-binding "\r")                                           |
;;   |                                                             |
;;   |                                                             |
;;   |                                                             |
;;   |--:**  *Elisp hyperlinks*   All L28     (Fundamental)--------|
;;   |_____________________________________________________________|
;;
;;
;; That is, a lot of hyperlinks pointing to interesting pieces of
;; information about the key RET and the command (`newline') that is
;; bound to it. Then you may follow these hyperlinks by evaluating the
;; sexps or you may copy them to other files by copying their text.
;;
;; [To do: explain M-h M-y. There's an example in `eesteps' format in
;; the NEWS file.]

;; See: <http://angg.twu.net/eev-current/README.html>
;; and: <http://angg.twu.net/eev-current/NEWS.html>

;; The second part of this file contains some older functions that
;; insert Elisp hyperlinks at the current buffer -- like `inn', that
;; inserts a hyperlink to the info node currently being visited -- or
;; transform text -- for example, a series of lines, each one
;; containing the name of a Debian package -- into hyperliks.


;; (find-debpkg-links "bash")

;; Missing, for variables (?): links to configuration info and to docstrings

;; (defun ee-filter-out-nils (list)
;;   "Return a list with the elements of LIST that are not nil."
;;   (ee-filter 'identity list))

;;;;
;;;; Inserting hyperlinks (new way)
;;;; This code quite recent - I wrote the first functions of it in 2004nov11.
;;;; Many function names are ugly.
;;;;

;;;
;;; Basic support functions
;;;

;; Moved to eev.el.
;; (defvar ee-hyperlink-prefix "")

;; (find-eevfile "eev-links.el" "ee-pp0")

;;;  _                           _ _       _                         __ _      
;;; | |__  _   _ _ __   ___ _ __| (_)_ __ | | __     _ __  _ __ ___ / _(_)_  __
;;; | '_ \| | | | '_ \ / _ \ '__| | | '_ \| |/ /____| '_ \| '__/ _ \ |_| \ \/ /
;;; | | | | |_| | |_) |  __/ |  | | | | | |   <_____| |_) | | |  __/  _| |>  < 
;;; |_| |_|\__, | .__/ \___|_|  |_|_|_| |_|_|\_\    | .__/|_|  \___|_| |_/_/\_\
;;;        |___/|_|                                 |_|                        



;; A lispish interface for customizing a variable.
;; Important note: this is just a proof-of-concept thing at the
;; moment, implemented for just one variable, using a function with
;; the same name of the variable. No generality at all!
;;
;; Running M-x ee-hyperlink-prefix produces a buffer like this:
;;    ________________________________________________________ 
;;   |# (ee-hyperlink-prefix)                                 |
;;   |# (setq ee-hyperlink-prefix "# ")                       |
;;   |                                                        |
;;   |# (setq ee-hyperlink-prefix "# ")                       |
;;   |# (setq ee-hyperlink-prefix ";; ")                      |
;;   |# (setq ee-hyperlink-prefix "-- ")                      |
;;   |# (setq ee-hyperlink-prefix "% ")                       |
;;   |                                                        |
;;   |                                                        |
;;   |--:**  *Elisp hyperlinks*   All L10    (Fundamental)----|
;;   |________________________________________________________|
;;
;; Note that in this interface instead of questions and buttons we
;; have sexps that can be changed and executed, and that the second
;; of those sexps (see the source code below) contains the current
;; value of the variable ee-hyperlink-prefix. The first sexp - as
;; always in these lists of hyperlinks - contains a sexp that
;; regenerates the buffer; changing the value of the variable
;; ee-hyperlink-prefix and then running the first sexp again will
;; generate the buffer again with the new value of
;; ee-hyperlink-prefix; for example, the result can be this:
;;    ________________________________________________________ 
;;   |;; (ee-hyperlink-prefix)                                |
;;   |;; (setq ee-hyperlink-prefix ";; ")                     |
;;   |                                                        |
;;   |;; (setq ee-hyperlink-prefix "# ")                      |
;;   |;; (setq ee-hyperlink-prefix ";; ")                     |
;;   |;; (setq ee-hyperlink-prefix "-- ")                     |
;;   |;; (setq ee-hyperlink-prefix "% ")                      |
;;   |                                                        |
;;   |                                                        |
;;   |--:**  *Elisp hyperlinks*   All L10    (Fundamental)----|
;;   |________________________________________________________|
;;


;; Example: running `M-x find-debpkg-links' when the
;; point is over the word "bash" runs this sexp,
;;   (find-debpkg-links "bash")
;; which creates a temporary buffer like this:
;;
;;  ___________________________________________________________________________ 
;; |# (find-debpkg-links "bash")                                               |
;; |                                                                           |
;; |# (find-available "bash")                                                  |
;; |                                                                           |
;; |# (find-status   "bash")                                                   |
;; |# (find-vldifile "bash.list")                                              |
;; |# (find-udfile   "bash/")                                                  |
;; |                                                                           |
;; |# (find-vldifile "bash.preinst")                                           |
;; |# (find-vldifile "bash.postinst")                                          |
;; |# (find-vldifile "bash.prerm")                                             |
;; |# (find-vldifile "bash.postrm")                                            |
;; |# (find-vldifile "bash.conffiles")                                         |
;; |# (find-vldifile "bash.md5sums")                                           |
;; |                                                                           |
;; |http://packages.debian.org/bash                                            |
;; |http://packages.debian.org/src:bash                                        |
;; |http://ftp.debian.org/debian/pool/main/b/bash/                             |
;; |http://backports.org/debian/pool/main/b/bash/                              |
;; |http://bugs.debian.org/cgi-bin/pkgreport.cgi?which=pkg&data=bash&archive=no|
;; |                                                                           |
;; |                                                                           |
;; |                                                                           |
;; |                                                                           |
;; |--:**  *Elisp hyperlinks*   All L1     (Fundamental)-----------------------|
;; |___________________________________________________________________________|
;;
;; For more info about the find-available and find-status links, see:
;;   (find-eev "eev.el" "find-Package")
;; As for the "find-vldifile" links, they point to files in the
;; "/var/lib/dpkg/info/" directory.
;;
;; Tests:
;; (find-elinks (ee-dfs0 "bash" "list"))
;; (find-elinks (ee-dfs0 "bash" "badextension"))
;; (find-elinks (ee-links-for-debpkg "bash"))
;; (find-sh "ls /var/lib/dpkg/info/ | awk -F . '{print $NF}' | sort | uniq")
;; (find-elinks (ee-links-for-debpkg-extra-vldi "bash"))
;; (find-elinks (ee-links-for-debpkg-extra-vldi "apache"))
;; (find-debpkg-links "bash")
;;



;;;                                                    
;;;  _ __ ___   __ _ _ __  _ __   __ _  __ _  ___  ___ 
;;; | '_ ` _ \ / _` | '_ \| '_ \ / _` |/ _` |/ _ \/ __|
;;; | | | | | | (_| | | | | |_) | (_| | (_| |  __/\__ \
;;; |_| |_| |_|\__,_|_| |_| .__/ \__,_|\__, |\___||___/
;;;                       |_|          |___/           

(defun ee-buffer-manpage-name (&optional bufname)
  "Return the name of the manpage in the buffer BUFNAME, or nil if none.
The default for BUFNAME is the name of the current buffer.
This function does a simple string matching and converts \"*Man
foo*\" to \"foo\"."
  (if (null bufname)
      (setq bufname (buffer-name)))
  (and bufname
       (string-match "^\\*\\(Wo\\)?Man \\(.*\\)\\*$" bufname)
       (match-string 2 bufname)))
  
(defun find-last-manpage-links (manpagename &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a manpage.
Use this when you are in a manpage buffer and you want links to it."
  (interactive (list (ee-buffer-manpage-name)))
  (apply 'find-elinks
	 (list (ee-pph `(find-man-links ,manpagename))
	       ""
	       (ee-pph `(find-man ,manpagename)))
	 rest))

(defun find-manpage-links (manpagename &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a manpage.
Use this when point is over a manpage name and you want links to that page."
  (interactive (list (ee-manpagename-ask)))
  (apply 'find-elinks
	 (list (ee-pph `(find-man-links ,manpagename))
	       ""
	       (ee-pph `(find-man ,manpagename)))
	 rest))







;;;   __ _           _                _                    
;;;  / _(_)_ __   __| |      _ __ ___(_)_ __ ___           
;;; | |_| | '_ \ / _` |_____| '__/ __| | '__/ __|____      
;;; |  _| | | | | (_| |_____| | | (__| | | | (_|_____| _ _ 
;;; |_| |_|_| |_|\__,_|     |_|  \___|_|_|  \___|   (_|_|_)
;;;                                                        

;; 2012: I haven't used this in ages.

;; find-rcirc-channel-sexps - inspect the variables associated to an
;; rcirc buffer. This is another experimental, proof-of-concept
;; function; I think that we should have a function similar to this
;; one to inspect the data structures used in dired buffers, and
;; another function for doing the same in customize buffers, etc etc
;; etc...
;;
;; If we run M-x find-rcirc-channel-sexps in an rcirc buffer we get
;; something like this:
;;    _______________________________________________________________________
;;   |# (find-rcirc-channel-sexps "#rcirc@irc.freenode.net")                 |
;;   |# (progn (make-local-variable 'c) (setq c "#rcirc@irc.freenode.net"))  |
;;   |# (progn (make-local-variable 's) (setq s "*irc.freenode.net*"))       |
;;   |                                                                       |
;;   |(with-current-buffer c rcirc-target)                                   |
;;   |  -> "#rcirc"                                                          |
;;   |(with-current-buffer c (rcirc-buffer-process))                         |
;;   |  -> #<process irc.freenode.net>                                       |
;;   |(with-current-buffer c rcirc-server-buffer)                            |
;;   |  -> #<buffer *irc.freenode.net*>                                      |
;;   |(with-current-buffer c rcirc-short-buffer-name)                        |
;;   |  -> "#r"                                                              |
;;   |(with-current-buffer c rcirc-topic)                                    |
;;   |  -> "new version at: http://yeske.ca/rcirc"                           |
;;   |(with-current-buffer c (rcirc-nick (rcirc-buffer-process)))            |
;;   |  -> "edrx"                                                            |
;;   |(with-current-buffer c (rcirc-channel-nicks (rcirc-buffer-process) rci\|
;;   |rc-target))                                                            |
;;   |  -> ("mikael" "Ghoul" "jeffz" "sabetts" "rcy" "ayrnieu" "lisppaste3" \|
;;   |"luna" "dys" "entropie" "edrx")                                        |
;;   |                                                                       |
;;   |(with-current-buffer s rcirc-server)                                   |
;;   |  -> "irc.freenode.net"                                                |
;;   |(with-current-buffer s rcirc-buffer-alist)                             |
;;   |  -> (("#fvwm" . #<buffer #fvwm@irc.freenode.net>) ("#forth" . #<buffe\|
;;   |r #forth@irc.freenode.net>) ("#lua" . #<buffer #lua@irc.freenode.net>)\|
;;   | ("#rcirc" . #<buffer #rcirc@irc.freenode.net>) ("#emacs" . #<buffer #\|
;;   |emacs@irc.freenode.net>) ("#eev" . #<buffer #eev@irc.freenode.net>))   |
;;   |                                                                       |
;;   |                                                                       |
;;   |                                                                       |
;;   |                                                                       |
;;   |--:**  *Elisp hyperlinks*   All L1     (Fundamental)-----[#em]---------|
;;   |_______________________________________________________________________|
;;
(defun ee-sexp-value (sexp)
  "Example: (ee-sexp-value '(+ 1 2))"
  (list (ee-pp0 sexp)
	(format "  -> %s" (ee-pp0 (eval sexp)))))

(defun ee-rcirc-channel-sexp (sexp)
  "Internal use. Uses the value and the name of the variable `c'."
  (ee-sexp-value `(with-current-buffer c ,sexp)))

(defun ee-rcirc-server-sexp (sexp)
  "Internal use. Uses the value and the name of the variable `s'."
  (ee-sexp-value `(with-current-buffer s ,sexp)))

(defun ee-rcirc-channel-sexps (c)
  "Example: (find-elinks (ee-rcirc-channel-sexps \"#rcirc@irc.freenode.net\"))"
  `(,@(ee-rcirc-channel-sexp 'rcirc-target)
    ,@(ee-rcirc-channel-sexp '(rcirc-buffer-process))
    ,@(ee-rcirc-channel-sexp 'rcirc-server-buffer)
    ,@(ee-rcirc-channel-sexp 'rcirc-short-buffer-name)
    ,@(ee-rcirc-channel-sexp 'rcirc-topic)
    ,@(ee-rcirc-channel-sexp '(rcirc-nick (rcirc-buffer-process)))
    ,@(ee-rcirc-channel-sexp
       '(rcirc-channel-nicks (rcirc-buffer-process) rcirc-target))
    ))

(defun ee-rcirc-server-sexps (s)
  "Example: (find-elinks (ee-rcirc-server-sexps \"*irc.freenode.net*\"))"
  `(,@(ee-rcirc-server-sexp 'rcirc-server)
    ,@(ee-rcirc-server-sexp 'rcirc-buffer-alist)
    ))

;; (find-elinks (ee-rcirc-channel-sexps "#rcirc@irc.freenode.net"))
;; (find-elinks (ee-rcirc-server-sexps "*irc.freenode.net*"))
;; (find-rcirc-channel-sexps "#rcirc@irc.freenode.net")

(defun find-rcirc-channel-sexps (&optional buffer)
  "Visit a temporary buffer showing data associated to an rcirc channel buffer.
How to use: if you run\n
  (define-key eev-mode-map \"\M-hr\" 'find-rcirc-channel-sexps)\n
and then type `M-h r' when you're inside an rcirc channel buffer -
for example, when you're at the buffer for the channel #rcirc at
irc.freenode.node - then you will get a buffer with several
interesting sexps and their values.\n
This function is mostly a proof of concept at the moment.
We should write something like this to inspect buttons, customize
buffers, and dired buffers."
  (interactive (list (buffer-name (current-buffer))))
  (if (bufferp buffer) (setq buffer (buffer-name buffer)))
  (let ((server-buffer (buffer-name
			(with-current-buffer buffer
			  rcirc-server-buffer))))
    (find-elinks
     `((find-rcirc-channel-sexps ,buffer)
       (progn (make-local-variable 'c) (setq c ,buffer))
       (progn (make-local-variable 's) (setq s ,server-buffer))
       ""
       ,@(ee-rcirc-channel-sexps buffer)
       ""
       ,@(ee-rcirc-server-sexps server-buffer)
       ))))




;;;  _                     _      __     _     ___  
;;; (_)_ __  ___  ___ _ __| |_   / /___ | | __| \ \ 
;;; | | '_ \/ __|/ _ \ '__| __| | |/ _ \| |/ _` || |
;;; | | | | \__ \  __/ |  | |_  | | (_) | | (_| || |
;;; |_|_| |_|___/\___|_|   \__| | |\___/|_|\__,_|| |
;;;                              \_\            /_/ 

;; Creating temporary buffers with lots of elisp hyperlinks is an idea
;; that I only had relatively recently - in 2004, I think... before
;; that I used some functions that either inserted hyperlinks into the
;; current buffer or modified the text in the current buffer to
;; produce hyperlinks. For example, `M-x inn' inserted a link to an
;; info node, and `M-x dff' converted a line with the name of a debian
;; package into three lines, each one with a hyperlink to something
;; related to that debian package...

;; The code below is VERY wold.
;; `ee-ill' needs to be rewritten.
;; The rest will probably be deleted.

'(

(defun ee-info-file-code (infofile)
  (if (and infofile
	   ee-info-file
	   (string= (file-name-nondirectory infofile)
		    (file-name-nondirectory ee-info-file)))
      ee-info-code))

(defun ee-string-to-posspec (str)
  (if str (replace-regexp-in-string "\n" "\\\\n" (format " %S" str))
    ""))

(defun ee-info-file-link0 (usecode infofile infonode posstr)
  (let* ((code (and usecode (ee-info-file-code infofile)))
	 (infofile-nondirectory (file-name-nondirectory infofile))
	 (parenstr (if code "" (format "(%s)" infofile-nondirectory))))
    (format "(find-%snode \"%s%s\"%s)"
	    (or code "")
	    parenstr
	    infonode
	    (ee-string-to-posspec posstr))))

(defun ee-info-file-link (usecode posstr)
  (format "%s%s\n"
	  (ee-comment-prefix)
	  (ee-info-file-link0
	   usecode
	   (save-excursion (set-buffer "*info*") Info-current-file)
	   (save-excursion (set-buffer "*info*") Info-current-node)
	   posstr)))

(defun ee-inn (arg)
  (interactive "P")
  (insert (ee-info-file-link arg nil)))

(defun ee-inns (arg)
  (interactive "P")
  (insert (ee-info-file-link arg (ee-no-properties (current-kill 0)))))

(defun ee-ill (N)
  "Convert a filename at the current line into a hyperlink, and go down.
Supports `find-man', `find-udfile', and `find-fline' hyperlinks.
This function recognizes lines containing directory names, and
handles them in the following way: if the current line contains a
directory name, say, /foo/bar, and the next line contains the
name of a file or a directory in /foo/bar, say, /foo/bar/plic,
then just delete the current line."
  (interactive "p")
  (dotimes (i N)
    (beginning-of-line)
    (if (looking-at "^\\(.*\\)\n\\1/")
	(delete-region (point) (progn (forward-line 1) (point)))
      (cond ((looking-at
	      "^[^\n]*/man./\\([^\n\t /]+\\)\\.\\([0-9A-Za-z]+\\)\\.gz$")
	     (replace-match (format "%s(find-man \"%s %s\")"
				    (ee-comment-prefix)
				    (match-string 2)
				    (match-string 1)) t t))
	    ((looking-at "^/usr/share/doc/\\(.*\\)")
	     (replace-match (format "%s(find-udfile \"%s\")"
				    (ee-comment-prefix)
				    (match-string 1)) t t))
	    ((looking-at "^\\([^\n]*\\)$")
	     (replace-match (format "%s(find-fline \"%s\")"
				    (ee-comment-prefix)
				    (match-string 1)) t t)))
      (forward-line 1))))


)




;;;                      _               _   
;;;  _ __   _____      _| |__   ___  ___| |_ 
;;; | '_ \ / _ \ \ /\ / / '_ \ / _ \/ __| __|
;;; | | | |  __/\ V  V /| | | | (_) \__ \ |_ 
;;; |_| |_|\___| \_/\_/ |_| |_|\___/|___/\__|
;;;                                          
;; Scripts to connect to a newly-installed Debian machine
;; (find-angg ".emacs.templates" "find-newhost-links")
;; (find-find-links-links "newhost" "thatmname thatip thismname thisip thisiface")
;; (find-angg "bin/etc.lua" "inet_addr")

(defvar ee-this-mname nil)
(defvar ee-this-iface "wlan0")
(defvar ee-this-ip nil)
;;
(defun  ee-this-mname () (find-sh0 "uname -n"))
(defun  ee-this-ip    (&optional iface)
  (find-sh0 (format "/sbin/ifconfig %s | etc.lua inet_addr"
		    (or ee-this-iface iface))))
;;
;;   (find-sh0 "uname -n")
;;   (find-sh0 "/sbin/ifconfig")
;;   (find-sh0 "/sbin/ifconfig wlan0")
;;   (find-sh0 "/sbin/ifconfig eth0")
;;   (setq ee-this-iface "wlan0")
;;   (setq ee-this-iface "eth0")
;;   (ee-this-mname)
;;   (ee-this-ip)
;; (setq ee-this-mname (ee-this-mname))
;; (setq ee-this-ip (ee-this-ip))

(defun find-newhost-links (&optional thatmname thatip thismname thisip thisiface &rest rest)
  "Visit a temporary buffer with an e-script to set up a new host."
  (interactive)
  (let ((thisdisplay))
    (setq thatmname (or thatmname "{thatmname}"))
    (setq thatip    (or thatip    "{thatip}"))
    (setq thismname (or thismname ee-this-mname "{thismname}"))
    (setq thisip    (or thisip    ee-this-ip    "{thisip}"))
    (setq thisiface (or thisiface ee-this-iface "{thisiface}"))
    (setq thisdisplay (or (getenv "DISPLAY") "{thisdisplay}"))
    (apply 'find-elinks `(
      ;; Convention: the first sexp always regenerates the buffer.
      (find-newhost-links ,thatmname ,thatip ,thismname ,thisip ,thisiface ,@rest)
      (find-newhost-links ,thatmname ,thatip ,ee-this-mname ,ee-this-ip ,ee-this-iface ,@rest)
      (find-newhost-links ,thatmname ,thatip nil nil nil ,@rest)
      (find-newhost-links ,thatmname ,thatip)
      (find-efunction 'find-newhost-links)
      ;; Body:
      ""
      ,(ee-template `(thatmname thatip
		      thismname thisip thisiface
		      thisdisplay) "\
# Basic setup (on this machine, {thismname} - set thismname and thisip):
#   (find-sh0 \"/sbin/ifconfig wlan0\")
#   (find-sh0 \"/sbin/ifconfig eth0\")
#   (setq ee-this-iface \"wlan0\")
#   (setq ee-this-iface \"eth0\")
#   (ee-this-mname)
#   (ee-this-ip)
# (setq ee-this-mname (ee-this-mname))
# (setq ee-this-ip    (ee-this-ip))
# (find-newhost-links \"{thatmname}\" \"{thatip}\")

# Make sure that we can refer to {thatmname} by name
#   (find-sh0 \"ls -l          /etc/hosts\")
#   (find-sh0 \"sudo chmod 666 /etc/hosts\")
#   (kill-new \"{thatip}   {thatmname}\")
#   (find-fline \"/etc/hosts\")
#   (find-fline \"/etc/hosts\" \"{thatmname}\")
#   (find-fline \"/etc/hosts\" \"{thatip}\")

# Basic setup (on the remote machine, {thatmname} - by hand):
#   adduser edrx
#   (find-es \"sudo\" \"sudo\")
#   apt-get install openssh-server xterm
#   chmod 666 /etc/hosts
#   nano      /etc/hosts
#   (kill-new \"{thisip}   {thismname}\")

# Try to connect:
#   (find-sh0 \"ssh-keygen -R {thatip}\")
#   (find-sh0 \"ssh-keygen -R {thatmname}\")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
ssh edrx@{thatip}
exit
ssh edrx@{thatmname}
exit

# Try file access by tramp
#   (find-fline \"/ssh:edrx@{thatip}:/\")
#   (find-fline \"/scp:edrx@{thatip}:/\")
#   (find-fline \"/ssh:edrx@{thatmname}:/\")
#   (find-fline \"/scp:edrx@{thatmname}:/\")

;; If that works:
\(code-c-d \"{thatmname}\" \"/scp:edrx@{thatmname}:\")
\(code-c-d \"{thatmname}\" \"/ssh:edrx@{thatmname}:\")
;; (find-{thatmname}file \"/\")
;; (find-{thatmname}file \"/home/edrx/\")
\(defun eepitch-{thatmname} () (interactive)
  (eepitch '(find-comintprocess \"ssh {thatmname}\" \"ssh edrx@{thamname}\")))


# Make sure that the remote machine knows this hostname
ssh edrx@{thatmname}
  #
  # Does the remote machine know this hostname?
  echo \"{thisip}   {thismname}\"
  cat            /etc/hosts | grep {thismname}
  #
  # If it isn't there, add it:
  cat            /etc/hosts
  ls -l          /etc/hosts
  sudo chmod 666 /etc/hosts
  echo \"{thisip}   {thismname}\" >> /etc/hosts
  cat            /etc/hosts | grep {thismname}
  cat            /etc/hosts
  #
  # Try to open a remote xterm on this X server
  # (find-sh0 \"xhost +\")
  DISPLAY={thismname}{thisdisplay} xterm -T \"xterm@{thatmname}\" &
  exit


# A high-level version of the above:
# (find-sh0 \"xhost +\")
# (find-{thatmname}file \"/etc/hosts\" \"{thismname}\")
#   (eekill \"{thisip}   {thismname}\\n\")
#   (eekill \"# (setq backup-inhibited t)\\n\")

 (eepitch-{thatmname})
 (eepitch-kill)
 (eepitch-{thatmname})
DISPLAY={thismname}{thisdisplay} xterm -T \"xterm@{thatmname}\" &

")
    ) rest)))

;; (find-newhost-links)
;; (find-newhost-links "gwen" "192.168.1.101")



;;;      _ _                     _        
;;;   __| | |__  _ __ ___   __ _| | _____ 
;;;  / _` | '_ \| '_ ` _ \ / _` | |/ / _ \
;;; | (_| | | | | | | | | | (_| |   <  __/
;;;  \__,_|_| |_|_| |_| |_|\__,_|_|\_\___|
;;;                                       
;; Some templates for generating ".deb"s.
;; If you want to build a .deb for a package called, say, "foo-bar",
;; these scripts will use the directory "~/usrc/foo-bar/foo-bar_xxx/"
;; to build it - that directory will be recreated from scratch each
;; time - and the "override files for dhmake" will be taken from the
;; directory "~/foo-bar/". At this moment these paths are hardcoded.
;;
;; I generate the 3 main packages at
;;   http://angg.twu.net/debian/
;;   http://angg.twu.net/debian/README.html
;; with these commands:
;;   (find-dhmake-links "eev" "eevbuild")
;;   (find-dhmake-links "eev-puro" "eevpuro")
;;   (find-dhmake-links "eev-lua-extras" "eevluaextras")

(defvar ee-dhmake-fullname "Eduardo Ochs")
(defvar ee-dhmake-email "eduardoochs@gmail.com")

;; (find-estring-elisp (ee-dhmake-codecds "eev-puro" "eevpuro" "20120305"))
;;
(defun ee-dhmake-codecds (stem c date)
  (ee-template '(stem c date) "\
;; Generated by:
;; (ee-dhmake-codecds \"{stem}\" \"{c}\" \"{date}\")
;;
\(code-c-d \"{c}dh\"  \"~/{stem}/\")
\(code-c-d \"{c}dhd\" \"~/{stem}/debian/\")
\(code-c-d \"{c}\"    \"~/usrc/{stem}/{stem}-0.1.{date}/\")
\(code-c-d \"{c}d\"   \"~/usrc/{stem}/{stem}-0.1.{date}/debian/\")
\(code-c-d \"{c}dd\"  \"~/usrc/{stem}/{stem}-0.1.{date}/debian/{stem}/\")
\(code-c-d \"{c}unp\" \"~/usrc/{stem}/{stem}-unpacked/\")
;; (find-{c}dhfile  \"\")
;; (find-{c}dhdfile \"\")
;; (find-{c}file    \"\")
;; (find-{c}dfile   \"\")
;; (find-{c}ddfile  \"\")
;; (find-{c}unpfile \"\")
;; (find-{c}dsh   \"find * | sort\")
;; (find-{c}ddsh  \"find * | sort\")
;; (find-{c}unpsh \"find * | sort\")
;; (find-status   \"{stem}-deb\")
;; (find-vldifile \"{stem}-deb.list\")
;; (find-udfile   \"{stem}-deb-puro/\")"))

;; (find-estring (ee-dhmake-build "eev-puro" "eevpuro" "20120305"))
;;
(defun ee-dhmake-build (stem c date)
  (ee-template '(stem c date ee-dhmake-fullname ee-dhmake-email) "\
# Generated by:
# (ee-dhmake-build \"{stem}\" \"{c}\" \"{date}\")
#
rm   -Rv ~/usrc/{stem}/
mkdir -p ~/usrc/{stem}/
mkdir    ~/usrc/{stem}/{stem}-0.1.{date}/
# ln -s                {stem}-0.1.{date}  ~/usrc/{stem}/{stem}-0.1
  ln -s                {stem}-0.1.{date}  {stem}-0.1
cd       ~/usrc/{stem}/{stem}-0.1.{date}/

echo | \\
DEBFULLNAME=\"{ee-dhmake-fullname}\" \\
  dh_make --email {ee-dhmake-email} \\
          --copyright=gpl \\
          --cdbs \\
          --native
rm -v debian/README.Debian
rm -v debian/*.EX
rm -v debian/*.ex
cp -iv debian/control debian/control.orig

# (find-fline      \"~/{stem}/debian/\")
# (find-sh0 \"rm -Rv ~/{stem}/debian/\")
mkdir  -p ~/{stem}/debian/
if [ ! -e ~/{stem}/Makefile ]; then
  echo Creating:               ~/{stem}/Makefile
  echo \"clean:\"              > ~/{stem}/Makefile
fi
if [ ! -e ~/{stem}/debian/control ]; then
  echo Creating:               ~/{stem}/debian/control
  cp -v debian/control         ~/{stem}/debian/control
fi
if [ ! -e ~/{stem}/debian/rules ]; then
  echo Creating:               ~/{stem}/debian/rules
  cp -v debian/rules           ~/{stem}/debian/rules
  echo \"build/{stem}::\"   >> ~/{stem}/debian/rules
  echo \"install/{stem}::\" >> ~/{stem}/debian/rules
fi

# (find-{c}dh \"debian/\")
# (find-{c}dh \"debian/control\")
# (find-{c}dh \"debian/rules\")
cp -v ~/{stem}/debian/* debian/
cp -v ~/{stem}/Makefile .

# (find-man \"1 dpkg-buildpackage\")
# dpkg-buildpackage -us -uc    -rfakeroot     2>&1 | tee odb
  dpkg-buildpackage -us -uc -b -rfakeroot     2>&1 | tee odb
# (find-{c}file \"odb\")

rm -Rv ~/usrc/{stem}/{stem}-unpacked/
mkdir  ~/usrc/{stem}/{stem}-unpacked/
mkdir  ~/usrc/{stem}/{stem}-unpacked/DEBIAN/
cd     ~/usrc/{stem}/
ar p {stem}_*.deb control.tar.gz | tar -C {stem}-unpacked/DEBIAN/ -xvzf -
ar p {stem}_*.deb data.tar.gz    | tar -C {stem}-unpacked/        -xvzf -

# (find-{c}unpfile \"\")
# (find-{c}unpfile \"DEBIAN/\")
# (find-{c}unpfile \"DEBIAN/control\")
# (find-{c}unpsh \"find * | sort\")"))

(defun find-dhmake-links (&optional stem c date &rest rest)
  "Visit a temporary buffer containing hyperlinks for dhmake."
  (interactive)
  (setq stem (or stem "{stem}"))
  (setq c    (or c    "{c}"))
  (setq date (or date (format-time-string "%Y%m%d")))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-dhmake-links ,stem ,c ,date ,@rest)
    (find-efunction 'find-dhmake-links)
    ;; Body:
    ""
    ;; (find-estring-elisp (ee-dhmake-codecds ,stem ,c ,date))
    ;; (find-estring (ee-dhmake-build ,stem ,c ,date))
    ;; (eev (ee-dhmake-build ,stem ,c ,date))
    ;; (find-fline "$EE")
    ;; (find-fline (format "~/%s/debian/" stem))
    ;; (find-sh0 ,(format "rm -Rv ~/%s/debian/" stem))
    ;; ""
    ,(ee-template `(stem c date) "\
# Set up the build script that `ee' will execute:
# (find-estring-elisp (ee-dhmake-codecds \"{stem}\" \"{c}\" \"{date}\"))
# (find-estring       (ee-dhmake-build   \"{stem}\" \"{c}\" \"{date}\"))
# (eev                (ee-dhmake-build   \"{stem}\" \"{c}\" \"{date}\"))
# (find-fline \"$EE\")

# Examine its main control files:
# (find-fline \"~/{stem}/debian/rules\")
# (find-fline \"~/{stem}/debian/control\")
# (find-fline \"~/{stem}/debian/\")

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
ee
cd ~/usrc/{stem}/
ls -lF *.deb
sudo dpkg -i {stem}*.deb
# sudo dselect update

# (find-status   \"{stem}\")
# (find-vldifile \"{stem}.list\")
# (find-udfile   \"{stem}/\")


# Upload to angg.twu.net.
# (THIS IS A HACK!)
# (find-twusfile \"debian/\")

 (eepitch-eshell)
cd ~/usrc/{stem}/
ls -l {stem}_0.1.{date}_all.deb
cp -v {stem}_0.1.{date}_all.deb (ee-twusfile \"debian/\")
ls -l (ee-twusfile \"debian/\")
#   (find-twusfile \"debian/\")

 (eepitch-Twu)
cd ~/slow_html/debian/
mkdir -p dists/./main/binary-i386/
mkdir -p dists/./main/binary-amd64/
ls -lAF
apt-ftparchive packages .
apt-ftparchive packages . \\
  | tee        dists/./main/binary-i386/Packages \\
  | gzip -c9 > dists/./main/binary-i386/Packages.gz
apt-ftparchive packages . \\
  | tee        dists/./main/binary-amd64/Packages \\
  | gzip -c9 > dists/./main/binary-amd64/Packages.gz
")
    ) rest))
 
;; (find-dhmake-links)
;; (find-dhmake-links "{stem}" "{c}" "{date}")
;; (find-dhmake-links "foo-bar" "foobar")
;; (find-dhmake-links "eev" "eevbuild")
;; (find-dhmake-links "eev" "eevbuild" "20120404")





;;;                    _         _                    _ _ 
;;;  _   _  ___  _   _| |_ _   _| |__   ___        __| | |
;;; | | | |/ _ \| | | | __| | | | '_ \ / _ \_____ / _` | |
;;; | |_| | (_) | |_| | |_| |_| | |_) |  __/_____| (_| | |
;;;  \__, |\___/ \__,_|\__|\__,_|_.__/ \___|      \__,_|_|
;;;  |___/                                                
;;
;; (find-angg ".emacs.templates" "find-youtubedl-links")

(defvar ee-youtubedl-dir "/sda5/videos/")

(defun find-youtubedl-links (&optional dir title hash ext stem &rest rest)
  "Visit a temporary buffer containing hyperlinks for youtube-dl."
  (interactive)
  (setq dir   (or dir "{dir}"))
  (setq title (or title "{title}"))
  (setq hash  (or hash "{hash}"))
  (setq ext   (or ext "{ext}"))
  (setq stem  (or stem "{stem}"))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-youtubedl-links ,dir ,title ,hash ,ext ,stem ,@rest)
    (find-youtubedl-links ee-youtubedl-dir ,title ,hash ,ext ,stem ,@rest)
    (setq ee-youtubedl-dir ,ee-youtubedl-dir)
    ;; Body:
    ""
    ,(ee-template `(dir title hash ext stem) "\
 (eepitch-shell2)
 (eepitch-kill)
 (eepitch-shell2)
# http://www.youtube.com/watch?v={hash}
cd {dir}
youtube-dl -t 'http://www.youtube.com/watch?v={hash}'

# (find-es \"video\" \"youtube-dl\")
# (find-fline \"{dir}\" \"{hash}\")
# (find-fline \"{dir}\" \"{title}-{hash}\")
# (find-fline \"{dir}\" \"{title}-{hash}{ext}\")
# (find-mplayer  \"{dir}{title}-{hash}{ext}\")
# (find-mplayer  \"{dir}{title}-{hash}{ext}.part\")
# (code-mplayer \"{stem}video\" \"{dir}{title}-{hash}{ext}\")
# (code-mplayer \"{stem}video\" \"{dir}{title}-{hash}{ext}.part\")
# (find-{stem}video)
")
    ) rest))



;; TODO: move these functions to another file (eev-video.el?)
;; (find-angg ".emacs" "mm:ss")
;; (find-angg ".emacs" "find-mplayer")
;; (find-angg ".emacs" "code-mplayer")
;;
(defun secs-to-mm:ss (n)
  "Force N - a number of seconds or an \"mm:ss\" string - to the mm:ss format"
  (if (stringp n) n
    (let* ((s (mod n 60))
	   (m (/ (- n s) 60)))
      (format "%d:%02d" m s))))
(defun mm:ss-to-secs (mm:ss)
  "Force MM:SS - a string or a number of seconds - to a number of seconds"
  (if (numberp mm:ss) mm:ss
    (let* ((ms (mapcar 'string-to-number (split-string mm:ss ":"))))
      (+ (* 60 (car ms)) (cadr ms)))))

(setq ee-mplayer-options '("-fs" "-osdlevel" "2"))
(defun find-mplayer (fname &optional pos &rest rest)
  (interactive "sFile name: ")
  (find-bgprocess
   `("mplayer" ,fname
     ,@(if pos `("-ss" ,(secs-to-mm:ss pos)))
     ,@ee-mplayer-options
     ,@rest)))

(defun ee-code-mplayer (code fname)
  (format "
    (defun find-%s (&rest rest) (interactive)
      (apply 'find-mplayer %S rest))
  " code fname))
(defun code-mplayer      (c f) (eval (ee-read (ee-code-mplayer c f))))
(defun find-code-mplayer (c f) (find-estring-elisp (ee-code-mplayer c f)))






;;;  _   _       _                 _                   _ 
;;; | | | |_ __ | | ___   __ _  __| |   __ _ _ __   __| |
;;; | | | | '_ \| |/ _ \ / _` |/ _` |  / _` | '_ \ / _` |
;;; | |_| | |_) | | (_) | (_| | (_| | | (_| | | | | (_| |
;;;  \___/| .__/|_|\___/ \__,_|\__,_|  \__,_|_| |_|\__,_|
;;;       |_|                                            
;;;      _                     _                 _ 
;;;   __| | _____      ___ __ | | ___   __ _  __| |
;;;  / _` |/ _ \ \ /\ / / '_ \| |/ _ \ / _` |/ _` |
;;; | (_| | (_) \ V  V /| | | | | (_) | (_| | (_| |
;;;  \__,_|\___/ \_/\_/ |_| |_|_|\___/ \__,_|\__,_|
;;;                                                

;; 2012jan26 - experimental hack.
;; Some of these functions use environment variables for readability
;; (i.e., to make the shell scripts more readable).
;; Default values (all for angg.twu.net):
(ee-setenv "MYPAGEDIR" "/scp:edrx@angg.twu.net:public_html")
(ee-setenv "MYPAGEURL" "http://angg.twu.net")
(ee-setenv "DNPAGEURL" "http://angg.twu.net")
;; (getenv "MYPAGEDIR")
;; (getenv "MYPAGEURL")
;; (getenv "DNPAGEURL")
;; (setenv "DNPAGEURL" "http://0branch.com/highlight/snippets")

(defun find-tkdiff (f1 f2)
  (find-bgprocess `("tkdiff" ,f1 ,f2)))

(defun ee-upload-links (fromdir/ todir/ fname)
  "An internal function used by `find-upload-links'.
Try this: (find-elinks (ee-upload-links \"eev-current/eev-template.el\"))"
  (let ((dir    (file-name-directory (ee-expand fname)))
	(fname- (file-name-nondirectory fname))
	(mypagedir (getenv "MYPAGEDIR"))
	(mypageurl (getenv "MYPAGEURL"))
	)
    `("# Env vars (current values):"
      (setenv "MYPAGEDIR" ,(getenv "MYPAGEDIR"))
      (setenv "MYPAGEURL" ,(getenv "MYPAGEURL"))
      ,(ee-template1 `(fromdir/ todir/ fname dir fname- mypagedir mypageurl) "\
# Upload (warning: SLOW, uses tramp!):
 (eepitch-eshell)
cp -v ~/{fromdir/}{fname} $MYPAGEDIR/{todir/}{fname}
ls -l ~/{fromdir/}{fname} $MYPAGEDIR/{todir/}{fname}\n
# Test:
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd /tmp/
wget -N {mypageurl}/{fname}
ls -l /tmp/{fname-} ~/{fname}
# (find-fline \"/tmp/{fname-}\")"))))

(defun ee-download-links (fromdir/ todir/ fname)
  "Visit a temporary buffer containing a script for downloading FNAME."
  (setq fromdir/ (or fromdir/ (file-name-directory fname)))
  (setq todir/   (or todir/   (file-name-directory fname)))
  (let ((fname-               (file-name-nondirectory fname))
	(dnpageurl            (getenv "DNPAGEURL")))
    `(,(ee-template `(fromdir/ todir/ fname fname- dnpageurl) "\
# Download:
 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd /tmp/
wget -N {dnpageurl}/{fromdir/}{fname}
ls -l /tmp/{fname-} ~/{todir/}{fname}
cp -v /tmp/{fname-} ~/{todir/}{fname}
#        (diff \"/tmp/{fname-}\" \"~/{todir/}{fname}\")
# (find-tkdiff \"/tmp/{fname-}\" \"~/{todir/}{fname}\")
# (find-fline \"/tmp/{fname-}\")
# (find-fline \"~/{todir/}{fname}\")"))))

(defun find-upload-links (&optional fromdir/ todir/ fname &rest rest)
  "Visit a temporary buffer containing a script for uploading FNAME."
  (interactive)
  (setq fname (or fname "{fname}"))
  (setq fromdir/ (or fromdir/ (file-name-directory fname)))
  (setq todir/   (or todir/   (file-name-directory fname)))
  (let ((ee-hyperlink-prefix "# "))
    (apply 'find-elinks `(
      ;; Convention: the first sexp always regenerates the buffer.
      (find-upload-links ,fromdir/ ,todir/ ,fname ,@rest)
      ;; The second sexp generates the corresponding download link.
      (find-download-links ,fname ,@rest)
      ""
      ,@(ee-upload-links fromdir/ todir/ fname)) rest)))

(defun find-download-links (&optional fromdir/ todir/ fname extras &rest rest)
  "Visit a temporary buffer containing a script for downloading FNAME."
  (interactive)
  (setq fname    (or fname "{fname}"))
  (setq fromdir/ (or fromdir/ (file-name-directory fname)))
  (setq todir/   (or todir/   (file-name-directory fname)))
  (apply 'find-elinks `(
    ;; Convention: the first sexp always regenerates the buffer.
    (find-download-links ,fromdir/ ,todir/ ,fname ,@rest)
    ;; Body:
    ""
    ,@(ee-download-links fromdir/ todir/ fname)
    ,@extras) rest))

(defun ut ()
  (interactive)
  "Upload eev-template.el"
  (find-upload-links "eev-current/" "eev-current/" "eev-template.el"))

(defun dt ()
  (interactive)
  "Download eev-template.el and load the new version."
  (find-download-links
   "eev-current/" "emacs/eev/" "eev-template.el"
   '("\n (load \"eev-template.el\")")))



;; Tests:
;; (find-upload-links "eev-current/eev-template.el")
;; (find-download-links "" "" "eev-current/eev-template.el")
;; (eevt-down "eev-current/" "emacs/eev/" "eev-template.el")
;
; (find-efunctiondescr 'ee-upload-links)






(provide 'eev-template)





;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";; "
;; no-byte-compile:   t
;; End:
