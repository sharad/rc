;;; eev-mini.el -- minimal versions for the main functions of eev.

;; Copyright (C) 2006,2007,2010 Free Software Foundation, Inc.
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
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Author:     Eduardo Ochs <eduardoochs@gmail.com>
;; Maintainer: Eduardo Ochs <eduardoochs@gmail.com>
;; Version:    2010sep10
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-mini.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-mini.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;;; Commentary:

;; This file is not really intended to be loaded... it contains
;; simplistic implementations of some of the main functions of eev.
;;
;; If you understand all the functions here and how to use them in
;; your e-scripts then you would have grasped most of the main ideas
;; of eev.
;;
;; This is not ready yet.



;;;          __  __                         
;;;         |  \/  | __ _ _ __  ___         
;;;  _____  | |\/| |/ _` | '_ \/ __|  _____ 
;;; |_____| | |  | | (_| | |_) \__ \ |_____|
;;;         |_|  |_|\__,_| .__/|___/        
;;;                      |_|                

;; One thing that makes eev's code a bit confusing is that even when
;; some words are "top-level words", in the sense that they are meant
;; to be called by the user (for example: I often type `M-x eev'
;; several hundred times a day, when I'm doing something that involves
;; sending lots of commands to shells), there also other words that
;; are "on a higher level than those", in the sense that those new
;; words call the previous words... for example, `eev-bounded' is also
;; meant to be invoked by the user - either by typing `M-x
;; eev-bounded' inside a delimited block, or by executing a sexp like
;; "(eev-bounded)" placed inside such a block - and `eev-bounded'
;; invokes `eev'.
;;
;; This passage from a function that operates on the region, like
;; `eev', to another one, like `eev-bounded', that works on delimited
;; regions, is something that we also want to have for other
;; functions... for example, it's also useful to have a "bounded
;; version" of `eelatex' - and we have that, it's called
;; `eelatex-bounded'.
;;
;; We don't build these bounded versions by hand - instead we have a
;; function, `eeb-define', that builds them for us. It generates some
;; Lisp code - with `ee-eeb-define' - and then evaluates it. If we
;; want to inspect that code we run `find-eeb-define' instead of
;; `eeb-define', with the same parameters; it generates the same code
;; but displays it in a temporary buffer instead of evaluating it.
;;
;; A diagram (for the `eev' family only):
;;
;;                              eeb-define   find-eeb-define            
;;                                    |    /       |                               
;;                                    v   v        v                         
;;                           ee-eeb-define   find-estring-elisp
;;                                   |                                  
;;                                   v                                  
;;                             eev-bounded                              
;;                              |       |                               
;;                              |       v                               
;;                              |   eeb-default                         
;;                              |    /     \                            
;;                              v   v       v                   
;;    eev <-------------- eeb-defaults    eeflash                       
;;    |  \                      :                       
;;    |   v                     v                 
;;    | ee-se-to-string   eev-set-glyph             
;;    |                                          
;;    v                                         
;;    ee-write-string                              
;;          |
;;          v
;;    ee-se-to-string
;; 
;; Top-level words:
;;   eev         - save the region to a temporary script file
;;   eev-bounded - save a bounded region into the temporary script file
;;   eeb-define  - used to define `eev-bounded' from `eev'
;;   find-eeb-define - display the code that eeb-define runs
;; 
;; The relations in the graph above:
;;   `eev' calls `ee-write-string', that calls `ee-se-to-string'.
;;   `eev' uses `ee-se-to-string' to get what to save.
;;   `eev-bounded' sets `eeb-defauls' and calls `eeb-default'.
;;   `eeb-default' read the delimiters and the function from `eeb-defaults'.
;;   The delimiters in `eeb-defaults' usually contain `', whose
;;   appearance is defined by a call to `eev-set-glyph'.
;;   `eeb-default' calls `eeflash' to highlight the region.
;;   `eeb-define' calls `ee-eeb-define' to generate the code of a defun -
;;   for example, for the definition of `eev-bounded'. This code is then
;;   evaluated.
;;   `find-eeb-define' also calls `ee-eeb-define', but instead of evaluating
;;   the code it uses `find-estring-elisp' to display the code in a buffer.

;; A diagram for the `find-fline'/`find-node' family:
;;
;;                                    code-c-d   find-code-c-d
;;                                       |     /       |
;;                                       v    v        v
;;               find-fline <----- ee-code-c-d   find-estring-elisp
;;             /  |              /                     |
;;            v   |             v                      v
;;   ee-expand    |    find-node                 find-eoutput-rerun
;;                |        |
;;                v        v
;;               ee-goto-position
;;                     |
;;                     v
;;               ee-goto-rest
;;
;; Top-level words:
;;   find-fline    - hyperlink to a file
;;   find-node     - hyperlink to an info node
;;   code-c-d      - to define `find-xxxfile' and `find-xxxnode' words
;;   find-code-c-d - inspect the code that `code-c-d' would run



;; (find-eevfile "eev.el")



;;;          _   _                       _ _       _                
;;;         | | | |_   _ _ __   ___ _ __| (_)_ __ | | _____         
;;;  _____  | |_| | | | | '_ \ / _ \ '__| | | '_ \| |/ / __|  _____ 
;;; |_____| |  _  | |_| | |_) |  __/ |  | | | | | |   <\__ \ |_____|
;;;         |_| |_|\__, | .__/ \___|_|  |_|_|_| |_|_|\_\___/        
;;;                |___/|_|                                         


;;;  _               _        _ _       _        
;;; | |__   __ _ ___(_) ___  | (_)_ __ | | _____ 
;;; | '_ \ / _` / __| |/ __| | | | '_ \| |/ / __|
;;; | |_) | (_| \__ \ | (__  | | | | | |   <\__ \
;;; |_.__/ \__,_|___/_|\___| |_|_|_| |_|_|\_\___/
;;;                                              
;;; Basic links: find-fline and find-node

(defun find-fline (fname &rest pos-spec-list)
  "Hyperlink to a file (or a directory).
This function is similar to `find-file' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (find-file  \"~/.emacs\")
  (find-fline \"~/.emacs\")
  (find-fline \"~/.emacs\" \"Beginning of the eev block\")"
  (find-file (ee-expand fname))
  (apply 'ee-goto-position pos-spec-list))

(defun find-node (nodestr &rest pos-spec-list)
  "Hyperlink to an info page.
This function is similar to `info' but it supports a
\"pos-spec-list\" - see `ee-goto-position'.
Examples:\n
  (info \"(emacs)Lisp Eval\")
  (find-node \"(emacs)Lisp Eval\" \"C-x C-e\")"
  (Info-goto-node nodestr)
  (apply 'ee-goto-position pos-spec-list))

(defun ee-goto-position (&optional pos-spec &rest rest)
  "Process the \"absolute pos-spec-lists\" arguments in hyperlink functions."
  (when pos-spec
    (cond ((numberp pos-spec)		; pos-spec is a number? (say, 42)
	   (goto-char (point-min))	; jump to the line 42
	   (forward-line (1- pos-spec)))
	  ((stringp pos-spec)		; pos-spec is a string? (say, "foo")
	   (goto-char (point-min))	; jump to the first occurrence of "foo"
	   (search-forward pos-spec))
	  (t (error "This is not a valid pos-spec: %S" pos-spec)))
    (if rest (ee-goto-rest rest))))

(defun ee-goto-rest (list)
  "Process \"relative pos-spec-lists\"."
  (cond ((null list))
	((stringp (car list))		; a string?
	 (search-forward (car list))	; jump to its next occurrence
	 (ee-goto-rest (cdr list)))
	((numberp (car list))		; a number?		 
	 (forward-line (car list))	; advance that many lines
	 (ee-goto-rest (cdr list)))
	((consp (car list))		; a cons?
	 (eval (car list))		; eval it
	 (ee-goto-rest (cdr list)))
	(t (error "Not a valid pos-spec item: %S" (car list)))))

(defun ee-expand (fname)
"Expand \"~\"s and \"$ENVVAR\"s in file names, but only at the beginning of the string."
  (cond ((string-match "^\\$\\([A-Za-z_][0-9A-Za-z_]*\\)\\(.*\\)" fname)
	 (concat (getenv (match-string 1 fname))
		 (match-string 2 fname)))
	((string-match "^\\(~\\([a-z][0-9a-z_]*\\)?\\)\\(/.*\\)?$" fname)
	 (concat (expand-file-name (match-string 1 fname))
		 (match-string 3 fname)))
	(t fname)))







;;;                _                          _ 
;;;   ___ ___   __| | ___        ___       __| |
;;;  / __/ _ \ / _` |/ _ \_____ / __|____ / _` |
;;; | (_| (_) | (_| |  __/_____| (_|_____| (_| |
;;;  \___\___/ \__,_|\___|      \___|     \__,_|
;;;                                             

;; Define lots of hyperlink functions at once. See:
;; http://angg.twu.net/eev-article.html#shorter-hyperlinks

(defun code-c-d (c d &rest rest)
 (eval (read (ee-concat (apply 'ee-code-c-d c d rest)))))

(defun find-code-c-d (c d &rest rest)
  "Use this to inspect the code that a `code-c-d' would run."
 (find-estring-elisp (ee-concat (apply 'ee-code-c-d c d rest))))

(defun ee-concat (list &optional sep) (mapconcat 'identity list (or sep "")))

(defun ee-code-c-d (c d &rest rest)
 (cons (format "
   (setq  ee-%sdir %S)
   (defun ee-%sfile (str)
     (concat (ee-expand ee-%sdir) str))
   (defun find-%sfile (str &rest pos-spec-list)
     (interactive (list \"\"))
     (apply 'find-fline (ee-%sfile str) pos-spec-list))
   " c d   c c  c c) (ee-code-c-d-rest rest)))


;; Recurse over the "rest" of the arguments using `ee-code-c-d-rest'.
;; Note that the ee-code-c-d-:keyword functions can access the
;; values of c and d from the parameter list of `ee-code-c-d'.

(defun ee-code-c-d-rest (rest)
 (if rest
     (let ((fun (cdr (assq (car rest) code-c-d-keywords))))
       (if (not fun)
	   (error "In (code-c-d %S ...): not a keyword: %S"
		  c (car rest)))
       (apply fun (cdr rest)))))

(defvar code-c-d-keywords nil)
(setq   code-c-d-keywords
 '((:info   . ee-code-c-d-:info)
   (:anchor . ee-code-c-d-:anchor)))

(defun ee-code-c-d-:info (info &rest rest)
 (cons (format "
   (defun find-%snode (page &rest pos-spec-list)
     (interactive (list \"\"))
     (apply 'find-node (format \"(%s)%%s\" page) pos-spec-list))
   " c info) (ee-code-c-d-rest rest)))

(defun ee-code-c-d-:anchor (&rest rest)
 (cons (format "
   (defun find-%s (str &rest pos-spec-list)
     (apply 'find-anchor (ee-%sfile str) pos-spec-list))
   " c c) (ee-code-c-d-rest rest)))

;; We can define new keywords for code-c-d by adding a pair entries to 
;; code-c-d-keywords and An example of extension...

(add-to-alist 'code-c-d-keywords '(:linfo  . ee-code-c-d-:linfo))

(defun ee-code-c-d-:linfo (linfo &rest rest)
 (cons (format "
   (defun find-%snode (page &rest pos-spec-list)
     (interactive (list \"\"))
     (apply 'find-node (format \"(%%s)%%s\" page)
            (ee-expand (ee-%sfile %S))
            pos-spec-list))
   " c c linfo) (ee-code-c-d-rest rest)))









;;;   __ _           _                        _               _   
;;;  / _(_)_ __   __| |       ___  ___  _   _| |_ _ __  _   _| |_ 
;;; | |_| | '_ \ / _` |_____ / _ \/ _ \| | | | __| '_ \| | | | __|
;;; |  _| | | | | (_| |_____|  __/ (_) | |_| | |_| |_) | |_| | |_ 
;;; |_| |_|_| |_|\__,_|      \___|\___/ \__,_|\__| .__/ \__,_|\__|
;;;                                              |_|              

(defun find-eoutput-rerun (buffer-name code &rest pos-spec-list)
  (if (get-buffer buffer-name)		  ; if the buffer exists
      (if (not (kill-buffer buffer-name)) ; try to kill it; confirm if needed
	  (error "Not killing the buffer %s" buffer-name)))
  (switch-to-buffer buffer-name)	  ; create the buffer
  (eval code)				  ; always run CODE on the empty buffer
  (goto-char (point-min))
  (apply 'ee-goto-position pos-spec-list))

(defun find-eoutput-reuse (buffer-name code &rest pos-spec-list)
  (if (get-buffer buffer-name)		; if the buffer exists
      (switch-to-buffer buffer-name)	; then just switch to it
    (switch-to-buffer buffer-name)	; otherwise switch to it and
    (eval code)				; run CODE to produce its contents
    (goto-char (point-min)))
  (apply 'ee-goto-position pos-spec-list))

(defun find-estring (string &rest pos-spec-list)
  (apply 'find-eoutput-rerun "*string*" `(insert ,string) pos-spec-list))

(defun find-estring-elisp (string &rest pos-spec-list)
  (apply 'find-eoutput-rerun "*string*"
	 `(progn (insert ,string) (emacs-lisp-mode)) pos-spec-list))




;;;   __ _           _           _        __                     
;;;  / _(_)_ __   __| |      ___| |__    / / __ ___   __ _ _ __  
;;; | |_| | '_ \ / _` |_____/ __| '_ \  / / '_ ` _ \ / _` | '_ \ 
;;; |  _| | | | | (_| |_____\__ \ | | |/ /| | | | | | (_| | | | |
;;; |_| |_|_| |_|\__,_|     |___/_| |_/_/ |_| |_| |_|\__,_|_| |_|
;;;                                                              

(defun find-sh (command &rest pos-spec-list)
  (apply 'find-eoutput-reuse
	 (ee-no-trailing-nl command)
	 `(insert (shell-command-to-string ,command))
	 pos-spec-list))

(defun find-sh00 (command) (shell-command-to-string command))
(defun find-sh0  (command) (ee-no-trailing-nl (find-sh00 command)))

(defun find-man (manpage &rest pos-spec-list)
  (apply 'find-sh (format "PAGER=cat man %s | col -bx" manpage)
	 pos-spec-list))




;;;                                              
;;;  _ __  _ __ ___   ___ ___  ___ ___  ___  ___ 
;;; | '_ \| '__/ _ \ / __/ _ \/ __/ __|/ _ \/ __|
;;; | |_) | | | (_) | (_|  __/\__ \__ \  __/\__ \
;;; | .__/|_|  \___/ \___\___||___/___/\___||___/
;;; |_|                                          

;; Suffixes:
;; "-ne" means "(do) not ee-expand"
;; "0"  means "don't display in a temp buffer, just return the string"
;; "00" means "like `0', but more low-level: don't strip the trailing newline".

(defun ee-split   (str) (if (stringp str) (split-string str "[ \t]+") str))
(defun ee-unsplit (list) (if (listp list) (mapconcat 'identity list " ") list))
(defun ee-split-and-expand (str) (mapcar 'ee-expand (ee-split str)))
(defun ee-no-trailing-nl   (str) (replace-regexp-in-string "\n$" "" str))

(defun find-bgprocess-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'start-process (car argv) "*Messages*" argv)))

(defun find-callprocess00-ne (program-and-args)
  (let ((argv (ee-split program-and-args)))
    (with-output-to-string
      (with-current-buffer standard-output
	(apply 'call-process (car argv) nil t nil (cdr argv))))))

(defun find-callprocess0-ne (program-and-args)
  (ee-no-trailing-nl (find-callprocess00 program-and-args)))

(defun find-comintprocess-ne (name program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'make-comint name (car argv) nil (cdr argv))
    (switch-to-buffer (format "*%s*" name))))

(defun find-bgprocess     (program-and-args)
  (find-bgprocess-ne      (ee-split-and-expand program-and-args)))
(defun find-callprocess00 (program-and-args)
  (find-callprocess00-ne  (ee-split-and-expand program-and-args)))
(defun find-callprocess0  (program-and-args)
  (find-callprocess0-ne   (ee-split-and-expand program-and-args)))
(defun find-comintprocess (name program-and-args)
  (find-comintprocess-ne   name (ee-split-and-expand program-and-args)))

;; These two are like `find-sh', but more low-level.
(defun find-callprocess-ne (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00-ne ',program-and-args))
	 pos-spec-list))
(defun find-callprocess (program-and-args &rest pos-spec-list)
  (apply 'find-eoutput-reuse (ee-unsplit program-and-args)
	 `(insert (find-callprocess00 ',program-and-args))
	 pos-spec-list))





;;;  __  __            
;;; |  \/  |       ___ 
;;; | |\/| |_____ / _ \
;;; | |  | |_____|  __/
;;; |_|  |_|      \___|
;;;                    

(defun ee-eval-last-sexp ()
  "Like C-x C-e, but does not start the debugger on errors."
  (interactive)
  (let ((debug-on-error nil))
    (eval-last-sexp nil)))

(defun ee-eval-sexp-eol ()
  "Like C-e C-x C-e, but does not start the debugger on errors."
  (interactive)
  (end-of-line)
  (let ((debug-on-error nil))
    (eval-last-sexp nil)))










;;;          ____  _            _                
;;;         | __ )| | ___   ___| | _____         
;;;  _____  |  _ \| |/ _ \ / __| |/ / __|  _____ 
;;; |_____| | |_) | | (_) | (__|   <\__ \ |_____|
;;;         |____/|_|\___/ \___|_|\_\___/        
;;;                                              

;;;  __  __                             
;;; |  \/  |    __  __   ___  _____   __
;;; | |\/| |____\ \/ /  / _ \/ _ \ \ / /
;;; | |  | |_____>  <  |  __/  __/\ V / 
;;; |_|  |_|    /_/\_\  \___|\___| \_/  
;;;                                     

(defvar ee-file           (ee-expand "$EE")
  "The temporary script file used by `eev'.")
(defvar ee-file-tex       (ee-expand "$EETEX")
  "The temporary script file used by `eelatex'.")
(defvar eelatex-eevscript "cd $EEVTMPDIR/; latex tmp.tex && xdvi tmp.dvi &"
  "See `eelatex'.")

(defun ee-se-to-string (s e)
"Convert the pair (S E) to a string - see `write-region' for the rules."
  (cond ((numberp s) (buffer-substring-no-properties s e))
        ((stringp s) s)))

(defun ee-write-string (str &optional altfile)
  "Write STR to ALTFILE, or to `ee-file' if ALTFILE is nil."
  (write-region str nil (ee-expand (or altfile ee-file)))
  (format "Wrote %s" (or altfile ee-file)))

(defun ee-write (s e pre post &optional altfile)
  "Write PRE+(ee-se-to-string S E)+POST to ALTFILE, or to `ee-file'.
PRE and POST must be strings. See `ee-se-to-string' and `ee-write-string'."
  (ee-write-string (concat pre (ee-se-to-string s e) post)
		   altfile))

(defun eev (s &optional e altfile)
  "Save the region to `ee-file'"
  (interactive "r")
  (ee-write-string (ee-se-to-string s e) altfile))

(defun eelatex (s &optional e)
  "Save the region to `ee-file-tex', then save `eelatex-eevscript' to `ee-file'."
  (interactive "r")
  (ee-write s e "" "" ee-file-tex)
  (eev eelatex-eevscript nil)
  (format "eelatex: wrote %s and %s" ee-file-tex ee-file))




;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;;
;;; Set just one glyph: the red star

(defface eev-glyph-face-red '((t (:foreground "red")))
  "Face used for the red star glyph (char 15).")

(defun ee-glyph (char &optional face)
  (logior char (ash (if face (face-id face) 0) 19)))

(defun eev-set-glyph (pos &optional char face)
  (aset standard-display-table pos
	(if char (vector (ee-glyph char face)))))

;; Make `^O's appear at red stars.
;; To cancel that run this: (eev-set-glyph ?\^O)
(eev-set-glyph ?\^O ?* 'eev-glyph-face-red)



;;;             __ _           _     
;;;   ___  ___ / _| | __ _ ___| |__  
;;;  / _ \/ _ \ |_| |/ _` / __| '_ \ 
;;; |  __/  __/  _| | (_| \__ \ | | |
;;;  \___|\___|_| |_|\__,_|___/_| |_|
;;;                                  

(defvar ee-flash-spec '(highlight 0.75))

(defun eeflash (s e)
  "Highlight temporarily the region between S and E. See `ee-flash-spec'."
  (interactive "r")
  (if (numberp s)
      (let ((ovl (make-overlay s e))
	    (face     (car  ee-flash-spec))
	    (duration (cadr ee-flash-spec)))
	(overlay-put ovl 'face face)
    (run-at-time duration nil 'delete-overlay ovl)))
  (ee-se-to-string s e))




;;;                                  _                           _          _ 
;;;   ___  _____  ____  ____  __    | |__   ___  _   _ _ __   __| | ___  __| |
;;;  / _ \/ _ \ \/ /\ \/ /\ \/ /____| '_ \ / _ \| | | | '_ \ / _` |/ _ \/ _` |
;;; |  __/  __/>  <  >  <  >  <_____| |_) | (_) | |_| | | | | (_| |  __/ (_| |
;;;  \___|\___/_/\_\/_/\_\/_/\_\    |_.__/ \___/ \__,_|_| |_|\__,_|\___|\__,_|
;;;                                                                           

(defvar eeb-defaults '(eev "\n#\n"))

(defun eeb-default ()
  "Run the default action on a delimited region around point.
The default action is determined by the entries in `eeb-defaults'.
See `eeb-define'."
  (interactive)
  (let* ((fun   (nth 0 eeb-defaults))
	 (delim (nth 1 eeb-defaults))
	 (s     (ee-search-backward edelim))
	 (e     (ee-search-forward  edelim)))
    (eeflash s e)
    (funcall fun s e)))

(defun ee-eeb-define (eeb-fun fun delim)
"Returns code (as as string) to define EEB-FUN as a wrapper around FUN."
  (read (format "
    (defun %S ()
      (interactive)
      (setq eeb-defaults '(%S %S))
      (eeb-default))" eeb-fun fun delim))))

(defun eeb-define (eeb-fun fun delim)
"Define EEB-FUN as a wrapper around FUN."
  (eval (read (ee-eeb-define eeb-fun fun delim))))

(defun find-eeb-define (eeb-fun fun delim)
  "Show the code that an `eeb-define' with the same args would run."
  (find-estring-elisp (ee-eeb-define eeb-fun fun delim)))

(eeb-define 'eev-bounded     'eev     "\n#\n")
(eeb-define 'eegdb-bounded   'eegdb   "\n#\n")
(eeb-define 'eelatex-bounded 'eelatex "\n%\n")
(eeb-define 'eeeval-bounded  'eeeval  "\n;;\n")
(eeb-define 'eeb-eval        'eeeval  "\n;;\n")











;;;          _____ _               
;;;         | ____| |_ ___         
;;;  _____  |  _| | __/ __|  _____ 
;;; |_____| | |___| || (__  |_____|
;;;         |_____|\__\___|        
;;;                                

(setq pop-up-windows nil))


;;;  _                                    
;;; | | _____ _   _ _ __ ___   __ _ _ __  
;;; | |/ / _ \ | | | '_ ` _ \ / _` | '_ \ 
;;; |   <  __/ |_| | | | | | | (_| | |_) |
;;; |_|\_\___|\__, |_| |_| |_|\__,_| .__/ 
;;;           |___/                |_|    
;; keymap: (find-eevfile "eev.el" "\n(defvar eev-mode-map")
;; M-e

(defvar eev-mode-map nil)
(if eev-mode-map
    ()
(setq eev-mode-map (make-sparse-keymap))
(define-key eev-mode-map "\M-E" 'ee-eval-last-sexp)     ;     C-x C-e
(define-key eev-mode-map "\M-e" 'ee-eval-sexp-eol)      ; C-e C-x C-e
(define-key eev-mode-map "\M-k" 'kill-this-buffer)      ; convenience
(define-key eev-mode-map "\M-K" 'bury-buffer)           ; convenience
(define-key eev-mode-map [f3]   'eeb-default)
(define-key eev-mode-map [f8]   'eepitch-this-line)
(define-key eev-mode-map [f9]   'eechannel-this-line)
(define-key eev-mode-map [f12]  'eesteps-do-step)
(define-key eev-mode-map "\M-h\M-f" 'find-efunction-links) ; in eev-insert.el
)

;; M-h M-2
;; M-h M-y












;; Do I need these things here in eev-mini?
;;
;; (defun ee-with-trailing-nl (str)
;;   "Add a trailing newline to STR if it ends on another char."
;;   (if (string-match "[^\n]\\'" str) (concat str "\n") str))
;; 
;; (defun ee-se-to-string-with-nl (s e)
;;   (ee-with-trailing-nl (ee-se-to-string s e)))
;; 
;; (defun ee-write-with-nl (s e pre post &optional altfile)
;;   "Same as `ee-write', but using `ee-se-to-string-with-nl'."
;;   (ee-write-string (concat pre (ee-se-to-string-with-nl s e) post)
;;                    altfile))







;; Local Variables:
;; coding:           raw-text-unix
;; ee-anchor-format: "defun %s "
;; no-byte-compile:  t
;; End:
