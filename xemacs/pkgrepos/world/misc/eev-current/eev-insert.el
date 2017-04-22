;;; eev-insert.el --- create and insert Elisp hyperlinks

;; Copyright (C) 1999,2000,2001,2002,2003,2004,2005,2006,2007,2010,
;; 2011 Free Software Foundation, Inc.
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
;; Version:    2011feb24
;; Keywords:   e-scripts, help, hyperlinks, hypertext

;;; Commentary:

;; 2006aug17: Major rewrite! The old version was moved to
;; eev-insert-old.el.

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

(defun ee-addhp (string)
  "Prepend `ee-hyperlink-prefix' to STRING"
  (concat ee-hyperlink-prefix string))

(defun ee-pph (object &optional tick)
  "Pretty-print OBJECT into a hyperlink line."
  (ee-addhp (ee-pp0 object tick)))



;;;
;;; Code for dealing with lists of hyperlinks.
;;; 

(defun ee-link-to-string  (link)
  "Internal use; convert a \"raw hyperlink\" to a hyperlink line."
  (concat (cond ((eq link nil) "")	; `nil's become empty lines
		((stringp link) link)	; strings become themselves
		(t (ee-pph link)))	; lispish things are ee-pph'ed
	  "\n"))			; always add a newline

(defun ee-links-to-string (links)
  "Convert a list of (raw) hyperlinks to a string.
Each element of LINKS becomes a line in the result.
See the source for details: (find-efunction 'ee-link-to-string)"
  (concat (mapconcat 'ee-link-to-string links "")))

(defun find-elinks (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*")))
    (apply 'find-estring (ee-links-to-string links) pos-spec-list)))

(defun find-elinks-elisp (links &rest pos-spec-list)
  "Visit a temporary buffer containing LINKS converted to hyperlink lines.
The buffer is put in Emacs Lisp mode."
  (let ((ee-buffer-name (or ee-buffer-name "*Elisp hyperlinks*")))
    (apply 'find-estring-elisp (ee-links-to-string links) pos-spec-list)))

(defalias 'find-elinks-new 'find-elinks)






;;;  _                     ____                    __                    
;;; | | _____ _   _ ___   / / _|_   _ _ __  ___   / /_   ____ _ _ __ ___ 
;;; | |/ / _ \ | | / __| / / |_| | | | '_ \/ __| / /\ \ / / _` | '__/ __|
;;; |   <  __/ |_| \__ \/ /|  _| |_| | | | \__ \/ /  \ V / (_| | |  \__ \
;;; |_|\_\___|\__, |___/_/ |_|  \__,_|_| |_|___/_/    \_/ \__,_|_|  |___/
;;;           |___/                                                      

(defun ee-links-for-eboundkey (key f)
  "From KEY and its binding, F, produce a list of hyperlinks."
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
    (key-binding ,key)
    ))

(defun ee-links-for-efunction (f)
  "Return a list of hyperlinks for F (a function symbol)."
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

(defun ee-links-for-evariable (var)
  "Return a list of hyperlinks for VAR (a variable, as a symbol)."
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

;; (find-ekey-links "\C-x2")
;; (find-ekey-links [down])
;; (find-elongkey-links "<down>")
;; (find-efunction-links '+)
;; (find-efunction-links 'next-line)
;; (find-evariable-links 'default-directory)
;; (find-debpkg-links "bash")

(defun find-ekey-links (key &rest rest)
"Visit a temporary buffer containing hyperlinks related to the key sequence KEY."
  (interactive "kElisp hyperlinks for key: ")
  (let ((longkey (format-kbd-macro key))
	(longkey+ (replace-regexp-in-string "[ \t][ \t]+" "  "
					    (format-kbd-macro key t)))
	(binding (key-binding key)))
    (apply 'find-elinks
	   `((find-ekey-links ,key)
	     (eekill ,longkey)
	     (eek ,(format "M-h M-k %s" longkey))
	     (find-elongkey-links ,longkey)
	     (find-elongkey-links ,longkey+)
	     ;; ,longkey+
	     ""
	     ;; ,@(eemakelinks-eboundkey key binding)
	     ,@(ee-links-for-eboundkey key binding)
	     )
	   rest)))

(defun find-elongkey-links (longkey &rest rest)
  "Like `find-ekey-links', but LONGKEY is a key sequence \"spelled out\".
Example: (find-elongkey-links \"M-h M-k\")
See `read-kbd-macro' and `edmacro-mode' for the format."
  (interactive "sElisp hyperlinks for key (long format): ")
  (let* ((key (read-kbd-macro longkey))
	 (binding (key-binding key)))
    (apply 'find-elinks
	   `((find-elongkey-links ,longkey)
	     (find-ekey-links ,key)
	     ""
	     ;; ,@(eemakelinks-eboundkey key binding)
	     ,@(ee-links-for-eboundkey key binding)
	     )
	   rest)))

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

;; Missing, for variables (?): links to configuration info and to docstrings



;;;   __ _           _        __ _ _            _ _       _        
;;;  / _(_)_ __   __| |      / _(_) | ___      | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____| |_| | |/ _ \_____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  _| | |  __/_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     |_| |_|_|\___|     |_|_|_| |_|_|\_\___/
;;;                                                                

;; To do:
;;   shorten file names (/home/edrx -> ~)
;;   add links to code to change permissions

(defun ee-filter (function list)
  "Return a sublist of LIST with only the elements for which (FUNCTION elt) is true."
  (let (newlist)
    (while (consp list)
      (if (funcall function (car list))
	  (setq newlist (cons (car list) newlist)))
      (setq list (cdr list)))
    (nreverse newlist)))

(defun ee-filter-out-nils (list)
  "Return a list with the elements of LIST that are not nil."
  (ee-filter 'identity list))

(defun ee-prefixp (prefix str)
  "Return t if STR begins with PREFIX."
  (and (<= (length prefix) (length str))
       (equal prefix (substring str 0 (length prefix)))))

(defun code-c-d-prefixes (path)
"Return the entries (C D) in `code-c-d-list' for which D is a prefix of PATH." 
  (ee-filter (lambda (c-d) (ee-prefixp (car (cdr c-d)) path))
	     code-c-d-list))

;;(code-c-d-prefixes (ee-expand "~/bigsrc/kernel-source-2.6.8/Documentation/"))
;; (ee-links-for-findxxxfile-1 "foo" "/usr/src/foo/" "/usr/src/foo/bar")
;; (ee-links-for-findxxxfile (ee-expand "~/bigsrc/kernel-source-2.6.8/Documentation/"))
;; (find-file-links "~/bigsrc/kernel-source-2.6.8/Documentation/")

(defun ee-remove-prefix (prefix str)
  (substring str (length prefix)))

(defun ee-links-for-findxxxfile-1 (c d path)
  (list (intern (format "find-%sfile" c))
	(ee-remove-prefix d path)))

(defun ee-links-for-findxxxfile (path)
  (mapcar (lambda (c-d) (ee-pph (ee-links-for-findxxxfile-1
				 (car c-d) (nth 1 c-d) path)))
	  (code-c-d-prefixes path)))

(defun find-file-links (fname &rest rest)
  "Visit a temporary buffer containing hyperlinks related a file."
  (interactive (list (or (buffer-file-name) default-directory)))
  (apply 'find-elinks
	 `((find-file-links ,fname)
	   ""
	   (find-fline ,fname)
	   ""
	   ,@(ee-links-for-findxxxfile (ee-expand fname)))
	 rest))



;;;   __ _           _            _        __             _ _       _        
;;;  / _(_)_ __   __| |       ___(_)_ __  / _| ___       | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |_____ / _ \ | '_ \| |_ / _ \ _____| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |_____|  __/ | | | |  _| (_) |_____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|      \___|_|_| |_|_|  \___/      |_|_|_| |_|_|\_\___/
;;;                                                                          

(defun find-einfo-links (&optional book+ &rest rest)
  "Visit a temporary buffer containing hyperlinks related to an info page.
The default is to use the info page being visited in the \"*info*\" buffer."
  (interactive)
  (let* ((book+ (or book+ (with-current-buffer "*info*" Info-current-file)))
	 (book- (file-name-nondirectory book+))
	 (code- (file-name-nondirectory ee-info-file))
	 (code  (if (string= book- code-) code-))
	 (find-xxxnode (if code (read (format "find-%snode" ee-info-code))))
	 (node (with-current-buffer "*info*" Info-current-node))
	 (booknode (format "(%s)%s" book- node)))
    (apply 'find-elinks
	   `((find-einfo-links)
	     ""
	     (info ,booknode)
	     (find-node ,booknode)
	     ,@(if find-xxxnode (list `(,find-xxxnode ,node))))
	   rest)))


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



;;;      _      _     _             
;;;   __| | ___| |__ (_) __ _ _ __  
;;;  / _` |/ _ \ '_ \| |/ _` | '_ \ 
;;; | (_| |  __/ |_) | | (_| | | | |
;;;  \__,_|\___|_.__/|_|\__,_|_| |_|
;;;                                 
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
(defun ee-dfs0 (pkg ext)
  "If the file /var/lib/dpkg/info/PKG.EXT exists, return a link to it."
  (let ((fname (concat pkg "." ext)))
    (if (file-exists-p (ee-vldifile fname))
	(format "%s(find-vldifile \"%s\")" ee-hyperlink-prefix fname))))

(defun ee-links-for-debpkg (pkg)
  "Return the three main links for the debian package named PKG."
  (list (format "%s(find-status   \"%s\")"      ee-hyperlink-prefix pkg)
	(format "%s(find-vldifile \"%s.list\")" ee-hyperlink-prefix pkg)
	(format "%s(find-udfile   \"%s/\")"     ee-hyperlink-prefix pkg)))

(defun ee-links-for-debpkg-extra-vldi (pkg)
  "Return a list of links for files in /var/lib/dpkg/info/ belonging to PKG."
  (ee-filter-out-nils
   (list (ee-dfs0 pkg "preinst")   (ee-dfs0 pkg "postinst")
	 (ee-dfs0 pkg "prerm")     (ee-dfs0 pkg "postrm")
	 (ee-dfs0 pkg "conffiles") (ee-dfs0 pkg "config")
	 (ee-dfs0 pkg "templates")
	 (ee-dfs0 pkg "md5sums")   (ee-dfs0 pkg "shlibs"))))

(defun ee-debian-pooldir (pkg)
  "Used by find-debpkg-links; \"foo\" -> \"f\", \"libfoo\" -> \"libf\"."
  (if (string-match "^\\(lib\\)?." pkgname)
      (match-string 0 pkgname)))

(defun find-debpkg-links (pkgname &rest rest)
  "Visit a temporary buffer containing hyperlinks related to a Debian package."
  (interactive (list (ee-debpkgname-ask)))
  (apply 'find-elinks
	 `((find-debpkg-links ,pkgname)
	   ""
	   (find-available ,pkgname)
	   ""
	   ,@(ee-links-for-debpkg pkgname)
	   ""
	   ,@(ee-links-for-debpkg-extra-vldi pkgname)
	   ""
	   (find-sh ,(format "apt-cache dump | grep-dctrl -P %s" pkgname))
	   (find-sh ,(format "apt-cache search %s | sort" pkgname))
	   (find-sh ,(format "apt-cache showpkg %s" pkgname))
	   (find-sh ,(format "grep-aptavail -P %s" pkgname))
	   ""
	   ,(concat "http://packages.debian.org/" pkgname)
	   ,(concat "http://packages.debian.org/src:" pkgname)
	   ,(format "http://ftp.debian.org/debian/pool/main/%s/%s/"
		    (ee-debian-pooldir pkgname) pkgname)
	   ,(format "http://backports.org/debian/pool/main/%s/%s/"
		    (ee-debian-pooldir pkgname) pkgname)
	   ,(format "http://bugs.debian.org/cgi-bin/pkgreport.cgi?which=pkg&data=%s&archive=no" pkgname))
	 rest))






;;;  __  __       _       __  __      ____  
;;; |  \/  |     | |__   |  \/  |    |___ \ 
;;; | |\/| |_____| '_ \  | |\/| |_____ __) |
;;; | |  | |_____| | | | | |  | |_____/ __/ 
;;; |_|  |_|     |_| |_| |_|  |_|    |_____|
;;;  __  __       _       __  __             
;;; |  \/  |     | |__   |  \/  |      _   _ 
;;; | |\/| |_____| '_ \  | |\/| |_____| | | |
;;; | |  | |_____| | | | | |  | |_____| |_| |
;;; |_|  |_|     |_| |_| |_|  |_|      \__, |
;;;                                    |___/ 

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
  `M-h M-2' runs `eemklinks-yank-pos-spec'.

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
(defun ee-hyperlink-prefix ()
  "A lispish interface for customizing the variable `ee-hyperlink-prefix'.
See the comments in the source code."
  (interactive)
  (find-elinks
   `((ee-hyperlink-prefix)
     (setq ee-hyperlink-prefix ,ee-hyperlink-prefix)
     ""
     (setq ee-hyperlink-prefix "# ")
     (setq ee-hyperlink-prefix ";; ")
     (setq ee-hyperlink-prefix "-- ")
     (setq ee-hyperlink-prefix "% ")
     )))



;;;   __ _           _                _                    
;;;  / _(_)_ __   __| |      _ __ ___(_)_ __ ___           
;;; | |_| | '_ \ / _` |_____| '__/ __| | '__/ __|____      
;;; |  _| | | | | (_| |_____| | | (__| | | | (_|_____| _ _ 
;;; |_| |_|_| |_|\__,_|     |_|  \___|_|_|  \___|   (_|_|_)
;;;                                                        

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
  (list (ee-pp0 sexp) (format "  -> %s" (ee-pp0 (eval sexp)))))

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




;;;   __ _           _                     _                _ _       _        
;;;  / _(_)_ __   __| |      ___  ___ ___ | | ___  _ __    | (_)_ __ | | _____ 
;;; | |_| | '_ \ / _` |____ / _ \/ __/ _ \| |/ _ \| '__|___| | | '_ \| |/ / __|
;;; |  _| | | | | (_| |____|  __/ (_| (_) | | (_) | | |____| | | | | |   <\__ \
;;; |_| |_|_| |_|\__,_|     \___|\___\___/|_|\___/|_|      |_|_|_| |_|_|\_\___/
;;;                                                                              
;; 2007sep29
;; (find-ecolor-links)
;; (find-ecolor-links "sienna")
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



;;;                  _       _       
;;;  _   _ _ __   __| | __ _| |_ ___ 
;;; | | | | '_ \ / _` |/ _` | __/ _ \
;;; | |_| | |_) | (_| | (_| | ||  __/
;;;  \__,_| .__/ \__,_|\__,_|\__\___|
;;;       |_|                        

;; (find-eev-update-links)
(defun find-eev-update-links (&optional dir &rest rest)
  (interactive)
  (let ((edir (or dir (ee-expand ee-eevdir))))
    (apply 'find-elinks `(
      (find-eev-update-links ,dir ,@rest)
      (find-eev-update-links "~/eev-current/" ,@rest)
      (find-eev-update-links ,edir ,@rest)
      (find-efunction 'find-eev-update-links)
      ""
      (ee-expand ee-eevdir)
      (find-eevfile "eev.el" "$EEVDIR")
      (find-eevfile "")
      ""
      ,(format " (eepitch-shell)")
      ,(format " (eepitch-kill)")
      ,(format " (eepitch-shell)")
      ""
      ,(format "mkdir %s" edir)
      ,(format "cd    %s" edir)
      ,(format "mv -v eev-current.tar.gz eev-current-old.tar.gz")
      ,(format "wget http://angg.twu.net/eev-current.tar.gz")
      ,(format "tar -xvzf eev-current.tar.gz")
      ""
      ,(format "(progn")
      ,(format "  (add-to-list 'load-path %S)" edir)
      ,(format "  (require 'eev-all)")
      ,(format "  (eev-mode 1)")
      ,(format "  )")
      ""
      ,(format " (eepitch-shell)")
      ,(format " (eepitch-kill)")
      ,(format " (eepitch-shell)")
      ""
      ,(format "cd %s" edir)
      ,(format "./eev-rctool prepare")
      ,(format "./eev-rctool install_rc")
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




;;;                                          
;;;   ___  ___     __      ___ __ __ _ _ __  
;;;  / _ \/ _ \____\ \ /\ / / '__/ _` | '_ \ 
;;; |  __/  __/_____\ V  V /| | | (_| | |_) |
;;;  \___|\___|      \_/\_/ |_|  \__,_| .__/ 
;;;                                   |_|    
;;

;; 2007dec21: this family of functions should replace ee-dfa, ee-inn,
;; etc, soon.

(defun ee-delete-extract-wrap (f &optional n)
  (dotimes (i (or n 1))
    (let ((str (ee-no-trailing-nl
		(ee-no-properties
		 (ee-delete-and-extract-line 1)))))
      (insert (ee-links-to-string (funcall f str))))))

(defun ee-wrap-file-links   (str) `((find-fline ,str)))
(defun ee-wrap-man-links    (str) `((find-man   ,str)))
(defun ee-wrap-sh-links     (str) `((find-sh    ,str)))
(defun ee-wrap-sh0-links    (str) `((find-sh0   ,str)))
(defun ee-wrap-debian-links (str) (ee-links-for-debpkg str))
(defun ee-wrap-code-c-d-links (str)
  (let* ((spl (split-string str))
	 (c (car spl))
	 (d (cadr spl))			; bug: will fail when d contains spaces
	 (find-_file (intern (format "find-%sfile" c))))
    `((code-c-d ,c ,d)
      (,find-_file ""))))
(defun ee-wrap-eepitch-links (str)
  `(,(format " (eepitch-%s)" str)
    ,(format " (eepitch-kill)")
    ,(format " (eepitch-%s)" str)))

(defun ee-wrap-file (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-file-links n))

(defun ee-wrap-man (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-man-links n))

(defun ee-wrap-debian (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-debian-links n))

(defun ee-wrap-sh (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-sh-links n))

(defun ee-wrap-sh0 (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-sh0-links n))

(defun ee-wrap-code-c-d (&optional n)
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-code-c-d-links n))

(defun ee-wrap-eepitch (&optional n)
  "Generate an \" (eepitch-{xxx,kill,xxx})\" block.
For example, the three \"\" lines below were generated by typing
first \"shell\" and then `\\[ee-wrap-eepitch]'. See `eepitch'.

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)
cd /tmp/
ls\n\n
"
  (interactive "p")
  (ee-delete-extract-wrap 'ee-wrap-eepitch-links n))




;;;
;;; ee-template: a better way to handle complex substitutions.
;;; This is new (2008feb11), and I am planning to replace many of the
;;; calls to `format' in the `find-xxx-links' functions above by calls
;;; to this...
;;;
;; The demos are in the docstring, so run this:
;; (find-efunctiondescr 'ee-template)

(defun ee-template (pairs templatestr)
  "Substitute all ocurrences of \"{tags}\" in TEMPLATESTR.
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
  ;; The following setq is a hack to convert symbols to pairs...
  ;; The example above doesn't explain how to use it yet!
  (setq pairs
	(mapcar (lambda (o)
		  (if (symbolp o)
		      (list (symbol-name o) (symbol-value o))
		    o))
		pairs))
  (let ((f (lambda (match)
	     (or (cadr (assoc (match-string 1 match) pairs))
		 match))))
    (replace-regexp-in-string "{\\([^{}]+\\)}" f templatestr 'fixedcase)))




;; Two auxiliary functions.
(defun ee-gformat (fmt)
  `(lambda (str) (replace-regexp-in-string
		  "\\`\\(.\\|\n\\)*\\'" ,fmt str 'fixedcase)))

(defun ee-gmapconcat-split (fmt sep str)
  (mapconcat (ee-gformat fmt) (ee-split str) (or sep "")))




;;;
;;; find-find-links-links:
;;; a way to generate skeletons for `find-xxx-links' functions.
;;; (Experimental, 2008feb11/2011feb24; may change)
;;;
;; Tests:
;; (find-find-links-links)
;; (find-find-links-links "aaa")

(defun find-find-links-links (&optional xxx args &rest rest)
"Visit a temporary buffer containing a skeleton for a `find-xxx-links' function."
  (interactive)
  (setq xxx (or xxx "{xxx}"))
  (setq args (or args ""))
  (apply 'find-elinks-elisp `(
    (find-find-links-links ,xxx ,args ,@rest)
    (find-efunction 'find-find-links-links)
    ""
    ,(ee-template
      `(("xxx" ,xxx)
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

;; (find-find-links-links "aaa")
;; (find-find-links-links "aaa" "b c")




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

;; These functions are old and have horrible names but I find them
;; useful - 

(defvar ee-comment-prefix nil)
(make-variable-buffer-local 'ee-comment-prefix)
(defun  ee-comment-prefix ()
  (or ee-comment-prefix "# "))		; to do: mode -> "#"/"%"/" *"/"--"

(defun ee-set-comment-prefix (value)
  (interactive "Xee-comment-prefix (in Lisp): ")
  (set (make-variable-buffer-local 'ee-comment-prefix) value))

(defun ee-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)


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

(defun ee-delete-and-extract-line (&optional adjust)
  (delete-and-extract-region (progn (beginning-of-line) (point))
			     (progn (end-of-line)
				    (min (+ (point) (or adjust 0))
					 (point-max)))))

(defun ee-dff (N)
"Convert the name of a debian package in the current line into three hyperlinks.
For example, if the current line contains just the string
\"bash\" and you run this function then the line becomes this:\n
# (find-status   \"bash\")
# (find-vldifile \"bash.list\")
# (find-udfile   \"bash/\")\n"
  (interactive "p")
  (dotimes (i N)
    (let ((pkgname (ee-delete-and-extract-line))
	  (prefix ee-hyperlink-prefix))
      (insert (format (concat "%s(find-status   \"%s\")\n"
			      "%s(find-vldifile \"%s.list\")\n"
			      "%s(find-udfile   \"%s/\")")
		      prefix pkgname prefix pkgname prefix pkgname))
      (next-line 1))))

(defun ee-dfa (N)
  (interactive "p")
  (dotimes (i N)
    (insert (format "%s(find-available \"%s\")"
		    (ee-comment-prefix) (ee-delete-and-extract-line)))
    (next-line 1)))

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




;; Obsolete utility functions.
;; I didn't know about backquoting when I wrote those.
;; Tests:
;; (ee-concat '("foo" nil ("bar" nil (("squeak" "")))))
;;
' (defun ee-concat (list &optional sep)
    "(OBSOLETE) Flatten LIST and concat it as a series of lines."
    (setq list (ee-flatten list))
    (or sep (setq sep "\n"))
    (setq list (mapcar (lambda (str) (concat str sep)) list))
    (apply 'concat list))

;; Obsolete - renamed to -old
' (defun find-elinks-old (list &rest rest)
    "OBSOLETE- Use find-elinks instead."
    (let ((ee-buffer-name "*Elisp hyperlinks*"))
      (apply 'find-estring (ee-concat list) rest)))



(provide 'eev-insert)





;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; ee-comment-prefix: ";; "
;; no-byte-compile:   t
;; End:
