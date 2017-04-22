;;; eev-bowse-url.el - eev functions that operate on the url at point.

;; Copyright (C) 2006,2007,2008,2009,2012 Free Software Foundation, Inc.
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
;; Version:    2012feb27
;; Keywords:   e-scripts, hyperlinks, hypertext
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-bowser-url.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-bowser-url.el.html>

;;; Commentary:

;; Here's a quick explanation of what the functions in this file do.
;; Suppose that you've downloaded local copies of two URLs:
;;
;;        http://foo.bar/plic.txt
;;   and  http://foo.bar/ploc/bletch.ps
;;
;; using "psne" (for an alternative, see `brep', below); if the
;; environment variable $S is set to the default, that is, S=~/snarf,
;; then the local copies will be at
;;
;;        ~/snarf/http/foo.bar/plic.txt
;;   and  ~/snarf/http/foo.bar/ploc/bletch.ps ;
;;
;; then if you place the point over "http://foo.bar/plic.txt" and type
;; `M-x brfl' you will visit the local copy of "plic.txt" in a buffer,
;; with `find-fline'; if you place the point over
;; "http://foo.bar/ploc/bletch.ps" and type `M-x brgvl' you will open
;; the local copy of "bletch.ps" with gv, with `find-pspage'; and if
;; you place the point over any one of these urls and type `M-x brep'
;; then you'll get a temporary buffer like this:
;;   ______________________________________________________________ 
;;  |# (find-psne-links "http://foo.bar/ploc/bletch.ps")           |
;;  |# http://foo.bar/ploc/bletch.ps                               |
;;  |                                                              |
;;  | (eepitch-shell)                                             |
;;  |mkdir -p $S/http/foo.bar/ploc/                                |
;;  |cd       $S/http/foo.bar/ploc/                                |
;;  |wget      http://foo.bar/ploc/bletch.ps                       |
;;  |echo     'http://foo.bar/ploc/bletch.ps' >> ~/.psne.log       |
;;  |                                                              |
;;  |                                                              |
;;  |--:**-  *Elisp hyperlinks*   All L1    (Fundamental)----------|
;;  |______________________________________________________________|
;;
;; that you can use to download a local copy even if you have not
;; installed the rcfile patches with eev-rctool to have a "psne"
;; command function available in the shell.
;;
;; See <http://angg.twu.net/eev-article.html#local-copies>.
;;
;; Notes:
;; A log of the local copies is stored at: (find-fline ".psne.log")
;; and these local copies can be "visited" with brfl, brgvl, brml,
;; brwl, etc.
;; 2007apr01: offby1 pointed this to me:
;;   (find-efunction 'browse-url-of-dired-file)
;;   (find-efunction 'browse-url-of-file)

;; «.conversion-functions»	(to "conversion-functions")
;; «.eeurl-define-from»		(to "eeurl-define-from")
;; «.many-br-functions»		(to "many-br-functions")

;; New stuff, 2007dec21:
;; «.find-wget»			(to "find-wget")
;; «.brwget»			(to "brwget")
;; «.ee-cp»			(to "ee-cp")
;; «.find-psne-links»		(to "find-psne-links")
;; «.brep»			(to "brep")


;; Autoloads:
;; (find-efile "net/browse-url.el")
;; (find-efile "net/browse-url.el" "defun browse-url-interactive-arg")
;;
(autoload 'browse-url-interactive-arg "browse-url")



;;
;; Utility functions.
;;

(defun eeurl-dired-file-name-at-point ()
  (if (eq major-mode 'dired-mode)
      (file-name-sans-versions (dired-get-filename) t)
    (error "Not in dired mode")))


;; «conversion-functions»  (to ".conversion-functions")
;;
(defun eeurl-u-to-f (url)
  "Convert an url like http://foo/bar to a filename like $S/http/foo/bar."
  ;; Add comments about psne and the snarf directory
  (replace-regexp-in-string "^\\(https?\\|ftp\\)://" "$S/\\1/" url))

(defun eeurl-f-to-u (fname)
  "Convert a filename to a \"file://\" url"
  (concat "file://" (expand-file-name (ee-expand fname))))

(defun eeurl-u-to-u-l (url)
  "Convert a url like http://foo/bar to a url like file://<$S>/http/foo/bar.
This should be made smarter - file:// urls should be returned unchanged."
  ;; Add comments about psne and the snarf directory
  (eeurl-f-to-u (eeurl-u-to-f url)))


;; The functions that generate the defuns.
;; Here is the explanation for the cryptic names that they use.
;; Names, long form:
;;              dired-    Names, short form:
;;   url-at-   fname-at-
;;    point      point        up     dfp
;;      |          |          |       |
;;      v          v          v       v
;;     url <===> fname        u <===> f
;;        \      /             \     /
;;         v    v               v   v
;;         action                 a
;;
;; Also, an "l" suffix means "prefer local copy" when both local and
;; remote make sense.
;;
;; Example: `eeurl-utoa-to-uptoa-defun' takes the name of a u->a
;; function (a symbol) and produces the defun for a up->a function
;; that is a wrapper around the original function.
;;
;; u  - string: an url, like http://foo/bar
;; sf - string: a snarfed filename, like $S/http/foo/bar
;; f  - string: a filename, like /tmp/foo
;; fu - string: a "file://" url, like file:///tmp/foo
;; su - string: a snarfed file url, like file:///home/edrx/snarf/http/foo/bar
;; _fun - symbol; a function whose argument is a _
;; brfun - symbol: a browse-url-like function
;; def___ - a defun sexp

(defun eeurl-utoa-to-uptoa-defun (find-uxxx brxxx)
  "Try this: (find-epp (eeurl-utoa-to-uptoa-defun 'find-w3m 'brw))"
  `(defun ,brxxx (url &rest ignore)
     ,(format "Apply `%S' on URL." find-uxxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxx)
     (list ',find-uxxx url '-> (,find-uxxx url))))

(defun eeurl-utoa-to-uptoal-defun (find-uxxx brxxxl)
  "Try this: (find-epp (eeurl-utoa-to-uptoal-defun 'find-w3m 'brwl))"
  `(defun ,brxxxl (url &rest ignore)
     ,(format "Apply `%S' on the local url associated to URL." find-uxxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxxl)
     (setq url (eeurl-u-to-u-l url))
     (list ',find-uxxx url '-> (,find-uxxx url))))

(defun eeurl-ftoa-to-uptoa-defun (find-xxx brxxxl)
  "Try this: (find-epp (eeurl-ftoa-to-uptoa-defun 'find-fline 'brfl))"
  `(defun ,brxxxl (url &rest ignore)
     ,(format "Apply `%S' on the local file name associated to URL." find-xxx)
     (interactive (browse-url-interactive-arg "URL: "))
     (setq browse-url-browser-function ',brxxxl)
     (let ((fname (eeurl-u-to-f url)))
       (list ',find-xxx fname '-> (,find-xxx fname)))))

(defun eeurl-ftoa-to-dfptoa-defun (find-xxx brxxxd)
  "Try this: (find-epp (eeurl-ftoa-to-dfptoa-defun 'find-pspage 'brgvd))"
  `(defun ,brxxxd ()
     ,(format "Apply `%S' on the dired file at point." find-xxx)
     (interactive)
      (let ((fname (eeurl-dired-file-name-at-point)))
	(message (format "%S" (list ',find-xxx fname '->
				    (,find-xxx fname)))))))

(defun eeurl-utoa-to-dfptoa-defun (find-uxxx brxxxd)
  "Try this: (find-epp (eeurl-utoa-to-dfptoa-defun 'find-w3m 'brwd))"
  ;; Note: a command like brgvd is in the right format to be bound in
  ;; dired-mode-map... See, for example: (find-efunction 'dired-find-file)
  `(defun ,brxxxd ()
     ,(format "Apply `%S' on the url of the dired file at point." find-uxxx)
     (interactive)
      (let ((url (eeurl-f-to-u
		  (eeurl-dired-file-name-at-point))))
	(message (format "%S" (list ',find-uxxx url '->
				    (,find-uxxx url)))))))


;;
;; The high-level interface - eeurl-define-from
;;

;; «eeurl-define-from»  (to ".eeurl-define-from")

(defun eeurl-keywords-to-builder (keyword1 keyword2)
  (let ((ks (list keyword1 keyword2)))
    (cond ((equal ks '(:url->action:  :remote:)) 'eeurl-utoa-to-uptoa-defun)
	  ((equal ks '(:url->action:   :local:)) 'eeurl-utoa-to-uptoal-defun)
	  ((equal ks '(:url->action:   :dired:)) 'eeurl-utoa-to-dfptoa-defun)
	  ((equal ks '(:fname->action: :local:)) 'eeurl-ftoa-to-uptoa-defun)
	  ((equal ks '(:fname->action: :dired:)) 'eeurl-ftoa-to-dfptoa-defun))))

(defun eeurl-builders-for-define-from
  (keyword1 origfun keyword2 newfun &rest rest)
  "Internal use - see: (find-efunctiondescr 'eeurl-define-from)"
  (cons `(,(eeurl-keywords-to-builder keyword1 keyword2)
	  ',origfun ',newfun)
	(if rest (apply 'eeurl-builders-for-define-from
			keyword1 origfun rest))))

(defun eeurl-defuns-for-define-from (&rest rest)
  "Internal use - see: (find-efunctiondescr 'eeurl-define-from)"
  (mapcar 'eval (apply 'eeurl-builders-for-define-from rest)))

;; This is pretty nice...
(defun find-eeurl-define-from (&rest rest)
  "Show the code that a `eeurl-define-from' call would evaluate, without evaluating it."
  (find-epp (cons 'progn (apply 'eeurl-defuns-for-define-from rest))))

(defun eeurl-define-from (&rest rest)
  "Define a series of browse-url or dired-visit functions from a standard function.
This is hard to describe abstractly, so try the `find-epp' sexps
below - they just produce lists and display them, and have no
side-effects.

  (find-epp (eeurl-builders-for-define-from
	     :fname->action: 'find-pspage
	     :local:         'brgvl
	     :dired:         'brgvd))

  (find-epp (eeurl-defuns-for-define-from
	     :fname->action: 'find-pspage
	     :local:         'brgvl
	     :dired:         'brgvd))

`eeurl-define-from' runs the defuns that
`eeurl-defuns-for-define-from' generates, so...
There are more examples in the source file. Eh, more later."
  (eval (cons 'progn (apply 'eeurl-defuns-for-define-from rest))))


;;
;; Define lots of br functions.
;; «many-br-functions»  (to ".many-br-functions")
;;

(eeurl-define-from :fname->action: 'find-fline
                   :local:         'brfl)
(eeurl-define-from :fname->action: 'eecd
                   :local:         'brcdl)
(eeurl-define-from :fname->action: 'find-pspage
                   :local:         'brgvl
                   :dired:         'brgvd)
(eeurl-define-from :fname->action: 'find-dvipage
                   :local:         'brxdvil
                   :dired:         'brxdvid)
(eeurl-define-from :fname->action: 'find-xpdfpage
                   :local:         'brxpdfl
                   :dired:         'brxpdfd)
(eeurl-define-from :fname->action: 'find-pdftotext
                   :local:         'brpdftxtl
                   :dired:         'brpdftxtd)
(eeurl-define-from :fname->action: 'find-djvupage
                   :local:         'brdjvul
                   :dired:         'brdjvud)
(eeurl-define-from :url->action:   'browse-url-firefox
		   :remote:        'brm
                   :local:         'brml
		   :dired:         'brmd)
(eeurl-define-from :url->action:   'find-w3m
		   :remote:        'brw
		   :local:         'brwl
		   :dired:         'brwd)
(eeurl-define-from :url->action:   'eepsne
		   :remote:        'brp)
(eeurl-define-from :url->action:   'eetmpwget
		   :remote:        'brtmpwget)


;; (find-efile "net/browse-url.el" "defun browse-url-firefox")
;; Delete this?
;;
;; (defun find-firefox (url &optional rest)
;;    (interactive "sURL: ")
;;    (start-process "firefox" "*Messages*" "firefox" url)
;;    url)
;;
;; (eeurl-define-from :url->action:   'find-firefox
;;                    :remote:        'brm
;;                    :local:         'brml
;;                    :dired:         'brmd)

;; http://angg.twu.net/eev-article.html#local-copies
(defun eepsne (url &rest ignore)
  (interactive (browse-url-interactive-arg "psne "))
  (eev (format "psne '%s'" url)))
 
(defun eetmpwget (url &rest ignore)
  (interactive (browse-url-interactive-arg "cd /tmp; wget "))
  (eev (concat "cd /tmp\nwget " url)))



;;;
;;; New functions added in 2007dec21.
;;; This file was already a mess, and I need these
;;; functions in eev to exchange code with a friend -
;;;
;; «find-wget»  (to ".find-wget")
;; «brwget»  (to ".brwget")
;;
(defun find-wget00 (url)
  (find-callprocess00 `("wget" "-q" "-O" "-" ,url)))

(defun find-wget (url &rest rest)
  (setq url (ee-expand url))
  (apply 'find-eoutput-reuse (format "*wget: %s*" url)
	 `(insert (find-wget00 ,url))
	 rest))

(eeurl-define-from :url->action: 'find-wget
		   :remote:      'brwget)

;; «ee-cp»  (to ".ee-cp")
;; http://article.gmane.org/gmane.emacs.bugs/17178
;;
(defun ee-cp (from to &optional ok-flag)
  (require 'dired)
  (let ((tramp-verbose 0))
    (dired-copy-file (ee-expand from) (ee-expand to) ok-flag)))

;; «find-psne-links»  (to ".find-psne-links")
;; «brep»  (to ".brep")
;; Tests:
;; (ee-psne-wget-lines "http://angg.twu.net/index.html")
;;    (find-psne-links "http://angg.twu.net/index.html")
;;               (brep "http://angg.twu.net/index.html")
;;
(defun ee-psne-wget-lines (url)
  (let* ((localurl (replace-regexp-in-string
		    "^\\(https?\\|ftp\\)://" "$S/\\1/" url))
	 (localdir (file-name-directory localurl)))
    (list (format "mkdir -p %s" localdir)
	  (format "cd       %s" localdir)
	  (format "wget     '%s'" url)
	  (format "echo     '%s' >> ~/.psne.log" url))))

(defun find-psne-links (url &rest rest)
  (find-elinks `(
    (find-psne-links ,url ,@rest)
    ;; (find-eev "eev-browse-url.el" "brep")
    ,(ee-addhp url)
    nil
    " (eepitch-shell)"
    ,@(ee-psne-wget-lines url)
    )))

(eeurl-define-from :url->action: 'find-psne-links
                   :remote:      'brep)




(provide 'eev-browse-url)


;; (eval-buffer)

;; Ooops, this block of notes is about how I'm planning to make an
;; intro to eev using lots of screenshots, like dto did for org-mode...
;; (find-angg "bin/Xscreenshot-window")
;; http://angg.twu.net/bin/Xscreenshot-window.html
;; Oops, Mod4-w is not yet bound by default...
;; (find-angg ".fvwm/keys.fvwm")
;; http://angg.twu.net/.fvwm/keys.fvwm.html
;;
;; The parts of eev
;; ================
;; Hyperlinks
;; Hyperlink generators
;;   Temporary buffers
;;   code-c-d, find-code-c-d
;;   dff
;; Sending regions
;;   bounded regions
;; The steppers
;; Glyphs
;; Help tools
;;
;; Auxiliars
;; =========
;; The installer
;; The snarfer
;; browse-url and friends
;; hippie-expand
;;
;; file:///home/edrx/TH/L/eev-article.html

;; Old notes, random crap.
;;
;; (find-efunction 'find-w3m)
;; (find-fline "~/TH/L/")
;; (progn (find-fline "~/TH/L/") (find-w3m "01jul14.html"))


;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; ee-anchor-format:  "«%s»"
;; ee-comment-prefix: ";;"
;; no-byte-compile:   t
;; End:
