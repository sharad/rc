;; eepitch.el - record interactions with shells as readable notes, redo tasks.
;; Source:        <http://angg.twu.net/eev-current/eepitch.el>
;; Htmlized:      <http://angg.twu.net/eev-current/eepitch.el.html>
;; Documentation: <http://angg.twu.net/eev-current/eepitch.readme.html>
;; Author: Eduardo Ochs <eduardoochs@gmail.com>.
;; Version: 2012may04
;; License: GPL3.
;;
;; This implements a much simpler way to interact with external
;; programs than the one shown in:
;;   <http://angg.twu.net/eev-current/anim/channels.anim.html>
;;
;;
;; Quick start guide
;; =================
;; Read the first sections of
;;   <http://angg.twu.net/eev-current/eepitch.readme.html>
;; then load this file, with something like:
;;   (load-file "eepitch.el")
;; Then in the '( ... ) block below type M-T on the `shell' line to
;; convert it to an "eepitch block", then use `F8's to execute the
;; three red-star lines, then use more `F8's to send the "cd /tmp/"
;; and the "ls" to the shell buffer.
'(

shell
cd /tmp/
ls

)
;; Note that as eepitch.el is still a bit prototype-ish we set two
;; keybindings and a glyph GLOBALLY - search for "set-glyph" and
;; "global-set-key" below.
;;
;;
;; If you are interested in eev
;; ============================
;; The current recommended way to install eev is through the Debian
;; package - see:
;;   <http://angg.twu.net/debian/README.html>
;; But this version can be used independently of the rest of eev.
;;
;;
;; My TODO list for eepitch (short and medium term)
;; ================================================
;; The current priorities for eepitch are:
;;   1) Debian packages. There's a quick-and-dirty package, called
;;      "eev-puro", that depends on the Debian package for eev and
;;      that installs some extra demo scripts; a bunch of students
;;      from my university are using that to learn Lua (and *NIX). Its
;;      docs are mostly in Portuguese, and they can be found at:
;;        <http://angg.twu.net/eev-puro/>
;;        <http://angg.twu.net/eev-puro/debian/README.Debian.html>
;;      The package itself is here:
;;        <http://angg.twu.net/debian/>
;;      The important thing is that new versions of that .deb are
;;      built and uploaded to angg.twu.net by templates similar to the
;;      ones in:
;;        <http://angg.twu.net/eev-current/eev-template.el.html>
;;      These templates need to cleaned up and adapted to generate the
;;      Debian packages for eev too (and for dednat5, and for
;;      blogme4).
;;   2) The support for GUD, SLIME and multi-window settings in
;;      general is quite primitive at the moment. Note also that here
;;      I do not use any of the "inferior <prog> mode" modes of Emacs.
;;      That's just because I never learn hot to use them. 8-(
;;   3) The docs in plain text format for eepitch, that are at
;;        <http://angg.twu.net/eev-current/eepitch.readme.html>
;;      are ok for the basic ideas but horribly incomplete on
;;      everything more advanced.
;;   4) I haven't touched this in years:
;;        <http://angg.twu.net/eev-article.html>
;;      It has several good parts, I would like to salvage it.
;;   5) All these docs should be converted to texinfo, possibly
;;      using blogme4, as in (but this is just a prototype):
;;        <http://angg.twu.net/blogme4/doc/>
;;   6) Understand packages with similar goals and write comparisons:
;;      eepitch with org-babel and org-babel-screen, and eev-puro with
;;      emacs-starter-kit.
;;   7) Produce short videos about eepitch, like:
;;        <http://angg.twu.net/eev-current/anim/channels.anim.html>
;;      note that Org has lots of videos, e.g.:
;;        <http://www.youtube.com/watch?v=oJTwQvgfgMM> Carsten Dominik
;;        <http://www.youtube.com/watch?v=ht4JtEbFtFI> \ Kurt Schwehr on
;;        <http://vislab-ccom.unh.edu/~schwehr/rt/>    / org-babel
;;
;;
;; The innards
;; ===========
;; In order to understand precisely how eepitch works (consider this a
;; preliminary hacker's guide!), let's make some definitions and
;; follow a low-level example. I will suppose that you have read
;; enough of
;;   <http://angg.twu.net/eev-current/eepitch.readme.html>
;; to understand how to use eepitch in the most basic cases.
;;
;; Some sexps, like `(shell)', always switch to a buffer with a
;; certain name when executed, and they create a buffer with that name
;; when it does not exist. We call that name the "target buffer name"
;; of the sexp, and, by convention, the cases in which the sexp raises
;; an error do not count. So, for example,
;;
;;   sexp              target buffer name
;;   ----------        ------------------
;;   (shell)           "*shell*"
;;   (shell "foo")     "foo"
;;   (info "(emacs)")  "*info*"
;;   (+ 1 2)           none/undefined
;;   (error)           none/undefined
;;
;; A "shell-like sexp" is one that has a target buffer name. So
;; `(shell)' and `(info "(emacs)")' are shell-like sexps, but `(+ 1
;; 2)' is not.
;;
;; Now consider the two Emacs frames below: we start with
;;                                 ___
;;    ______________emacs_________|-|X|
;;   |                                 |  \
;;   |  (eepitch '(shell))_           |  | We will call this the
;;   | cd /tmp/                        |  | "e-script window".
;;   | ls                              |  | The point is at the "_".
;;   |                                 |  | We will type F8 three times.
;;   |                                 |  |
;;   |                                 |  |
;;   |                                 |  |
;;   |                                 |  /
;;   |--:** NOTES  (Fundamental) ------|  <-- Its modeline.
;;   |_________________________________|  <-- The minibuffer.
;;
;; then we type F8 three times, and we get:
;;                                 ___
;;    ______________emacs_________|-|X|
;;   |                                 |  \
;;   |  (eepitch '(shell))            |  | Let's call this the
;;   | cd /tmp/                        |  | "e-script window".
;;   | ls                              |  | The point is at the "_".
;;   | _                               |  | We just typed F8 three times.
;;   |                                 |  /
;;   |--:** NOTES  (Fundamental) ------|  <-- Its modeline.
;;   |                                 |  \
;;   | /home/edrx# cd /tmp/            |  | Let's call this the
;;   | /tmp# ls                        |  | "target window".
;;   | ./  ../  foo.txt                |  |
;;   | /tmp#                           |  |
;;   |                                 |  /
;;   |--:** *shell*  (Shell:run) ------|  <-- Its modeline.
;;   |_________________________________|  <-- The minibuffer.
;;
;; When we typed F8 on the line " (eepitch '(shell))" the system
;; became "prepared". More precisely:
;;   a) `eepitch-code' was set to `(shell)',
;;   b) `eepitch-buffer-name' was set to the string "*shell*",
;;   c) the buffer "*shell*" was displayed in another window.
;; The actions (b) and (c) were performed by the function
;; `eepitch-prepare'.
;;
;; When we typed F8 on the line "cd /tmp/" the string "cd /tmp/" was
;; "pitched" to the target window, by `(eepitch-line "cd /tmp/")'.
;; Same for the "ls" in the next line. But before pitching each line
;; `eepitch-prepare' was run to make sure that a target window exists.
;;
;; We need more definitions. We say that the system is "(at least)
;; half-prepared" when:
;;   1) `eepitch-buffer-name' holds the target buffer name of the sexp
;;      in `eepitch-code',
;;   2) a buffer with name `eepitch-buffer-name' exists,
;; and we say that the system is "prepared" when (1) and (2) hold,
;; and, besides that,
;;   3) the current buffer's name is not `eepitch-buffer-name', and
;;   4) there is a window - that we will call the "target window" -
;;      showing the buffer `eepitch-buffer-name'.
;;
;; In the code below,
;;   `eepitch-buffer-create' takes care of conditions 1 and 2.
;;   `eepitch-assert-not-target', of condition 3.
;;   `eepitch-window-show', of condition 4 (supposing 1, 2, 3 hold).
;;   `eepitch-prepare', of all the conditions 1-4.


(defun ee-bol () (point-at-bol))
(defun ee-eol () (point-at-eol))
(defun ee-eval-string (str)
  "Wrap STR in a progn then read it and eval it.
Examples: (ee-eval-string \"(+ 1 2) (* 3 4) ;; this returns 12=3*4\")
          (ee-eval-string \";; this returns nil\")"
  (eval (read (concat "(progn\n" str "\n)"))))

(defun ee-next-line (&optional arg try-vscroll)
"Line `next-line', but ignoring visual line mode.
This function is used by `eepitch-this-line'."
  (let ((line-move-visual nil))
    (next-line arg try-vscroll)))

(defun eek (str)
  "Execute STR as a keyboard macro. See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (execute-kbd-macro (read-kbd-macro str)))



;;;                     
;;;   ___ ___  _ __ ___ 
;;;  / __/ _ \| '__/ _ \
;;; | (_| (_) | | |  __/
;;;  \___\___/|_|  \___|
;;;                     

(defvar eepitch-regexp "^\\(.*\\)"
"The regexp used by `eepitch-this-line' to determine what is a red-star line.
Red star lines are evaluated as lisp, other lines are pitched to
the target buffer.")

(defvar eepitch-buffer-name ""
  "The name of the target buffer for eepitch.
Set this to \"\" to force running `eepitch-buffer-create' again.
Note that `eepitch-buffer-create' sets this variable!")

(defvar eepitch-code '(error "eepitch not set up")
  "The code to create and switch to the target buffer.")

(defvar eepitch-window-show    '(eepitch-window-show))  ; cheap indirection
(defvar eepitch-kill           '(eepitch-kill-buffer))  ; cheap indirection
(defvar eepitch-kill-windows   'nil)	                ; cheap indirection
(defun eepitch-buffer-exists () (get-buffer        eepitch-buffer-name))
(defun eepitch-window-exists () (get-buffer-window eepitch-buffer-name))
(defun eepitch-target-buffer () (get-buffer        eepitch-buffer-name))
(defun eepitch-target-window () (get-buffer-window eepitch-buffer-name))
(defun eepitch-target-here () (eq (current-buffer) (eepitch-target-buffer)))

(defun eepitch-buffer-create ()
  "Eval the sexp in `eepitch-code' and set `eepitch-buffer-name'.
This is done without disturbing the current window configuration.\n
Remember that we say that \"the system is (at least) half-prepared\" when:
  1) `eepitch-buffer-name' holds the target buffer name of the sexp
     in `eepitch-code',
  2) a buffer with name `eepitch-buffer-name' exists.\n
This function makes sure that the system is at least half-prepared.
See `eepitch' and `eepitch-prepare'."
  (save-window-excursion
    (eval eepitch-code)
    (setq eepitch-buffer-name
	  (buffer-name (current-buffer)))))

(defun eepitch-window-show ()
  "Display the buffer `eepitch-buffer-name' in another window.
This is just the default way of making sure that the \"target
window\" is visible; note that `eepitch' sets the variable
`eepitch-window-show' to `(eepitch-window-show)', and that
`eepitch-prepare' evaluates the sexp in the variable
`eepitch-window-show'. Alternative eepitch settings - like the
ones for GUD or Slime, that use multiple windows - put calls to
other functions instead of this one in the variable
`eepitch-window-show'.\n
This function uses `display-buffer', which calls
`split-window-sensibly'."
  (let ((pop-up-windows t)
	(same-window-buffer-names nil))
    (display-buffer eepitch-buffer-name)))

(defun eepitch-prepare ()
"If the eepitch buffer does not exist, create it; if it is not shown, show it.
In eepitch's terminology we say that the system is \"prepared\" when:
  1) the variable `eepitch-buffer-name' holds the target buffer
     name of the sexp in `eepitch-code',
  2) a buffer with name `eepitch-buffer-name' exists,
  3) the current buffer's name is not `eepitch-buffer-name', and
  4) there is a window - that we will call the \"target window\" -
     showing the buffer `eepitch-buffer-name'.
This function makes sure that the system is prepared. Note that
this function is called from both `eepitch' and
`eepitch-this-line'."
  (if (not (eepitch-buffer-exists))
      (eepitch-buffer-create))
  (if (eq (current-buffer) (eepitch-target-buffer))
      (error "Can't pitch to the current buffer"))
  (if (not (eepitch-window-exists))
      (eval eepitch-window-show)))

(defun eepitch (code)
"Set up a target for eepitch and make sure it is displayed in another window.
The argument CODE must be a \"shell-like sexp\", i.e., one that
when evaluated always switches to a buffer with a fixed name, and
when that buffer does not exists it creates it.\n
This function sets `eepitch-code' to CODE and sets the variables
`eepitch-window-show' and `eepitch-kill' to defaults that are
good for two-window settings, and then calls `eepitch-prepare',
which does all the hard work."
  (setq eepitch-code code)
  (setq eepitch-buffer-name "")	; so that `eepitch-buffer-exists' will fail
  (setq eepitch-window-show	; set the way to set up windows to the
      '(eepitch-window-show))   ; default two-window setting
  (setq eepitch-kill		; set the behavior of `eepitch-kill'
      '(eepitch-kill-buffer))   ; to just kill the target buffer
  (eepitch-prepare))

(defun eepitch-line (line)
  "Send LINE to the target window and run the key binding for RET there.
This is a low-level function used by `eepitch-this-line'."
  (save-selected-window
    (select-window (eepitch-target-window))
    (insert line)                              ; "type" the line
    (call-interactively (key-binding "\r"))))  ; then do a RET

(defun eepitch-this-line ()
"Pitch this line to the target buffer, or eval it as lisp if it starts with `'.
See `eepitch', `eepitch-regexp', and the source code."
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match eepitch-regexp line)           ; a line with a red star
        (ee-eval-string (match-string 1 line))       ; are eval'ed, but
      (eepitch-prepare)		   ; for other lines prepare the target buffer
      (eepitch-line line)))	   ; and pitch the line to the target.
  (ee-next-line 1))




;;;                  _ _       _           _    _ _ _ 
;;;   ___  ___ _ __ (_) |_ ___| |__       | | _(_) | |
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____| |/ / | | |
;;; |  __/  __/ |_) | | || (__| | | |_____|   <| | | |
;;;  \___|\___| .__/|_|\__\___|_| |_|     |_|\_\_|_|_|
;;;           |_|                                     

(defun ee-kill-buffer (buffer)
  "Kill BUFFER if it exists, asking for fewer confirmations than usual."
  (if (get-buffer buffer)
      (let ((kill-buffer-query-functions nil))
	(kill-buffer buffer))))

(defun eepitch-kill-buffer ()
  "Kill the eepitch target buffer if it exists, avoiding most warnings.
This function does not change the current window configuration,
and is the default behavior for `eepitch-kill' in two-window
settings. See `eepitch' and `eepitch-kill'."
  (if (eepitch-buffer-exists)
      (if (eepitch-target-here)
	  (error "Can't kill this")
	(ee-kill-buffer eepitch-buffer-name) ; kill with few warnings
	)))

(defun eepitch-kill ()
  "Kill the current eepitch target buffer in the default way.
The default is always the one stored in the variable
`eepitch-kill', and is usually `eepitch-kill-buffer'.

A common idiom - called an \"eepitch block\"; see `eepitch-wrap'
for a quick way to create eepitch blocks - is to use three
red-star lines in sequence to \"recreate the target\", like this:

 (eepitch-shell)
 (eepitch-kill)
 (eepitch-shell)

When we run the first `(eepitch-shell)' the eepitch target buffer
becomes the buffer \"*shell*\"; then we run the `(eepitch-kill)'
and we are sure that it will kill the buffer \"*shell*\", not
something else; then we run the last `(eepitch-shell)', and as
the eepitch target buffer does not exist it is recreated from
scratch."
  (eval eepitch-kill))




;;;                  _ _       _               _          _ _ 
;;;   ___  ___ _ __ (_) |_ ___| |__        ___| |__   ___| | |
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____/ __| '_ \ / _ \ | |
;;; |  __/  __/ |_) | | || (__| | | |_____\__ \ | | |  __/ | |
;;;  \___|\___| .__/|_|\__\___|_| |_|     |___/_| |_|\___|_|_|
;;;           |_|                                             

(defun eepitch-shell ()
  "Same as (eepitch '(shell)). See `eepitch' and `eepitch-wrap'."
  (interactive)
  (eepitch '(shell)))

(defun eepitch-shell2  () (interactive) (eepitch '(shell "*shell 2*")))
(defun eepitch-eshell  () (interactive) (eepitch '(eshell)))



;;;                      _       _   
;;;   ___ ___  _ __ ___ (_)_ __ | |_ 
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|
;;; | (_| (_) | | | | | | | | | | |_ 
;;;  \___\___/|_| |_| |_|_|_| |_|\__|
;;;                                  

(defun ee-expand (fname)
"Expand \"~\"s and \"$ENVVAR\"s in file names, but only at the beginning of the string."
  (cond ((string-match "^\\$\\([A-Za-z_][0-9A-Za-z_]*\\)\\(.*\\)" fname)
	 (concat (getenv (match-string 1 fname))
		 (match-string 2 fname)))
	((string-match "^\\(~\\([a-z][0-9a-z_]*\\)?\\)\\(/.*\\)?$" fname)
	 (concat (expand-file-name (match-string 1 fname))
		 (match-string 3 fname)))
	(t fname)))

(defun ee-split (str) (if (stringp str) (split-string str "[ \t\n]+") str))
(defun ee-split-and-expand (str) (mapcar 'ee-expand (ee-split str)))

(defun find-comintprocess-ne (name program-and-args)
  (let ((argv (ee-split program-and-args)))
    (apply 'make-comint name (car argv) nil (cdr argv))
    (switch-to-buffer (format "*%s*" name))))

(defun find-comintprocess (name program-and-args)
  (find-comintprocess-ne   name (ee-split-and-expand program-and-args)))

(defun eepitch-comint (name program-and-args)
"Set `eepitch' to run PROGRAM-AND-ARGS in comint mode, in the buffer \"*NAME*\"."
  (eepitch `(find-comintprocess ,name ',program-and-args)))




;;;        _             _         
;;;   __ _| |_   _ _ __ | |__  ___ 
;;;  / _` | | | | | '_ \| '_ \/ __|
;;; | (_| | | |_| | |_) | | | \__ \
;;;  \__, |_|\__, | .__/|_| |_|___/
;;;  |___/   |___/|_|              
;;;
;; (find-eev "eev-glyphs.el")
;; (find-eevarticlesection "glyphs")

(defface eepitch-star-face
  '((t (:foreground "red")))
  "Face used for the red star glyph (char 15).")

(defun eepitch-set-glyph (pos &optional char face)
  (aset standard-display-table pos
	(if char (vector (make-glyph-code char face)))))




;;;                           
;;; __      ___ __ __ _ _ __  
;;; \ \ /\ / / '__/ _` | '_ \ 
;;;  \ V  V /| | | (_| | |_) |
;;;   \_/\_/ |_|  \__,_| .__/ 
;;;                    |_|    
;;
;; Note: there are many more functions like `eepitch-wrap' in the file
;; eev-template.el - for example:
;;   http://angg.twu.net/eev-current/eev-template.el.html#ee-wrap-file
;;   http://angg.twu.net/eev-current/eev-template.el.html#ee-wrap-man
;; even `eepitch-wrap' is reimplemented there, in a way that is much
;; harder to read...
;;   http://angg.twu.net/eev-current/eev-template.el.html#ee-wrap-eepitch

(defun ee-no-properties (str)
  (setq str (copy-sequence str))
  (set-text-properties 0 (length str) nil str)
  str)

(defun eepitch-delete-and-extract-line ()
  (delete-and-extract-region (ee-bol) (ee-eol)))

(defun eepitch-wrap () (interactive)
  (let* ((fmt   " (eepitch-%s)\n (eepitch-kill)\n (eepitch-%s)")
	 (li    (eepitch-delete-and-extract-line))
	 (newli (format fmt li li)))
    (insert newli))
  (ee-next-line 1))



;;;           _           _             _         
;;;  ___  ___| |_    __ _| |_   _ _ __ | |__  ___ 
;;; / __|/ _ \ __|  / _` | | | | | '_ \| '_ \/ __|
;;; \__ \  __/ |_  | (_| | | |_| | |_) | | | \__ \
;;; |___/\___|\__|  \__, |_|\__, | .__/|_| |_|___/
;;;                 |___/   |___/|_|              

(if (not standard-display-table)
    (setq standard-display-table (make-display-table)))
(eepitch-set-glyph ?\^O ?* 'eepitch-star-face)


;;;           _     _                  
;;;  ___  ___| |_  | | _____ _   _ ___ 
;;; / __|/ _ \ __| | |/ / _ \ | | / __|
;;; \__ \  __/ |_  |   <  __/ |_| \__ \
;;; |___/\___|\__| |_|\_\___|\__, |___/
;;;                          |___/     

(global-set-key [f8]   'eepitch-this-line)
(global-set-key "\M-T" 'eepitch-wrap)

(provide 'eepitch)


;;;  _____           _          __   _   _                               
;;; | ____|_ __   __| |   ___  / _| | |_| |__   ___    ___ ___  _ __ ___ 
;;; |  _| | '_ \ / _` |  / _ \| |_  | __| '_ \ / _ \  / __/ _ \| '__/ _ \
;;; | |___| | | | (_| | | (_) |  _| | |_| | | |  __/ | (_| (_) | | |  __/
;;; |_____|_| |_|\__,_|  \___/|_|    \__|_| |_|\___|  \___\___/|_|  \___|
;;;                                                                      
;;; ----------------------------------------------------------------------
;;; ----------------------------------------------------------------------
;;; ----------------------------------------------------------------------




;;;                      _       _                  _           
;;;   ___ ___  _ __ ___ (_)_ __ | |_       ___  ___| |__   ___  
;;;  / __/ _ \| '_ ` _ \| | '_ \| __|____ / _ \/ __| '_ \ / _ \ 
;;; | (_| (_) | | | | | | | | | | ||_____|  __/ (__| | | | (_) |
;;;  \___\___/|_| |_| |_|_|_| |_|\__|     \___|\___|_| |_|\___/ 
;;;                                                             
;; What is this: I am trying to find an elegant way to deal with
;; programs that echo their input (like zsh)... This is still a bit
;; experimental!
;; See: (find-variable 'comint-process-echoes)
;; To do: send an e-mail to Olin Shivers about echoing and stty.

(defun at-eepitch-target (code)
  (eepitch-prepare)
  (save-selected-window
    (select-window (eepitch-target-window))
    (eval code)))

(defun del-echo (flag)
"A hack to help determining whether a program echoes its commands or not.
An example of use:\n
 (eepitch-zsh)
 (eepitch-kill)
 (eepitch-zsh)
cd /tmp/
 (del-echo t)
cd /tmp/
 (del-echo nil)
cd /tmp/\n"
  (at-eepitch-target `(setq comint-process-echoes ,flag))
  (message "At %s: %S" eepitch-buffer-name
	   `(setq comint-process-echoes ,flag)))

(defun eepitch-de (code)
  "Like `eepitch', but deletes the echoed commands.
Use this to control programs that echo the commands that they receive."
  (eepitch `(progn ,code (setq comint-process-echoes t))))

(defun eepitch-comint-de (name program-and-args)
  "Like `eepitch-comint', but deletes the echoed commands.
Use this to control programs that echo the commands that they receive."
  (eepitch-de `(find-comintprocess ,name ',program-and-args)))



;;;   ___  _   _                 _              _     
;;;  / _ \| |_| |__   ___ _ __  | |_ ___   ___ | |___ 
;;; | | | | __| '_ \ / _ \ '__| | __/ _ \ / _ \| / __|
;;; | |_| | |_| | | |  __/ |    | || (_) | (_) | \__ \
;;;  \___/ \__|_| |_|\___|_|     \__\___/ \___/|_|___/
;;;                                                   
;; Useful for controlling certain external programs.

(defun ee-at0 (dir code)
  "Eval CODE at DIR.
If DIR does not end with a slash then weird things might happen.
Note the DIR is `ee-expand'-ed."
  (let ((default-directory (ee-expand dir)))
    (if (not (file-accessible-directory-p dir))
	(error "Can't chdir to %s" dir))
    (eval code)))

(defun eepitch-comint-at (dir name program-and-args)
  "Like `eepitch-comint', but executes `eepitch-buffer-create' at DIR."
  (ee-at0 dir `(eepitch-comint ,name ,program-and-args)))

(defun with-pager-cat (code)
  "Run CODE with the environment variable PAGER set to \"cat\".
This is useful for for running processes that use pagers like
\"more\" by default."
  (let ((process-environment (cons "PAGER=cat" process-environment)))
    (eval code)))

(defun eepitch-to-buffer (name)
  (interactive "beepitch to buffer: ")
  (eepitch `(switch-to-buffer ,name)))

(defun at-nth-window (n code)
  "Run `other-window' N times, run CODE there, and go back."
  (save-selected-window
    (other-window n)
    (eval code)))



;;;  _                                                  
;;; | |    __ _ _ __   __ _ _   _  __ _  __ _  ___  ___ 
;;; | |   / _` | '_ \ / _` | | | |/ _` |/ _` |/ _ \/ __|
;;; | |__| (_| | | | | (_| | |_| | (_| | (_| |  __/\__ \
;;; |_____\__,_|_| |_|\__, |\__,_|\__,_|\__, |\___||___/
;;;                   |___/             |___/           

;; Shells
(defun eepitch-shell  () (interactive) (eepitch '(shell)))
(defun eepitch-shell2 () (interactive) (eepitch '(shell "*shell 2*")))
(defun eepitch-eshell () (interactive) (eepitch '(eshell)))
(defun eepitch-bash () (interactive) (eepitch-comint "bash" "bash"))
(defun eepitch-dash () (interactive) (eepitch-comint "dash" "dash"))
(defun eepitch-ksh  () (interactive) (eepitch-comint "ksh" "ksh"))
(defun eepitch-tcsh () (interactive) (eepitch-comint "tcsh" "tcsh"))
(defun eepitch-zsh  () (interactive) (eepitch-comint-de "zsh" "zsh"))
(defun eepitch-scsh () (interactive) (eepitch-comint "scsh" "scsh"))

;; Main interpreted languages
(defun eepitch-lua51  () (interactive) (eepitch-comint "lua51"  "lua5.1"))
(defun eepitch-python () (interactive) (eepitch-comint "python" "python"))
(defun eepitch-ruby   () (interactive) (eepitch-comint "ruby"   "irb1.8"))
(defun eepitch-perl () (interactive) (eepitch-comint "perl" "perl -d -e 42"))

;; Tcl
(defun eepitch-tcl     () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-tclsh   () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-wish    () (interactive) (eepitch-comint "wish"    "wish"))
(defun eepitch-expect  () (interactive) (eepitch-comint "expect"  "expect"))

;; Lisps and Schemes
;; It would be better to run them in Slime.
(defun eepitch-sbcl   () (interactive) (eepitch-comint "sbcl" "sbcl"))
(defun eepitch-gcl    () (interactive) (eepitch-comint "gcl"  "gcl"))
(defun eepitch-guile  () (interactive) (eepitch-comint "guile" "guile"))
(defun eepitch-mitscheme () (interactive)
  (eepitch-comint "mit-scheme" "mit-scheme"))
(defun eepitch-tinyscheme () (interactive)
  (eepitch-comint "tinyscheme" "tinyscheme"))

;; Haskell, ML, Erlang, Coq
(defun eepitch-hugs   () (interactive) (eepitch-comint "hugs" "hugs"))
(defun eepitch-hugs98 () (interactive) (eepitch-comint "hugs" "hugs -98"))
(defun eepitch-ghci   () (interactive) (eepitch-comint "ghci" "ghci"))
(defun eepitch-ocaml  () (interactive) (eepitch-comint "ocaml" "ocaml"))
(defun eepitch-labltk () (interactive) (eepitch-comint "labltk" "labltk"))
(defun eepitch-polyml () (interactive) (eepitch-comint "polyml" "poly"))
(defun eepitch-erl    () (interactive) (eepitch-comint "erl" "erl"))
(defun eepitch-coqtop () (interactive) (eepitch-comint "coqtop" "coqtop"))

;; Forth
(defun eepitch-gforth () (interactive) (eepitch '(run-forth "gforth")))
(defun eepitch-gforth () (interactive) (eepitch-comint "gforth" "gforth"))
(defun eepitch-pforth () (interactive) (eepitch-comint "pforth" "pforth"))
(defun eepitch-yforth () (interactive) (eepitch-comint "yforth" "yforth"))

;; Mathematics
(defun eepitch-maxima () (interactive) (eepitch-comint "maxima" "maxima"))
(defun eepitch-octave () (interactive) (eepitch-comint "octave" "octave"))
(defun eepitch-R () (interactive)
  (eepitch '(with-pager-cat (find-comintprocess "R" "R"))))

;; Plotters.
;; We force GhostScript's resolution to make its window fit on the screen.
(defun eepitch-gs () (interactive) (eepitch-comint "gs" "gs -r45"))
(defun eepitch-gs () (interactive) (eepitch-comint "gs" "gs -r60"))
(defun eepitch-gnuplot () (interactive) (eepitch-comint "gnuplot" "gnuplot"))

;; Java-based languages
(defun eepitch-bsh () (interactive)
  (eepitch-de '(find-comintprocess "bsh" "bsh")))
(defun eepitch-scala () (interactive)
  (eepitch '(find-comintprocess "scala" "scala")))
(defun eepitch-clojure () (interactive)
  (eepitch '(find-comintprocess "clojure" "clojure -r")))

;; SQL. To do: add postgres and sqlite
(defun eepitch-mysql () (interactive)
  (eepitch '(with-pager-cat '(find-comintprocess "mysql" "mysql -u root"))))

;; SmallTalk
(defun eepitch-gst () (interactive)
  (eepitch '(find-comintprocess "gst" "gst")))

;; JavaScript
;; MozRepl is a Javascript REPL in a running Mozilla browser.
;; See: https://github.com/bard/mozrepl/wiki/tutorial
(defun eepitch-smjs () (interactive) (eepitch-comint "smjs" "smjs"))
(defun eepitch-mozrepl () (interactive)
  (eepitch-comint "mozrepl" "telnet localhost 4242"))

;; Programs from the TeX family.
;; They create logfiles in the current dir, so we run them in /tmp/.
(defun eepitch-luatex () (interactive)
  (eepitch-comint-at "/tmp/" "luatex" "luatex"))
(defun eepitch-lualatex () (interactive)
  (eepitch-comint-at "/tmp/" "lualatex" "lualatex"))
(defun eepitch-latex () (interactive)
  (eepitch-comint-at "/tmp/" "latex" "latex"))
(defun eepitch-tex   () (interactive)
  (eepitch-comint-at "/tmp/" "tex"   "tex"))
(defun eepitch-mf    () (interactive)
  (eepitch-comint-at "/tmp/" "mf"   "mf"))
(defun eepitch-mpost () (interactive)
  (eepitch-comint-at "/tmp/" "mpost" "mpost"))

;; Pulseaudio (this is to interact with its daemon)
(defun eepitch-pacmd () (interactive) (eepitch-comint "pacmd" "pacmd"))




;;;           _          
;;;  _ __ ___(_)_ __ ___ 
;;; | '__/ __| | '__/ __|
;;; | | | (__| | | | (__ 
;;; |_|  \___|_|_|  \___|
;;;                      
;; When we run (rcirc-connect "irc.freenode.net" ...)
;; and the buffer "*irc.freenode.net*" already exists, `rcirc-connect'
;; does nasty things; so it's better to run
;;   (eepitch '(ee-rcirc-connect))
;; instead.
;;
;; Actually we want to do another trick too. Killing the buffer
;; "*irc.freenode.net*" is too expensive, as reconnection takes about
;; 10 seconds; so we set `eepitch-kill' to something different from
;; the default, which is `(eepitch-kill-buffer)'.

(defun ee-rcirc-serverbuf  (server)         (format "*%s*" server))
(defun ee-rcirc-channelbuf (server channel) (format "%s@%s" channel server))
(defun ee-rcirc-connected  (server)
  (and (get-buffer           (ee-rcirc-serverbuf server))
       (rcirc-buffer-process (ee-rcirc-serverbuf server))))

(defun ee-rcirc-connect    (server channels)
  "Connect to an irc server (if not already connected).
TODO: if we are already connected to SERVER, just connect to CHANNELS."
  (if (not (ee-rcirc-connected server))
      (rcirc-connect server nil nil nil nil channels))
  (switch-to-buffer (ee-rcirc-serverbuf server)))

(defun ee-rcirc-sexp (server channel)
  `(find-ebuffer ,(ee-rcirc-channelbuf server channel)))

(defun eepitch-kill-rcirc  (server)
  (message "Not killing: %S" (ee-rcirc-serverbuf server)))

(defun eepitch-rcirc-server (server channels)
  "Connect to the irc server SERVER if not already connected, and to CHANNELS."
  (interactive)
  (eepitch `(ee-rcirc-connect ,server ',channels))
  (setq eepitch-kill `(eepitch-kill-rcirc ,server))
  (ee-rcirc-sexp server (car channels))) ; easter egg (use M-1 C-x C-e)

(defun eepitch-freenode (&optional channels) (interactive)
  (eepitch-rcirc-server "irc.freenode.net" (or channels '("#eev"))))

(defun eepitch-ircgnome (&optional channels) (interactive)
  (eepitch-rcirc-server "irc.gnome.org"    (or channels '("#docs"))))


  






;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
