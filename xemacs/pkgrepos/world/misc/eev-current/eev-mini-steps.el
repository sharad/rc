;;; eev-steps-mini.el -- minimal versions of the four steppers from eev.

;; Copyright (C) 2006,2007,2008 Free Software Foundation, Inc.
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
;; Version:    2008jul07
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-steps-mini.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-steps-mini.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;;; Commentary:

;; This file will be merged with "eev-steps.el" at some point,
;; hopefully soon... "eev-steps.el" is much more documented than this,
;; but the code there is a mess; here the names are better, and the
;; functions are much better factored than there.
;;
;; Note that "eev-all.el" loads "eev-mini-steps.el" after
;; "eev-steps.el", and so the definitions from this file will override
;; the definitions from "eev-steps.el" with the same names.

;; (find-eev "eev-steps.el")
;; (find-eev "eev-all.el")
;; (find-eevsh "./eev-rctool")
;; (find-eevsh "./eev-rctool new_block_emacs")

(defun ee-next-line (&optional arg try-vscroll)
"Like `next-line', but always moves by \"buffer lines\".
See: (find-enode \"Moving Point\" \"`line-move-visual'\")\n
Note: the variable `line-move-visual' was introduced in Emacs in
2008jul11. On versions of Emacs older than that this function
behaves exactly like `next-line'."
  (let ((line-move-visual nil))
    (next-line arg try-vscroll)))




;;;            _
;;;   ___  ___| | __
;;;  / _ \/ _ \ |/ /
;;; |  __/  __/   <
;;;  \___|\___|_|\_\
;;;

(defun eek (s &optional e count)
  "Execute the string S (or the region between S and E) as a keyboard macro.
See `edmacro-mode' for the exact format.\n
An example: (eek \"C-x 4 C-h\")"
  (interactive "r")
  (execute-kbd-macro (read-kbd-macro (ee-se-to-string s e)) count))

(defun eek0 (kbmacro &optional count)
  "This is similar to `eek', but uses the low-level formats for macros.
Example: (eek \"\\C-x4\\C-h\")"
  (execute-kbd-macro kbmacro count))





;;;                 _
;;;   ___  ___  ___| |_ ___ _ __  ___
;;;  / _ \/ _ \/ __| __/ _ \ '_ \/ __|
;;; |  __/  __/\__ \ ||  __/ |_) \__ \
;;;  \___|\___||___/\__\___| .__/|___/
;;;                        |_|
;; Steppers - just one, from: (find-eev "eev-steps.el")
;; eek
;; eek0
;; eesteps

(defvar eesteps-list ())
(defvar eesteps-pos  0)

(defun eesteps (list)
  "Set the LIST of steps that `eesteps-do-step' will execute.\n
Here's an example: run\n
  (eesteps '(\"C-x b * scratch * RET   ;;; change to the buffer *scratch*\"
             \"foobar\"
             \"3*<left>\"
             (insert \"!\")))\n
then type \\[eesteps-do-step] four times.\n
Each step is either a string -- meaning a series of keys, in the
format used by `edmacro-mode' -- or a sexp to be evaluated."
  (setq eesteps-pos 0)
  (setq eesteps-list list)
  `(,(length list) steps stored - use <f12> to execute a step))

(defun eesteps-perform (step &rest rest)
  (if (stringp step)
      (eek step)
    (eval step))
  (if rest (apply 'eesteps-perform rest)))

(defun eesteps-do-step (&optional arg)
  (interactive "P")
  (if (>= eesteps-pos (length eesteps-list))
      (error "No more steps"))
  (if (eq arg 0)
      (message "Next step: %d = %S" eesteps-pos (nth eesteps-pos eesteps-list))
    (eesteps-perform (nth eesteps-pos eesteps-list))
    (setq eesteps-pos (1+ eesteps-pos))))




;;;                            _
;;;   ___  ___ _ __   __ _ ___| |_ ___
;;;  / _ \/ _ \ '_ \ / _` / __| __/ _ \
;;; |  __/  __/ |_) | (_| \__ \ ||  __/
;;;  \___|\___| .__/ \__,_|___/\__\___|
;;;           |_|

(defun eepaste-one-line ()
  "Paste (yank) the first line of the top of the kill-ring here and do a RET."
  (interactive)
  (let ((bigstr (car kill-ring)))
    (if (equal bigstr "") (error "No more lines"))
    (string-match "^\\([^\n]*\\)\\(\n\\|$\\)" bigstr)
    (let ((line (match-string 1 bigstr))           ; first line from the kill
	  (rest (substring bigstr (match-end 0)))) ; rest of the kill
      (if (string-match "^\\(.*\\)" line)         ; lines with a red star
	  (ee-eval-string (match-string 1 line))   ; are eval'ed
	(insert line)			           ; other lines are "typed"
	(call-interactively (key-binding "\r")))   ; and then we do a RET
      (setcar kill-ring rest))))		   ; remove the first line

(defun eestore (s &optional e)
  "Store the region between S and E in the kill ring."
  (kill-new (ee-se-to-string s e))
  (format "Stored in the kill-ring"))

(eeb-define 'eestore-bounded  'eestore 'ee-delimiter-hash  nil t t)




;;;                  _ _       _
;;;   ___  ___ _ __ (_) |_ ___| |__
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \
;;; |  __/  __/ |_) | | || (__| | | |
;;;  \___|\___| .__/|_|\__\___|_| |_|
;;;           |_|
;;
;; The code is quite intricate here, but maybe the diagram below will
;; help... so many functions use the variable `eepitch-target-buffer'
;; that I have not drawn arrows pointing to it.
;;
;;   <f8> ---->   eepitch-this-line
;;                    | |         \ (on "" lines)
;;        (on non-"" | |          v
;;             lines) | |           eepitch --------------
;;                    | |          /                       \(set)
;;                    | v         v                  (run)  v
;;                    | eepitch-prepare-target-buffer ---> eepitch-code
;;                    | eepitch-display-target-buffer
;;                    | eepitch-not-this-buffer
;;                    v
;;                 eepitch-line
;;                       |                         eepitch-target-buffer
;;                       v
;;                 (insert line)
;;                 (call-interactively (key-binding "\r"))

(defvar eepitch-code '(error "eepitch not set up"))
(defvar eepitch-target-buffer nil)

(defun eepitch-prepare-target-buffer ()
"Run `eepitch-code' and store the name of the resulting buffer in `eepitch-target-buffer'."
  (save-window-excursion
    (eval eepitch-code)
    (setq eepitch-target-buffer (current-buffer))))

(defun eepitch-display-target-buffer ()
  "Display the buffer `eepitch-target-buffer' in another window."
  (if (not (get-buffer-window eepitch-target-buffer))
      (let ((pop-up-windows t)
	    (same-window-buffer-names nil))
	(display-buffer eepitch-target-buffer))))

(defun eepitch (code)
"Set the target buffer for pitching lines to; CODE is something like `(shell)'.
To pitch lines to the target buffer, use `\\[eepitch-this-line]'.
As with `eechannel', lines starting with `' are executed as Lisp;
other lines are sent.

There's a (trivial) example of usage below; each line is meant to
be executed with `\\[eepitch-this-line]'. For a convenient way to
generate the three \"\" lines at once - by typing just \"shell\"
and then `\\[ee-wrap-eepitch]' - see `ee-wrap-eepitch' and
`eepitch-shell'.

 (eepitch '(shell))
 (eepitch-kill)
 (eepitch '(shell))
cd /tmp/
pwd
"
  (if (not (listp code))
      (error "eepitch is no longer a macro - quote the code!"))
  (setq eepitch-code code)
  (eepitch-prepare-target-buffer)
  (eepitch-display-target-buffer))

(defun eepitch-line (line)
  (save-selected-window
    (select-window (get-buffer-window eepitch-target-buffer))
    (insert line)                              ; "type" the line
    (call-interactively (key-binding "\r"))))  ; then do a RET

(defun eepitch-not-this-buffer ()
  (if (eq (current-buffer) eepitch-target-buffer)
      (error "Can't pitch to the current buffer")))

(defun eepitch-this-line ()
"Pitch this line to the target buffer, or eval it as lisp if it starts with `'.
See `eepitch' and the source code."
  (interactive)
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
        (ee-eval-string (match-string 1 line))       ; are eval'ed -
      (eepitch-prepare-target-buffer)	 ; for other lines reconstruct the
      (eepitch-display-target-buffer)	 ; target buffer, display it, make
      (eepitch-not-this-buffer)		 ; sure it's a different buffer, and
      (eepitch-line line)))		 ; pitch the line to the target.
  (ee-next-line 1))

(defun eepitch-kill ()
  "Kill the `eepitch-target-buffer'. See `eepitch' and the source code."
  (interactive)
  (eepitch-prepare-target-buffer)	 ; Prepare (maybe reconstruct) the
  (eepitch-display-target-buffer)	 ; target buffer, display it, make
  (eepitch-not-this-buffer)		 ; sure it's a different buffer...
  (save-selected-window			 ; Then temporarily switch to the
    (select-window (get-buffer-window eepitch-target-buffer)) ; right window,
    (ee-kill-this-buffer)))		 ; and kill the buffer that is there.




;; convenience

(defun eepitch-comint (name program-and-args)
"Set `eepitch' to run PROGRAM-AND-ARGS in comint mode, in the buffer \"*NAME*\"."
  (eepitch `(find-comintprocess ,name ',program-and-args)))

(defun eepitch-shell ()
  "Same as (eepitch '(shell)). See `eepitch' and `ee-wrap-eepitch'."
  (interactive)
  (eepitch '(shell)))

(defun eepitch-shell2  () (interactive) (eepitch '(shell "*shell 2*")))
(defun eepitch-eshell  () (interactive) (eepitch '(eshell)))
(defun eepitch-lua51   () (interactive) (eepitch-comint "lua51"   "lua51"))
(defun eepitch-python  () (interactive) (eepitch-comint "python"  "python"))
(defun eepitch-ruby    () (interactive) (eepitch-comint "ruby"    "irb1.8"))
(defun eepitch-gnuplot () (interactive) (eepitch-comint "gnuplot" "gnuplot"))
(defun eepitch-wish    () (interactive) (eepitch-comint "wish"    "wish"))
(defun eepitch-tcl     () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-tclsh   () (interactive) (eepitch-comint "tclsh"   "tclsh"))
(defun eepitch-expect  () (interactive) (eepitch-comint "expect"  "expect"))
(defun eepitch-gforth  () (interactive) (eepitch '(run-forth "gforth")))


;; Added in 2008jan10:
(defun ee-eepitch-comint (dir name program-and-args)
  "Like `eepitch-comint', but run PROGRAM-AND-ARG at the directory DIR.
DIR is expanded with `ee-expand'; the result of the expansion
must be absolute, and must end with a slash."
  (eepitch `(ee-find-comintprocess (ee-expand ,dir) ,name ',program-and-args)))

(defun eepitch-latex () (interactive) (ee-eepitch-comint "/tmp/" "latex" "latex"))
(defun eepitch-tex   () (interactive) (ee-eepitch-comint "/tmp/" "tex"   "tex"))
(defun eepitch-mf    () (interactive) (ee-eepitch-comint "/tmp/" "mf"   "mf"))



;;;                  _ _       _                     _ _
;;;   ___  ___ _ __ (_) |_ ___| |__         __ _  __| | |__
;;;  / _ \/ _ \ '_ \| | __/ __| '_ \ _____ / _` |/ _` | '_ \
;;; |  __/  __/ |_) | | || (__| | | |_____| (_| | (_| | |_) |
;;;  \___|\___| .__/|_|\__\___|_| |_|      \__, |\__,_|_.__/
;;;           |_|                          |___/
;;;
;; 2007dec14: This is immature code, but it is very useful... see:
;;   http://article.gmane.org/gmane.emacs.eev.devel/47
;;   http://lists.gnu.org/archive/html/eev/2007-07/msg00000.html
;; Layout:
;;    ______________________
;;   |           |	    |
;;   | e-script  |	    |
;;   |  buffer   |  source  |
;;   |___________|   code   |
;;   |           |  buffer  |
;;   |   gdb     |	    |
;;   |  buffer   |	    |
;;   |___________|__________|
;; This will probably make the `eegud-*' functions in "eev-langs.el"
;; obsolete very soon.
;;   (find-eev "eev-langs.el")
;;   (find-efunction 'eegud-keys-mode)
;;   http://angg.twu.net/eev-current/eev-langs.el.html#eegud-keys-mode
;; HELP NEEDED: there's a new interface for gdb that I don't know how
;; to use... see: (find-enode "GDB-UI Layout")
;; There's a screenshot of it at:
;;   http://www.gnu.org/software/emacs/tour/
;;   http://www.gnu.org/software/emacs/tour/images/gdb.png

(define-key eegud-keys-mode-map "\M-k" 'eegud-kill-this-buffer)

(defun eegud-kill-this-buffer ()
  (interactive)
  (delete-other-windows)
  (ee-kill-this-buffer))

(defvar ee-gud-escript-window nil
  "An internal variable used to simplify the code of `eepitch-gdb'.
The \"e-script window\" is the upper-left quadrant of the frame.")

(defvar ee-gud-gud-window nil
  "An internal variable used to simplify the code of `eepitch-gdb'.
The \"gud window\" is the lower-left quadrant of the frame.")

(defvar ee-gud-source-window nil
  "An internal variable used to simplify the code of `eepitch-gdb'.
The \"source window\" is the right half of the frame.")

(defun eepitch-gud-window-setup ()
  (interactive)
  (delete-other-windows)
  (split-window-horizontally)  ; left|right
  (split-window-vertically)    ; split the left side into upper/lower
  (setq ee-gud-escript-window (selected-window)) ; upper left
  (setq ee-gud-gud-window     (next-window))	 ; lower left
  (setq ee-gud-source-window  (next-window (next-window)))) ; right

(defun eepitch-gdb (buffer-name gdb-prog-and-args)
  (eepitch-gud-window-setup)
  (with-selected-window ee-gud-gud-window
    (if (get-buffer buffer-name)
	(find-ebuffer buffer-name)
      (gdb gdb-prog-and-args)
      (eegud-keys-mode 1)))
  (setq eepitch-code `(find-ebuffer ,buffer-name)))

(defun eepitch-gdb-kill (buffer-name)
  (if (get-buffer buffer-name) (kill-buffer buffer-name))
  (delete-other-windows))

'(
;; An example of usage:
;; see: (find-es "lua5" "compile-5.1.2-lua_O0")
;; http://angg.twu.net/e/lua5.e.html#compile-5.1.2-lua_O0
;; This example should be moved to somewhere else...

(defun eepitch-gdb-lua ()
  (eepitch-gdb "*gud-lua" "gdb --annotate=3 --quiet ~/usrc/lua-5.1.2/src/lua"))
(defun eepitch-gdb-lua-kill ()
  (eepitch-gdb-kill "*gud-lua"))

(defun eepitch-gdb-lua ()
  (eepitch-gdb "*gud-lua_O0*" "gdb --annotate=3 --quiet ~/usrc/lua-5.1.2/src/lua_O0"))
(defun eepitch-gdb-lua-kill ()
  (eepitch-gdb-kill "*gud-lua_O0*"))

 (eepitch-gdb-lua)
 (eepitch-gdb-lua-kill)
 (eepitch-gdb-lua)
br main
run
n

) ;; end of the example






;;;                 _                            _
;;;   ___  ___  ___| |__   __ _ _ __  _ __   ___| |
;;;  / _ \/ _ \/ __| '_ \ / _` | '_ \| '_ \ / _ \ |
;;; |  __/  __/ (__| | | | (_| | | | | | | |  __/ |
;;;  \___|\___|\___|_| |_|\__,_|_| |_|_| |_|\___|_|
;;;
;; (find-man "xterm" "-T string")
;; (find-man "xterm" "-e program [ arguments ... ]")
;; (find-eev "eegchannel")
;; (find-eev "eegchannel" "pidfile")
;; (find-eev "eegchannel" "strfile")
;;
;; There is a big diagram explaining how this works at:
;;
;;   (find-eev "anim/channels.anim")
;;
;; Note that this is a "communication diagram" - it shows which
;; programs start which other programs, and how they communicate.
;; Here is a call diagram for the lisp functions (and some
;; variables):
;;
;;   <F9> ---> eechannel-this-line
;;                    |          \ (on "" lines)
;;        (on non-"" |           v 
;;             lines) |            ee-eval-string 
;;                    v
;;             eechannel-send          
;;              |  |  |                
;;              |  |  v                  (sets)
;;              |  v eechannel-default  <------ eechannel
;;              v eechannel-strfile
;;       /---> eechannel-pid ----------> eechannel-pidfile
;;       |
;;      eechannel-kill

(defvar eechannel-default nil)

(defun eechannel-strfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.str" channel)))

(defun eechannel-pidfile (channel)
  (ee-expand (format "$EEVTMPDIR/eeg.%s.pid" channel)))

(defun eechannel-pid     (channel)
"Return the pid stored in the eeg.CHANNEL.pid file, as a string (or nil on error)."
  (let ((pidfile (eechannel-pidfile channel)))
    (if (file-exists-p pidfile)
	(ee-no-trailing-nl (ee-read-file pidfile)))))

(defun eechannel-kill (channel sig)
  "Send the signal SIG to the process listening on the channel CHANNEL."
  ;; We call "kill" to send the signal.
  (find-callprocess0 (format "kill %s %s" sig (eechannel-pid channel))))

(defun eechannel-send (channel str)
  "Send STR through channel CHANNEL (or through channel `eechannel-default')."
  (setq channel (or channel eechannel-default))
  (write-region str nil (eechannel-strfile channel))
  (find-callprocess0 (format "kill -USR1 %s" (eechannel-pid channel))))

(defun eechannel-this-line () (interactive)
  "Send the current line through the channel `eechannel-default', and go down.
If the line starts with a `' then evaluate it as lisp instead of sending it."
  (let ((line (buffer-substring (ee-bol) (ee-eol)))) ; contents of this line
    (if (string-match "^\\(.*\\)" line)             ; lines with a red star
	(ee-eval-string (match-string 1 line))       ; are eval'ed
      (eechannel-send nil (concat line "\n")))       ; other lines are sent
    (ee-next-line 1)))			             ; go down

(defun eechannel (channel)
  "Set the default channel to CHANNEL."
  (interactive "sDefault channel: ")
  (setq eechannel-default channel))




;;;                 _                                   _
;;;   ___  ___  ___| |__         __ _ ___ ___  ___ _ __| |_
;;;  / _ \/ _ \/ __| '_ \ _____ / _` / __/ __|/ _ \ '__| __|
;;; |  __/  __/ (__| | | |_____| (_| \__ \__ \  __/ |  | |_
;;;  \___|\___|\___|_| |_|      \__,_|___/___/\___|_|   \__|
;;;

(defun eechannel-pid-running-p (pid)
  "Return t if a process with pid PID is running. This is linux-specific."
  ;; I've heard the on BSDs "/proc" is optional and often disabled...
  ;; Calling "ps" every time sounds expensive, what should I do?
  (file-exists-p (format "/proc/%s" pid)))

;; The six functions below are for when we want to use eegchannel
;; directly, without calling it from an xterm (as in eexterm)...

(defun eechannel-args-ne (channel prog-and-args)
  `(,(ee-expand "$EEVDIR/eegchannel") ,channel
    ,@(ee-split prog-and-args)))

(defun eechannel-create-ne (channel prog-and-args)
  (find-bgprocess-ne (eechannel-args-ne channel prog-and-args)))

(defun eechannel-assert-ne (channel prog-and-args)
  (let ((pid (eechannel-pid channel)))
    (if (eechannel-pid-running-p (eechannel-pid channel))
	(message "Channel %s (pid %s) looks alive, reusing" channel pid)
      (eechannel-create-ne channel prog-and-args))))

(defun eechannel-args   (channel prog-and-args)
  (eechannel-args-ne   channel (ee-split-and-expand prog-and-args)))
(defun eechannel-create (channel prog-and-args)
  (eechannel-create-ne channel (ee-split-and-expand prog-and-args)))
(defun eechannel-assert (channel prog-and-args)
  (eechannel-assert-ne channel (ee-split-and-expand prog-and-args)))




;;;                 _
;;;   ___  _____  _| |_ ___ _ __ _ __ ___
;;;  / _ \/ _ \ \/ / __/ _ \ '__| '_ ` _ \
;;; |  __/  __/>  <| ||  __/ |  | | | | | |
;;;  \___|\___/_/\_\\__\___|_|  |_| |_| |_|
;;;
;; A call diagram:
;;
;;   eexterm ---------> eexterm-ne      
;;                        |    |       
;;                        |    v       
;;                        |  eechannel-pid-running-p
;;                        v             
;;   eexterm-create --> eexterm-create-ne    
;;                        |             
;;                        v             
;;   eexterm-args ----> eexterm-args-ne      
;;
;;   eexterm-kill -----> eechannel-kill

(defun eexterm-args-ne (channel prog-and-args xterm-args)
"Return a list of arguments for running a xterm listening on CHANNEL.
Try these examples:
  (eexterm-args-ne \"A\" nil nil)
  (eexterm-args-ne \"A\" '(\"ssh\" \"foo@bar\") \"-geometry 80x20\")"
  `("xterm"
    "-T" ,(format "channel %s" channel)
    ,@(ee-split xterm-args)
    "-e" ,(ee-expand "$EEVDIR/eegchannel") ,channel
    ,@(ee-split (or prog-and-args (ee-expand "$SHELL")))))

(defun eexterm-create-ne (channel prog-and-args xterm-args)
  "Start a xterm listening on CHANNEL. See `eexterm-args-ne'."
  (find-bgprocess-ne (eexterm-args-ne channel prog-and-args xterm-args)))

(defun eexterm-ne (channel prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sDefault channel: ")
  (setq eechannel-default channel)
  (if (eechannel-pid-running-p (eechannel-pid channel))
      (message "Reusing xterm at channel %s" channel)
    (eexterm-create-ne channel prog-and-args xterm-args)))

(defun eexterm-args   (channel &optional prog-and-args xterm-args)
  (eexterm-args-ne   channel (ee-split-and-expand prog-and-args) xterm-args))
(defun eexterm-create (channel &optional prog-and-args xterm-args)
  (eexterm-create-ne channel (ee-split-and-expand prog-and-args) xterm-args))
(defun eexterm        (channel &optional prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sDefault channel: ")
  (eexterm-ne        channel (ee-split-and-expand prog-and-args) xterm-args))

(defalias 'eechannel-xterm 'eexterm)

(defun eexterm-kill (&optional channel sig)
  (interactive)
  (eechannel-kill (or channel eechannel-default) (or sig "")))



'(

;; obsolete versions:
(defun eexterm-create (channel &optional prog-and-args xterm-args)
  "Create an xterm listening on CHANNEL."
  (interactive "sChannel: ")
  (find-bgprocess-ne
   `("xterm"
     "-T" ,(format "channel %s" channel)
     ,@xterm-args
     "-e" ,(ee-expand "$EEVDIR/eegchannel") ,channel
     ,@(or prog-and-args (list (ee-expand "$SHELL"))))))

(defun eexterm (channel &optional prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sDefault channel: ")
  (setq eechannel-default channel)
  (if (eechannel-pid-running-p (eechannel-pid channel))
      (message "Reusing xterm at channel %s" channel)
    (eexterm-create channel prog-and-args xterm-args)))

)






;; extras - cannibalize and discard the rest

;; ...
;; CODE is typically a hyperlink to a comint buffer, like in:
;;   (eepitch (shell))
;; or in:

;; (find-efunction 'eepitch-this-line)
;; (find-efunction 'eepitch)

'(

(defun eechannel-running-p (channel)
  "Returns t if there is a process listening on CHANNEL."
  (let ((pid (eechannel-pid channel)))
    (if pid (ee-pid-running-p pid))))

(defun eech (s &optional e)		; bad name?
  (interactive "r")
  (eechannel-send eechannel-default (ee-se-to-string s e)))

(eeb-define 'eech-bounded 'eech 'ee-delimiter-hash nil t t)

(defun eechannel-xterm (channel &optional prog-and-args xterm-args)
"Set the default channel to CHANNEL; create an xterm listening on CHANNEL if needed."
  (interactive "sChannel: ")
  (eechannel channel)
  (if (eechannel-running-p channel)
      (message "Reusing channel %s" channel)
    (eebg-channel-xterm channel prog-and-args xterm-args)))

(defun eechannel-kill (channel)
  "Kill the process associated to channel CHANNEL."
  (find-sh0 (format "kill -9 $(cat %s)" (eechannel-pidfile channel))))

)



(provide 'eev-mini-steps)


;; Local Variables:
;; coding:           raw-text-unix
;; ee-anchor-format: "defun %s "
;; no-byte-compile:  t
;; End:
