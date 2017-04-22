;; eev-multiwindow.el - hacks for using eepitch with multi-window
;; setups (where by "multi" we means "more than two").
;;
;; Source:        <http://angg.twu.net/eev-current/eev-multiwindow.el>
;; Htmlized:      <http://angg.twu.net/eev-current/eev-multiwindow.el.html>
;; Documentation: <http://angg.twu.net/eev-current/eepitch.readme.html>
;; Author: Eduardo Ochs <eduardoochs@gmail.com>.
;; Version: 2012mar26
;; License: GPL3.

;; WARNING: This file consists of a few stable functions - mainly
;; `find-w' and `find-wset' -, plus lots of VERY experimental code,
;; plus garbage, plus relevant comments, mixed with obsolete comments.
;; Use with care.
;;
;; What is this
;; ============
;; Emacs modes like GUD and Slime use multiple windows and do not
;; expect that the current frame will have more windows than the ones
;; that they use - which means that they can't handle an "e-script
;; window" in the current frame...
;;
;; It SEEMS that if we craft very carefully window setups that include
;; all the windows that these modes use - so that they will not try to
;; create extra windows or rearrange the existing ones - then we can
;; also accomodate on the screen an e-script window, and things *MAY*
;; work.
;;
;; For example, these window configurations *should* work for GDB -
;; but the defaults for GUD have changed in ways that I don't
;; understand yet, so I can't be sure, I need more tests...
;;
;;     _________________________      ________________________ 
;;    |             |           |    |            |           |
;;    |   e-script  |           |    |  e-script  |  program  |
;;    |             |           |    |            |   source  |
;;    |_____________|  program  |    |____________|___________|
;;    |  GDB prompt |   source  |    |            |           |
;;    |     and     |           |    |    GDB     |  program  |
;;    | program I/O |           |    |   prompt   |    I/O    |
;;    |_____________|___________|    |____________|___________|
;;
;;              Old GUD                 New GUD, with gdb-mi
;;
;; The (obsolete) tests for the GDB interface have been moved to:
;;   (find-eev "eepitch.readme" "gdb-preparation")
;;   (find-eev "eepitch.readme" "gdb-3-windows")
;;   (find-eev "eepitch.readme" "gdb-4-windows")
;;
;; Note that it is also possible to run gdb via comint, in a
;; two-window setting, without any of the usuall GUD goodies...
;;   (find-es "anatocc" "stabs")
;;   (find-es "anatocc" "stabs" "eepitch-comint")






(defun find-w (ms &rest es)
"Execute the macros in MS and the sexps in ES alternately."
  (when (or ms es)
    (eek  (car ms))          ; execute the first macro in ms
    (eval (car es))	     ; evaluate the first sexp in es
    (apply 'find-w (cdr ms) (cdr es))))  ; recurse

;; Example: for this setting,
;;  ______________
;; |              |
;; |    #eev      |
;; |______________|
;; |      |       |
;; | TODO |  *J*  |
;; |______|_______|
;;
;; run the `find-w' below:
;;
;;   (find-w
;;       '("C-x 1" "C-x 2 C-x o" "C-x 3 C-x o")
;;     ' (find-ebuffer "#eev@irc.freenode.net")
;;     ' (find-fline "~/TODO")
;;     ' (find-ebuffer "*J*")
;;     )
;;
;; CAVEAT: `find-ebuffer' and `find-fline' are defined in "eev.el"...

;; (find-angg ".emacs" "find-wset")
;; (find-angg ".emacs" "find-2")




(defun find-wset (chars &rest sexps)
  (if (not (equal chars ""))
      (let ((c     (substring chars 0 1))
	    (crest (substring chars 1)))
	(cond ((equal c "1")
	       (delete-other-windows)
	       (apply 'find-wset crest sexps))
	      ((equal c "2")
	       (split-window-vertically)
	       (apply 'find-wset crest sexps))
	      ((equal c "3")
	       (split-window-horizontally)
	       (apply 'find-wset crest sexps))
	      ((equal c "o")
	       (other-window 1)
	       (apply 'find-wset crest sexps))
	      ((equal c "+")
	       (balance-windows)
	       (apply 'find-wset crest sexps))
	      ((equal c "_")
	       (eval (car sexps))
	       (apply 'find-wset crest (cdr sexps)))
	      (t (error))))))







;;;            _ _     
;;;   __ _  __| | |__  
;;;  / _` |/ _` | '_ \ 
;;; | (_| | (_| | |_) |
;;;  \__, |\__,_|_.__/ 
;;;  |___/             
;;
;; THIS IS LEGACY CODE, not currently working - and I was never happy
;; with this anyway - this need to be rewritten using `find-w'!
;;
;; Note that Slime also uses several buffers, and if (and when) this
;; starts working well most ideas should appliable to controlling
;; Slime too.

(defvar eepitch-gdb-buffer-name nil)
(defvar eepitch-i/o-buffer-name nil)

(defun eepitch-window-show-3 ()
  "Display the buffer `eepitch-buffer-name' in another window.
This function uses a hardcoded 3-window setup, intended mainly for gdb."
  (delete-other-windows)
  (split-window-horizontally)  ; left|right
  (split-window-vertically)    ; split the left side into upper/lower
  (at-nth-window 1 '(switch-to-buffer eepitch-buffer-name)))

(defun eepitch-window-show-4 ()
  "Display the buffer `eepitch-buffer-name' in another window.
This function uses a hardcoded 4-window setup, intended mainly for gdb."
  (delete-other-windows)
  (split-window-horizontally)  ; left|right
  (split-window-vertically)    ; split the left side into upper/lower
  (at-nth-window 2 '(split-window-vertically)) ; split the right side into u/l
  (at-nth-window 1 '(switch-to-buffer eepitch-buffer-name))
  (at-nth-window 3 '(switch-to-buffer eepitch-i/o-buffer-name)))

;; To do: reimplement gud-keys (M-n for "next", etc) here.
;; (find-eev "eev-mini-steps.el" "eegud-kill-this-buffer")
;; (find-eev "eev-langs.el" nil "define-minor-mode eegud-keys-mode")
;; (setq eepitch-code `(progn (gdb ,cmd) (eegud-keys-mode 1)))

(defun eepitch-gdb-buffer-name-setup (program)
  (let (stem (file-name-nondirectory fullfname))
    (setq eepitch-gdb-buffer-name (format "*gud-%s*" stem))
    (setq eepitch-i/o-buffer-name (format "*input/output of %s*" stem))
    (setq eepitch-buffer-name eepitch-gdb-buffer-name)))

(defun eepitch-gdb-core-setup (gdb fmt program)
  (setq eepitch-code (list gdb (format fmt (ee-expand program))))
  (eepitch-gdb-buffer-name-setup program)
  (eepitch-prepare))

(defun eepitch-gdb-3 (program)
  (setq eepitch-window-show '(eepitch-window-show-3))
  (eepitch-gdb-core-setup 'gud-gdb "gdb --fullname %s" program))

(defun eepitch-gdb-4 (program)
  (setq eepitch-window-show '(eepitch-window-show-4))
  (eepitch-gdb-core-setup 'gdb "gdb --annotate=3 %s" program))

(defun eepitch-gdb-4 (program)
  (setq eepitch-window-show '(eepitch-window-show-4))
  (eepitch-gdb-core-setup 'gdb "gdb -i=mi %s" program))









;; Local Variables:
;; coding:            raw-text-unix
;; ee-anchor-format:  "defun %s "
;; no-byte-compile:   t
;; End:
