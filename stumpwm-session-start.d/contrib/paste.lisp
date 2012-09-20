  ;; -*-lisp-*-
  ;;paste.lisp --- my Common Lisp package to enhance pasting in Stumpwm
  ;;Copyright (C) 2006 by gwern
  ;;Author: gwern <gwern0@gmail.com>
  ;;License: public domain
  ;;Where: http://stumpwm.elektrubadur.se/cgi-bin/wiki/paste.lisp
  ;;When:  Time-stamp: "2007-01-09 18:08:23 gwern"
  ;;Keywords: local,customization,stumpwm
  ;;Commentary: Allows Emacs-style registers for copying and pasting

  ;;Installation: you can either load this file through your .stumpwmrc by adding
  ;;"(load "/home/who-ever/paste.lisp")" (load requires the full path), or you can
  ;;just copy and paste this in. Omitting the package statements might or might not help.

  ;;Declare what we work with and where we are.
  (defpackage :swm-paste
   (:use :cl :stumpwm))
  (in-package :swm-paste)

  ;;Keybindings
  ;;;Two choices. First choice, is bound to a bunch of number keys. The name is already set for you.
  (defvar *paste-map* nil
   "The default bindings that hang off the paste key.")
  (when (null *paste-map*)
   (setf *paste-map*
         (let ((m (make-sparse-keymap)))
           (define-key m (kbd "1") "set-paste 1")
           (define-key m (kbd "2") "set-paste 2")
           (define-key m (kbd "3") "set-paste 3")
           (define-key m (kbd "4") "set-paste 4")
           (define-key m (kbd "5") "set-paste 5")
           (define-key m (kbd "6") "set-paste 6")
           (define-key m (kbd "7") "set-paste 7")
           (define-key m (kbd "8") "set-paste 8")
           (define-key m (kbd "9") "set-paste 9")
       (define-key m (kbd "M-1") "get-paste 1")
           (define-key m (kbd "M-2") "get-paste 2")
           (define-key m (kbd "M-3") "get-paste 3")
           (define-key m (kbd "M-4") "get-paste 4")
           (define-key m (kbd "M-5") "get-paste 5")
           (define-key m (kbd "M-6") "get-paste 6")
           (define-key m (kbd "M-7") "get-paste 7")
           (define-key m (kbd "M-8") "get-paste 8")
           (define-key m (kbd "M-9") "get-paste 9")
           m)))
  (define-key *root-map* (kbd "P") '*paste-map*)

  ;;;The second choice is to simply use two keys, and specify the name of the register
  ;;;manually as we please.
  (define-key *root-map* (kbd "i") "set-paste")
  (define-key *root-map* (kbd "I") "get-paste")

  ;;Create the hash table, define the creating, deleting ,setting, and getting functions.
  (defparameter *paste-hash* (make-hash-table :test 'equal))

  ;;If you try to set a register when there is no selection, it crashes Stumpwm.
  ;;Rather than add error-checking, simply guarantee there will always be *something*
  (set-x-selection " ")


  (define-stumpwm-command "create-paste" ((paste-number-arg :string "Empty register to create: "))
   (check-type paste-number-arg string)
   (setf 
    (gethash paste-number-arg *paste-hash*) 
    nil)
   )

  (define-stumpwm-command "delete-paste" ((paste-number-arg :string "Register name to delete: "))
   (if 
    (remhash paste-number-arg *paste-hash*) ;delete it
    (echo-string (current-screen) "Deleted.") ;If deletion was successful, it returns true
    (echo-string (current-screen) ;If failed, did not exist
         (concatenate 'string "Register '" paste-number-arg "' " "not deleted. Does not exist?")) 
    )
   )

  (define-stumpwm-command "set-paste" ((paste-number-arg :string "Register name to store in: "))
   (setf 
    (gethash paste-number-arg *paste-hash*) 
    (get-x-selection))
   )

  (define-stumpwm-command "get-paste" ((paste-number-arg :string "Register name to paste from: "))
   (window-send-string 
    (screen-current-window (current-screen)) 
    (check-type (gethash paste-number-arg *paste-hash*) string))   
   )
