;;; oneliner.el --- shell-mode hooks for Oneliners

;; Copyright (C) 2001,02,03  Kiyoka Nishiyama

;; Author:  Kiyoka Nishiyama <kiyoka@netfort.gr.jp>
;;          Masahiro Mishima <mishima@momo.so-net.ne.jp>
;; Created: Jan  9, 2001
;; Revised: $Date: 2003/12/09 00:37:46 $

;; This file is part of Oneliner

;; Oneliner is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; Oneliner is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with Oneliner; see the file COPYING.

;;; Commentary:

;; Minimum setup:
;;   1. Please add to ".emacs" file
;;	  (require 'oneliner)
;;
;; Recommend setup:
;;   1. change directory
;;        (global-set-key "\C-cd" 'oneliner-send-cd)
;;

;;; Documentation:

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; User Options
;;;

;; hook
(defcustom oneliner-init-hook '()
  "*Hook for customising oneliner."
  :type 'hook
  :group 'oneliner)

(defcustom oneliner-comint-send-hook '()
  "*Hook for oneliner-comint-send().
oneliner-comint-send() runs this hook at last of function."
  :type 'hook
  :group 'oneliner)

;; flag
(defcustom oneliner-keep-temp-file nil
  "*Non-nil means temporary files will not be deleted."
  :type 'boolean
  :group 'oneliner)

;; flag
(defcustom oneliner-show-top-of-pipe-buffer nil
  "*Non-nil means you can see the top of the pipe buffer when you
open one with `oneliner-pipe-buffer-other-window' command."
  :type 'boolean
  :group 'oneliner)

;; flag
(defcustom oneliner-beep       nil
  "*Non-nil means beep after command execution.  See also `oneliner-beep-time'."
  :type 'boolean
  :group 'oneliner)

;; time to beep
(defcustom oneliner-beep-time  3
  "*Minimum interval in second after which `oneliner-beep' will
take effect."
  :type 'integer
  :group 'oneliner)

;; pwd command name
(defcustom oneliner-pwd-command-on-shell  " \\pwd"
  "*shell command name that your shell display current directory path."
  :type 'string
  :group 'oneliner)

;; flag
(defcustom oneliner-complement-newline-for-input	nil
  "*Non-nil means complement newline at the end of input data."
  :type 'boolean
  :group 'oneliner)

;; flag
(defcustom oneliner-sync-default-directory-after-prompt	nil
  "*Non-nil means sync default-directory after prompt."
  :type 'boolean
  :group 'oneliner)

;; alist
(defcustom oneliner-indirect-buffer-alist nil
  "*Hold pairs of pipe buffer number and its base buffer name."
  :type '(repeat (cons integer string))
  :group 'oneliner)

;; flag
(defcustom oneliner-handle-control-codes-flag nil
  "*Non-nil means Handle control codes in oneliner-mode buffer."
  :type 'boolean
  :group 'oneliner)

;; flag
(defcustom oneliner-outpipe-busy-check nil
  "*Non-nil means block the key input during outpipe busy."
  :type 'boolean
  :group 'oneliner)

;; flag
(defcustom oneliner-display-evaled-value-flag nil
  "*Non-nil means display the evaluated value of pipe-buffer."
  :type 'boolean
  :group 'oneliner)

;; EOL conversion
(defcustom oneliner-eol-for-write nil
  "*EOL-TYPE for writing temporary file. Default is nil."
  :type '(choice (const :tag "default" nil)
		 (const :tag "unix" 0)
		 (const :tag "dos"  1)
		 (const :tag "mac"  2))
  :group 'oneliner)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Variables for internal
;;;
(defvar oneliner-debug nil)                       ; debugging enable/disable flag.
(set-default 'oneliner-hook-enable nil)           ;
(make-variable-buffer-local 'oneliner-hook-enable); hook action enable/disable flag.
(defvar oneliner-buffer-name-format	          ; FORMAT of pipe-buffer.
  "*Oneliner pipe*%d")
(defvar oneliner-default-pipe-buffer-id 0)        ; default id of in/out-pipe-buffer
(defvar oneliner-inpipe	 nil)		          ; input  pipe process is done flag(no use)
					          ;
(defvar oneliner-outpipe nil)	                  ; output pipe process is done flag
					          ; value takes 'buffer' or 'nil'
(defvar oneliner-eval    nil)	                  ; Eval flag when output pipe process is done
(defvar oneliner-beep-enable-flag nil)            ; for beep
(defvar oneliner-reserved-time nil)               ; for beep
(defvar oneliner-shell-buffer nil)                ; shell buffer-name
(defvar oneliner-invisible-command-flag nil)      ; for invisible-command
(defvar oneliner-invisible-command-result nil)    ; for invisible-command
(defvar oneliner-invisible-command-point nil)     ; for invisible-command
(defvar oneliner-shell-command-busy nil)          ; shell-command busy flag
(defvar oneliner-random-key-number
  (concat
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))
   (number-to-string (random 10))))
(defvar oneliner-shell-prompt-start               ; prompt key (start)
  (concat "<" oneliner-random-key-number ":prompt>"))
(defvar oneliner-shell-prompt-end                 ; prompt key (end)
  (concat "</" oneliner-random-key-number ":prompt>"))
(defvar oneliner-shell-prompt-pattern             ; matching pattern of prompt format
  (concat oneliner-shell-prompt-start "\\(.*\\)" oneliner-shell-prompt-end))
(defvar oneliner-std-start                        ; key for stdout data (start)
  (concat "<" oneliner-random-key-number ":std>"))
(defvar oneliner-std-end                          ; key for stdout data (end)
  (concat "</" oneliner-random-key-number ":std>"))
(defvar oneliner-std-pattern                      ; matching mattern of stdout data format
  (concat oneliner-std-start "\\(.*\\)" oneliner-std-end))
(defvar oneliner-ex-cmd-prefix                    ; for special command given from external program
  (concat "<" oneliner-random-key-number ":ex-cmd/>"))
(defvar oneliner-outpipe-command-string           ; output-pipe processing command
  (concat
   "awk '{"
   "printf(\""
   oneliner-std-start
   "%s"
   oneliner-std-end
   "\\n\", $0 );"
   "}'"
   ))
(defvar oneliner-title-display-done nil)          ; 'title displayed' flag
(defvar oneliner-deny-key-list                    ; deny keys during shell command execution.
  (append
   (split-string
    " !\"#$%&'()*+,-./0123456789:;<=>?@ABCDEFGHIJKLMNOPQRSTUVWXYZ[\]^_`abcdefghijklmnopqrstuvwxyz{|}~"
    "")
   '([delete] [backspace] [(control k)] [(control w)] [(control d)] [(control h)] [(control i)]
     [return] [del] [?\177] [?\^h] )))
(defvar oneliner-backup-local-map nil)            ; keymap for backup
(defvar oneliner-shell-type 'bash)                ; shell type (bash,zsh,csh)
(defvar oneliner-deny-keymap nil)                 ; keymap for deny
(defvar oneliner-temp-dir-name (getenv "TMPDIR")) ; temp-dir-name always using environment "TMPDIR".

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Program Code
;;;

(defconst oneliner-version-number "0.3.6")
(defconst oneliner-version (format "Oneliner version %s" oneliner-version-number) "Version string for this version of Oneliner.")
(defconst oneliner-shell-buffer-name "Oneliner shell")

(provide 'oneliner)
(require 'poe)
(require 'tramp-config)

;;--- debugging message logger
(defmacro oneliner-debug-print (string)
  `(if oneliner-debug
       (let ((buffer (get-buffer-create "*oneliner-debug*")))
	 (with-current-buffer buffer
	   (goto-char (point-max))
	   (insert ,string)))))


(defun cd-tramp-absolute (dir &optional base-directory)
  (let* ((base-directory (or base-directory default-directory))
         (prefix (tramp-file-prefix base-directory))
         (tdir (concat  prefix dir)))
    (cd-absolute tdir)))

(defun make-oneliner-shell-buffer-name ()
  (let ((connection-name
         (or
          (if (boundp 'oneliner-suffix)
              oneliner-suffix)
          (tramp-file-prefix default-directory))))
    (concat "*" oneliner-shell-buffer-name
            (if connection-name
                (concat " " connection-name))
            "*")))

(defun oneliner (&optional arg)
  "Execute Oneliner."
  (interactive "P")

  ;; directory for temporary file
  (cond (oneliner-temp-dir-name
	 (let ((rename nil)
               (shell-buffer-name (make-oneliner-shell-buffer-name)))
	   (cond ((get-buffer shell-buffer-name)
		  (set-window-buffer (selected-window) shell-buffer-name))
		 (t
		  (setenv "ONELINER_EX_CMD_PREFIX" oneliner-ex-cmd-prefix)
		  (setenv "ONELINER_STD_START" oneliner-std-start)
		  (setenv "ONELINER_STD_END" oneliner-std-end)
		  ;; (when (get-buffer "*shell*")
		  ;;   (with-current-buffer (get-buffer "*shell*")
		  ;;     (rename-buffer "*shell*-rename")
		  ;;     (setq rename t)))
		  (add-hook 'shell-mode-hook 'oneliner-init)
		  (shell shell-buffer-name)
		  (remove-hook 'shell-mode-hook 'oneliner-init)
		  ;; (when rename
		  ;;   (with-current-buffer (get-buffer "*shell*-rename")
		  ;;     (rename-buffer "*shell*")))
                  ))))
	(t
	 (message "Error: Please set environment variable TMPDIR (e.g. export TMPDIR=\"/home/youraccount/temp/\")"))))

(defun oneliner-title-display (string)
  (cond
   (oneliner-title-display-done
    nil)
   (t
    (setq oneliner-title-display-done t)
    (let
	((buffer (oneliner-get-pipe-buffer 0 t)))
      (with-current-buffer buffer (progn
				    (erase-buffer)
				    (insert oneliner-version)
				    (insert " --- shell-mode hooks for Oneliners

Author:  Kiyoka Nishiyama <kiyoka@netfort.gr.jp>
         Masahiro Mishima <mishima@momo.so-net.ne.jp>

------------------------------
Oneliner is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.


")
				    (goto-char (point-min))
				    (set-buffer-modified-p nil)))
      (oneliner-pipe-buffer-other-window)
      (select-window (display-buffer oneliner-shell-buffer))
      (goto-char (point-max))
      (sit-for 30)
      (with-current-buffer buffer (progn
				    (erase-buffer)
				    (set-buffer-modified-p nil)))))))

(defun oneliner-init ()
  ;;--- initialization of Oneliner hooks and variables.
  (let
      ((buffer (oneliner-get-pipe-buffer 0 t))
       (prompt-str))

    ;; (rename-buffer (make-oneliner-shell-buffer-name))
    (setq oneliner-hook-enable t)
    (setq oneliner-title-display-done nil)
    (oneliner-command-done)
    (add-hook 'comint-output-filter-functions 'oneliner-handle-OL-control)
    (add-hook 'comint-output-filter-functions 'oneliner-handle-control-codes)
    (setq comint-input-sender (function oneliner-comint-send))
    (setq comint-dynamic-complete-functions
	  (cons 'oneliner-buffer-complete
		comint-dynamic-complete-functions))
    (oneliner-set-keys)
    (setq oneliner-shell-buffer (current-buffer))
    (run-hooks 'oneliner-init-hook)
    (cond ((string-match "csh$" shell-file-name)
	   (setq oneliner-shell-type 'csh))
	  ((string-match "zsh$" shell-file-name)
	   (setq oneliner-shell-type 'zsh))
	  (t
	   (setq oneliner-shell-type 'bash)))
    (cond ((eq 'csh oneliner-shell-type)
	   (setq prompt-str (concat
			     " set prompt = \""
			     oneliner-shell-prompt-start
			     "%/"
			     oneliner-shell-prompt-end
			     "\\n${prompt}"
			     "\"\n"
			     )))
	  ((eq 'zsh oneliner-shell-type)
	   (setq prompt-str (concat
			     "unsetopt zle\nPROMPT=\$\'"
			     oneliner-shell-prompt-start
			     "\'%/\$'"
			     oneliner-shell-prompt-end
			     "\\n\'${PS1}\n"
			     )))

	  (t
	   (setq prompt-str (concat
			     " PS1=\""
			     oneliner-shell-prompt-start
			     "\\w"
			     oneliner-shell-prompt-end
			     "\\n${PS1}"
			     "\"\n"
			     ))))
    (comint-send-string oneliner-shell-buffer prompt-str)

    (oneliner-debug-print (format "Info: prompt-setcomand=[%s]\n" prompt-str))
    (oneliner-make-deny-keymap)))

;;--- setup key bindings
(defun oneliner-set-keys ()
  (local-set-key "\C-c\C-s" 'oneliner-sync-default-directory)
  (local-set-key "\C-c\C-b" 'oneliner-pipe-buffer-other-window)
  (local-set-key [(control c) (control ?0)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 0)))
  (local-set-key [(control c) (control ?1)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 1)))
  (local-set-key [(control c) (control ?2)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 2)))
  (local-set-key [(control c) (control ?3)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 3)))
  (local-set-key [(control c) (control ?4)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 4)))
  (local-set-key [(control c) (control ?5)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 5)))
  (local-set-key [(control c) (control ?6)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 6)))
  (local-set-key [(control c) (control ?7)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 7)))
  (local-set-key [(control c) (control ?8)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 8)))
  (local-set-key [(control c) (control ?9)]
		 (lambda () (interactive)(oneliner-pipe-buffer-other-window 9))))

;;--- pickup the prompt string.
(defun oneliner-get-prompt-string (str)
  (string-match oneliner-shell-prompt-pattern str)
  (match-string-no-properties 1 str))

;;--- resetting flags for when command execution done.
(defun oneliner-command-done nil
  (oneliner-beeper)
  (setq oneliner-inpipe	nil)
  (setq oneliner-outpipe nil)
  (setq oneliner-eval nil)
  (setq oneliner-shell-command-busy nil))

;;--- beeper
(defun oneliner-beeper nil
  (if oneliner-beep
      (if oneliner-beep-enable-flag
	  (beep))))

;;--- beep disabler (and enabler)
(defun oneliner-beep-disable (&optional en)
  (setq oneliner-beep-enable-flag (not en)))

;; [command string sender ( Oneliner original version )]
;; This function processes the in-pipe '|' and out-pipe '|' syntax.
(defun oneliner-comint-send (&optional proc string invisible)
  "Send input STRING to PROC."
  "Override `comint-input-sender'(comint.el's default)."
  (setq oneliner-invisible-command-flag nil)
  (if invisible
      (progn
	(setq oneliner-invisible-command-flag t)
	(setq oneliner-invisible-command-result nil)
	(comint-send-string proc string)
	(comint-send-string proc "\n")
	(oneliner-debug-print (concat "COMMAND(invisible):" string "\n"))
	string)

    ;; visible command processing
    (cond ((and oneliner-outpipe oneliner-outpipe-busy-check)
	   (message "Oneliner : outpipe busy...")
	   nil)
	  (t
	   (setq oneliner-shell-command-busy t)
	   ;; detection of in-pipe syntax
	   (if (string-match "^[ ]*\\([0-9]+\\|@[^|]+\\)?|" string)
	       (let* ((pipe  (1- (match-end 0)))
		      (bufid (match-string-no-properties 1 string))
		      (buf   (oneliner-get-buffer bufid))
		      (temp (concat
			     (make-temp-file
			      (concat
			       (file-name-as-directory oneliner-temp-dir-name)
			       "oneliner-")) ".tmp")))
		 (setq string
		       (if oneliner-keep-temp-file
			   (concat "cat " temp (substring string pipe))
			 (concat "(cat " temp ";rm -f " temp ")"
				 (substring string pipe))))
		 (oneliner-write-buffer buf temp)
		 (oneliner-debug-print
		  (format "INPUT : pipe=%s bufid=%s\n" pipe bufid))))
	   ;; detection of out-pipe syntax
	   (if (string-match "\\(+\\)?|\\([0-9]+\\|@[^|!]+\\)?\\([!]\\)?[ ]*$" string)
	       (let* ((pipe  (match-beginning 0))
		      (append (match-string-no-properties 1 string))
		      (bufid (match-string-no-properties 2 string))
		      (ev    (match-string-no-properties 3 string))
		      (buf   (oneliner-get-buffer bufid)))
		 (when ev (setq oneliner-eval buf))
		 (oneliner-debug-print
		  (format "OUT-PARAM: append=%s bufid=%s ev=%s\n" append bufid ev))
		 (setq string (concat
			       (substring string 0 pipe)
			       " | "
			       oneliner-outpipe-command-string
			       ))
		 (setq oneliner-outpipe buf)
		 (if (not append)
		     (with-current-buffer buf (erase-buffer)))
		 (oneliner-debug-print
		  (format "OUTPUT: pipe=%s bufid=%s\n" pipe bufid))
		 (oneliner-set-deny-keys))
	     (setq oneliner-outpipe nil))
	   ;; setting a beep timer.
	   (if oneliner-reserved-time
	       (cancel-timer oneliner-reserved-time))
	   (oneliner-beep-disable t)
	   (setq oneliner-reserved-time
		 (run-at-time oneliner-beep-time nil 'oneliner-beep-disable))
	   ;; sending the execution command.
	   (oneliner-debug-print (concat "COMMAND:" string "\n"))
	   (comint-send-string proc string)
	   (comint-send-string proc "\n")
	   (run-hooks 'oneliner-comint-send-hook)))))

;;
;; [invisible command sender]
;;   action:  sending command and pickup first line of output.
;;   return:  first line of output / nil(when no outout)
;;
(defun oneliner-invisible-command-exec (&optional string error-disp-flag)
  (let ((proc (get-buffer-process oneliner-shell-buffer)))
    (cond (oneliner-shell-command-busy
	   (message "Oneliner Error : !!BUSY!! can't exec invisible command\n" )
	   nil)
	  (t
	   (cond ((string-equal "exit" (process-status proc))
		  nil)
		 (t
		  (oneliner-debug-print (format "invisible shell proc = %s\n" proc))
		  (with-current-buffer oneliner-shell-buffer
		    (setq oneliner-invisible-command-point (point)))
		  (oneliner-comint-send proc string t)
		  (oneliner-debug-print (format "invisible accept = %s\n" (accept-process-output proc 2)))
		  (if error-disp-flag
		      (unless oneliner-invisible-command-result
			(message (concat "Can't execute command [" string "]"))))
		  (cond
		   ((string-equal "exit" oneliner-invisible-command-result)
		    nil)
		   (t
		    oneliner-invisible-command-result))))))))

;;
;; [buffer-object resolver with object, name or id]
;;
(defun oneliner-get-buffer (buffer)
  (cond ((bufferp buffer)
	 ;; in case of buffer object
	 buffer)
	((stringp buffer)
	 ;; in case of string
	 (if (string-match "^[0-9]*$" buffer)
	     ;; sequence of digits or empty string
	     (oneliner-get-pipe-buffer buffer t)
	   ;; other string
	   (if (string-match "^@" buffer)
	       ;; "@" and buffer name
	       (get-buffer-create (substring buffer (match-end 0)))
	     ;; buffer name without prefix
	     (get-buffer-create buffer))))
	(t
	 ;; not sure ... perhaps integer or something
	 (oneliner-get-pipe-buffer buffer t))))
;;
;; [buffer-name resolver with id]
;;
(defun oneliner-get-pipe-buffer-name (&optional id)
  (let ((name (format oneliner-buffer-name-format (oneliner-fix-id id))))
    (oneliner-debug-print (format "get-pipe-buffer-name %s\n" name))
    name))

;;
;; [buffer-object resolver with id ( and create action supported )]
;;
(defun oneliner-get-pipe-buffer (&optional id create)
  (let ((name (oneliner-get-pipe-buffer-name id)))
    (or (get-buffer name)
	(if create
	    (let ((base
		   (cdr (assq (oneliner-fix-id id)
			      oneliner-indirect-buffer-alist))))
	      (unless (functionp 'make-indirect-buffer)
		(progn
		  (setq base nil)
		  (message "oneliner.el [Warning]: make-indirect-buffer() is no exists.\n")
		  (sit-for 3)))
	      (cond (base
		     (get-buffer-create base)
		     (make-indirect-buffer base name))
		    (t
		     (get-buffer-create name))))))))

;;
;; popuping pipe-buffer
;;
(defun oneliner-pipe-buffer-other-window (&optional id)
  "Visit the working buffer for standard I/O in another window."
  (interactive "P")
  (setq id (or (oneliner-fix-id id)
	       (string-to-number
		(read-string
		 (format "Oneliner pipe buffer number (default %d): "
			 oneliner-default-pipe-buffer-id)))))
  (let ((window (display-buffer (oneliner-get-pipe-buffer id t))))
    (if oneliner-show-top-of-pipe-buffer
	(set-window-point window 1))))

;;
;; buffer-name complementation on command line
;;
(defun oneliner-buffer-complete ()
  (interactive)
  (let ((pos-saved (point)))
    (if (save-excursion (search-backward-regexp
			 "@\\([^#$%>|]*\\)$" (point-at-bol) t))
	(let* ((buf-input (match-string-no-properties 1))
	       (pos-begin (match-beginning 1))
	       (buf-list  (mapcar (lambda (b)
				    (list (buffer-name b) t))(buffer-list)))
	       (buf-name  (try-completion buf-input buf-list)))
	  (cond ((eq buf-name t)
		 (setq buf-name buf-input))
		((or (not buf-name) (string-equal buf-input buf-name))
		 (setq buf-name (completing-read
				 "Buffer name: " buf-list
				 nil nil (if buf-name buf-name buf-input)
				 buffer-name-history nil))))
	  (if buf-name
	      (progn
		(delete-region pos-begin pos-saved)
		(goto-char pos-begin)
		(insert buf-name)
		t))))))

;;
;;  [default-direcoty synchronizer]
;;
(defun oneliner-sync-default-directory ()
  "Sync the variable `default-directory' with current directory."
  (interactive)
  (let ((dir (oneliner-invisible-command-exec oneliner-pwd-command-on-shell))
	(len 0))
    (if dir (setq len (length (split-string dir " "))))
    (if (= 1 len) (cd-tramp-absolute dir))
    (if (called-interactively-p 'any) ;;(interactive-p)
	(message "Default directory is now '%s'" default-directory))
    default-directory))

;;
;;  [buffer-id resolver from buffer-id string, integer and 'nil'.]
;;
(defun oneliner-fix-id (id)
  (cond ((not id) oneliner-default-pipe-buffer-id)
	((stringp id) (string-to-number id))
	((natnump id) id)))

;;
;;  [current directory changer]
;;
(defun oneliner-send-cd (arg)
  "Change directory of *Oneliner shell* to current buffer's `default-directory'."
  (interactive "p")
  (message "Hello")
  (let ((curdir default-directory))
    (oneliner-invisible-command-exec (concat "cd " curdir))
    (when (called-interactively-p 'any) ;;(interactive-p)
      (message "Send to %s buffer 'cd %s'" (buffer-name oneliner-shell-buffer) curdir))))

;;
;;  [control code hander( hook for comint-output-filter-functions )]
;;
(defun oneliner-handle-control-codes (string)
  "Act properly when certain control codes are seen."
  ;; (message "ctrl-control: %s" default-directory)
  (when (and oneliner-handle-control-codes-flag oneliner-hook-enable)
    (let ((len (length (split-string string "[\r\n]"))))
      (save-excursion
	(goto-char (point-max))
	(forward-line (- len))
	(while (< (point) (point-max))
	  (let ((char (char-after)))
	    (cond
	     ((eq char ?\r)
	      (if t
		  (if (memq (char-after (1+ (point)))
			    '(?\n ?\r))
		      (delete-char 1)
		    (let ((end (1+ (point))))
		      (beginning-of-line)
		      (delete-region (point) end)))
		(add-text-properties (point) (1+ (point))
				     '(invisible t))
		(forward-char)))
	     ((eq char ?\a)
	      (delete-char 1)
	      (beep))
	     ((eq char ?\C-h)
              (delete-char -1)
              ;; (delete-backward-char 1)
	      (delete-char 1))
	     (t
	      (forward-char)))))))))


;;
;;  [OL-control string handler( hook for comint-output-filter-functions )]
;;
(defun oneliner-handle-OL-control (string)
  "Handle OL-control string."
  ;; (message "OL-control: %s" default-directory)
  (when oneliner-hook-enable
    (progn
      (let ((len (length (split-string string "[\n]")))
	    (str)
	    (prompt-flag nil)
	    (pwd)
	    (ma (point-marker)))

	(set-marker-insertion-type ma t)
	(save-excursion
	  (goto-char (point-max))
	  (forward-line (- len))
	  (oneliner-debug-print (format "[*oneliner*OUT] line=%d %s\n" len string))
	  (while (< (point-at-eol) (point-max))
	    ;;(setq str (buffer-substring (point-at-bol) (point-at-eol)))
	    (goto-char (point-at-bol))
	    (progn
	      (cond ((re-search-forward oneliner-shell-prompt-pattern (point-at-eol) t)
		     ;; find prompt pattern (may not be at beggining of line)
		     ;; and remove whole line
		     (setq str (match-string-no-properties 1))
		     (delete-region (point-at-bol) (point-at-eol))
		     (delete-char 1)
		     (when (string-match "\\(^.+\\)" str)
		       (setq pwd (match-string-no-properties 1 str))
		       (oneliner-debug-print (format "[*oneliner*prompt(%s)]\n" pwd)))
		     (setq prompt-flag t))
		    ((looking-at oneliner-std-pattern)
		     ;; find stdout pattern (should be at beggining of line)
		     ;; and redirect data into pipe-buffer
		     (setq str (match-string-no-properties 1))
		     (oneliner-debug-print (format "[*oneliner*std(%s)]\n" str))
		     (let ((l-ma))
		       (with-current-buffer oneliner-outpipe

			 (setq l-ma (point-marker))
			 (set-marker-insertion-type l-ma t)
			 (save-excursion
			   (goto-char (point-max))
			   (insert str "\n"))
			 (goto-char l-ma)))

		     (let ((str (buffer-substring (match-beginning 0) (match-end 0))))
		       (oneliner-debug-print (concat "[*oneliner*longdelete(" str ")]\n")))
		     (let ((str (buffer-substring (point-at-bol) (point-at-eol))))
		       (oneliner-debug-print (concat "[*oneliner*delete(" str ")]\n")))

		     (delete-region (match-beginning 0) (match-end 0))
		     (if (memq (char-after) '(?\n ?\r))
			 (delete-char 1))
		     )
		    ((looking-at oneliner-ex-cmd-prefix)
		     ;; find external command (should be at beggining of line)
		     ;; and do special behavior
		     (goto-char (match-end 0))
		     (cond
		      ((looking-at "autoeval")
		       ;; eval whole output as elisp after comand finish
		       (let ((buf (get-buffer-create "*Oneliner auto-eval*")))
			 (unless oneliner-outpipe
			   (with-current-buffer buf (erase-buffer))
			   (setq oneliner-outpipe buf)
			   (setq oneliner-eval buf))))
		      ((looking-at "eval *\\([^\r\n]*\\)")
		       ;; eval argnument as elisp just now
		       (eval-region (match-beginning 1) (match-end 1)))
		      ((looking-at "outpipe *\\([0-9]+\\|@[^|!\r\n]+\\)")
		       ;; switch outpipe
		       (let* ((bufid (match-string-no-properties 1))
			      (buf   (oneliner-get-buffer bufid)))
			 (with-current-buffer buf (erase-buffer))
			 (setq oneliner-outpipe buf))))
		     (delete-region (match-beginning 0) (match-end 0))
		     (if (memq (char-after) '(?\n ?\r))
			 (delete-char 1)))
		    (t
		     ;; just leave output in shell buffer
		     (forward-line 1)
		 ))))

	  ;;
	  ;; prompt proc (Comes here When prompt string was found.)
	  ;;
	  (cond (prompt-flag
		 (unwind-protect
		     (when oneliner-eval
		       (oneliner-eval-buffer oneliner-eval))
		   (progn
		     (oneliner-unset-deny-keys)
		     (oneliner-debug-print (format "[*oneliner*PROMPT-flag-on]\n" ))
		     (oneliner-command-done)
		     (cond (oneliner-invisible-command-flag
			    (goto-char oneliner-invisible-command-point)
			    (setq str (buffer-substring (point-at-bol) (point-at-eol)))
			    (when (string-match "[ ]\\(.+$\\)" str)
			      (setq oneliner-invisible-command-result (match-string-no-properties 1 str)))
			    (oneliner-debug-print (format "[*oneliner*invisible command result] %s\n" oneliner-invisible-command-result))
			    (delete-region oneliner-invisible-command-point (point-max))
			    )
			   (t
			    (oneliner-title-display "")
			    (when oneliner-sync-default-directory-after-prompt
			      (when (featurep 'meadow)
				(setq pwd (car (split-string (shell-command-to-string (concat "cygpath -w " pwd))))))
			      (cd-tramp-absolute pwd))))
		     )))))
	  (goto-char (marker-position ma))))))


;;
;; write buffer into file
;;
(defun oneliner-write-buffer (buffer file)
  "Write specified buffer into file.

First arugment BUFFER specifies buffer, which may be buffer object,
buffer name or id number of pipe-buffer.
Second argument FILE should specifies output file name."
  (let ((buf (oneliner-get-buffer buffer)))
    (with-current-buffer buf
      (let (;; If coding system of this buffer is not nil,
	    ;; set EOL conversion to the customized type.
	    (coding-system-for-write
	     (if buffer-file-coding-system
		 (coding-system-change-eol-conversion
		  buffer-file-coding-system oneliner-eol-for-write))))
	(write-region (point-min) (point-max) file nil 0)
	(if (and oneliner-complement-newline-for-input
		 (not (eq ?\n (char-before (point-max)))))
	    (with-temp-buffer
	      (insert ?\n)
	      (write-region (point-min) (point-max) file t 0)))))))
;;
;; [buffer evaluater]
;;
(defun oneliner-eval-buffer (&optional buffer)
  "Execute Oneliner."
  (interactive "P")
  (let ((curdir (with-current-buffer oneliner-shell-buffer default-directory)))
    (if (not buffer) (setq buffer (oneliner-get-pipe-buffer 0)))
    (with-current-buffer buffer
      (setq default-directory curdir)
      (eval-buffer nil oneliner-display-evaled-value-flag))))

;;
;;  [key blocker during busy]
;;
(defun oneliner-set-deny-key (string)
  (define-key oneliner-deny-keymap string '(lambda () (interactive)
					     (message (concat
						       (buffer-name oneliner-shell-buffer)
						       "buffer is busy...")))))
;; making    deny  keymap
(defun oneliner-make-deny-keymap nil
  (setq oneliner-deny-keymap (copy-keymap (current-local-map)))
  (mapcar 'oneliner-set-deny-key oneliner-deny-key-list))
;; setting   deny  keys
(defun oneliner-set-deny-keys nil
  (setq oneliner-backup-local-map (copy-keymap (current-local-map)))
  (use-local-map oneliner-deny-keymap))
;; resetting deny keys
(defun oneliner-unset-deny-keys nil
  (when (keymapp oneliner-backup-local-map)
    (use-local-map (copy-keymap oneliner-backup-local-map))))

;;; oneliner.el ends here
