;;
;; ercrc.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Thu Mar  3 16:13:21 2011 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;



;; from: http://www.emacswiki.org/emacs/ErcExampleEmacsFile

(add-hook 'erc-after-connect '(lambda (SERVER NICK)
                               (erc-message "PRIVMSG" "NickServ identify mypassword")))

;; Here is a slightly more complex erc-after-connect hook:

;; ******** DO NOT USE BOTH OF THEM! ********

(add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
            (cond
    	      ((string-match "freenode\\.net" SERVER)
    	       (erc-message "PRIVMSG" "NickServ identify ctrplmqw"))

    	      ;; ((string-match "oftc\\.net" SERVER)
    	      ;;  (erc-message "PRIVMSG" "NickServ identify password2"))

    	      ;; ((string-match "jin\\.tekken" SERVER)
    	      ;;  (erc-message "PRIVMSG" "#bitlbee identify password3"))
    	      ((string-match "localhost" SERVER)
    	       (erc-message "PRIVMSG" "#bitlbee identify ctrplmqw"))
              )))


(require 'erc-join)
(erc-autojoin-mode 1)
(setq erc-autojoin-channels-alist
      '(("freenode.net" "#emacs" "#gnustep" "#latex" "#screen"
         "#fnr" "#fnr-staff" "#ducttape" "#carvux" "#unit-e" "#isys"
         "#fsptb" "#freestream")
        ("crystalia.net" "#crystalia")
        ("localhost" "#bitlbee")
        ))


(require 'erc-match)
(setq erc-keywords '("davidmccabe"))
(erc-match-mode)

(require 'erc-track)
(erc-track-mode t) ; was (erc-track-modified-channels-mode t)
                                        ; Note: erc-track-modified-channels-mode changed
                                        ; to erc-track-mode as of erc-track.el
                                        ; CVS revision 1.23 (November 2002)

(add-hook 'erc-mode-hook
          '(lambda ()
            (require 'erc-pcomplete)
            (pcomplete-erc-setup)
            (erc-completion-mode 1)))

(require 'erc-fill)
(erc-fill-mode t)

(require 'erc-ring)
(erc-ring-mode t)

(require 'erc-netsplit)
(erc-netsplit-mode t)

(erc-timestamp-mode t)
(setq erc-timestamp-format "[%R-%m/%d]")

(erc-button-mode nil) ;slow

(setq erc-user-full-name "Sharad Pratap")
(setq erc-email-userid "sh4r4d@NOSPAM.gmail.com")

;; logging:
(setq erc-log-insert-log-on-open nil)
(setq erc-log-channels t)
(setq erc-log-channels-directory "~/.irclogs/")
(setq erc-save-buffer-on-part t)
(setq erc-hide-timestamps nil)

(defadvice save-buffers-kill-emacs (before save-logs (arg) activate)
  (save-some-buffers t (lambda () (when (and (eq major-mode 'erc-mode)
                                             (not (null buffer-file-name)))))))

(add-hook 'erc-insert-post-hook 'erc-save-buffer-in-logs)
(add-hook 'erc-mode-hook '(lambda () (when (not (featurep 'xemacs))
                                       (set (make-variable-buffer-local
                                             'coding-system-for-write)
                                            'emacs-mule))))
;; end logging

;; Truncate buffers so they don't hog core.
(setq erc-max-buffer-size 20000)
(defvar erc-insert-post-hook)
(add-hook 'erc-insert-post-hook 'erc-truncate-buffer)
(setq erc-truncate-buffer-on-save t)


;; Clears out annoying erc-track-mode stuff for when we don't care.
;; Useful for when ChanServ restarts :P
(defun reset-erc-track-mode ()
  (interactive)
  (setq erc-modified-channels-alist nil)
  (erc-modified-channels-update))
(global-set-key (kbd "C-c r") 'reset-erc-track-mode)


;;; Finally, connect to the networks.
(defun irc-maybe ()
  "Connect to IRC."
  (interactive)
  (when (y-or-n-p "IRC? ")
    (erc :server "irc.freenode.net" :port 6667
         :nick "sharad" :full-name "Sharad Pratap")
    ;; (erc :server "localhost" :port 6667
    ;;      :nick "s" :full-name "Sharad Pratap")
    (erc :server "localhost" :port 6667 :nick "s")))
