
;;
;; misc.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Fri Dec 10 14:44:47 2010 Sharad Pratap
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




;; Integration to bbdb and dired
(deh-require-maybe bbdb
  (add-hook 'gnus-startup-hook 'bbdb-insinuate-gnus))
(add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)

(make-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory) t)
(setq
 mail-user-agent 'gnus-user-agent
 gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory)
 gnus-startup-file   (expand-file-name "autoconfig/gnus/newsrc" user-emacs-directory))


(global-set-key-if-unbind (kbd "H-s") 'gnus-group-save-newsrc)

;; all from gnus-bonus-el deb package
(xrequire 'gnus-eyecandy) ; -enhance the group buffer by adding icons.
(xrequire 'gnus-filterhist) ; -add a buffer which display the message filtering
(xrequire 'gnus-junk) ; -semi-automatic replies to junk e-mails;
(xrequire 'gnus-pers) ; -an alternative to gnus-posting-styles.
(xrequire 'message-x) ; -customizable completion in message headers;
(xrequire 'nnir) ; -searchable mail backend;
(xrequire 'nnnil) ; -empty, read-only backend;
(xrequire 'nntodo) ; -manage to-do items;
(xrequire 'spam-stat) ; -spam-detector based on statistics.




;; www.student.montefiore.ulg.ac.be/~merciadri/emacs-gnus.php
(add-hook 'gnus-article-display-hook 'gnus-article-highlight-citation)


;; (defun gnus-other-frame ()
;;   "Like `mail' command, but display mail buffer in another frame."
;;   (interactive "P")
;;   (let ((pop-up-frames t)
;; 	(special-display-buffer-names nil)
;; 	(special-display-regexps nil)
;; 	(same-window-buffer-names nil)
;; 	(same-window-regexps nil))
;;     (pop-to-buffer "*mail*"))
;;   (mail noerase to subject in-reply-to cc replybuffer sendactions))





;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; General speedups.

(setq gnus-read-active-file nil
      gnus-check-new-newsgroups nil ; 'ask-server
      gnus-nov-is-evil nil
      gnus-save-newsrc-file t
      message-from-style nil) ;;'angles)

; (setq user-full-name "Chris Ball")

;; Using %G (default is %g) avoids the nnfoo:bar+ prefix
;; (setq gnus-group-line-format "%M\%S\%p\%5y: %G\n")


;; Use a second connection to grab the next article when I read one, so
;; I don't have to wait for it be downloaded.
(setq gnus-asynchronous t)

;;{{
(setq
 ;see http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC13

 ;;  1.9 Auto Save

 ;; Whenever you do something that changes the Gnus data (reading
 ;; articles, catching up, killing/subscribing groups), the change is
 ;; added to a special dribble buffer. This buffer is auto-saved the
 ;; normal Emacs way. If your Emacs should crash before you have saved
 ;; the `.newsrc' files, all changes you have made can be recovered
 ;; from this file.

 ;; If Gnus detects this file at startup, it will ask the user whether
 ;; to read it. The auto save file is deleted whenever the real
 ;; startup file is saved.

 ;; If gnus-use-dribble-file is nil, Gnus won't create and maintain a
 ;; dribble buffer. The default is t.

 ;; Gnus will put the dribble file(s) in gnus-dribble-directory. If
 ;; this variable is nil, which it is by default, Gnus will dribble
 ;; into the directory where the `.newsrc' file is located. (This is
 ;; normally the user's home directory.) The dribble file will get the
 ;; same file permissions as the .newsrc file.

 ;; If gnus-always-read-dribble-file is non-nil, Gnus will read the
 ;; dribble file on startup without querying the user.

 gnus-dribble-directory "~/.gnus-data"
 gnus-always-read-dribble-file t
 ;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC12
 ;; You can turn off writing the `.newsrc' file by setting
 ;; gnus-save-newsrc-file to nil, which means you can delete the file
 ;; and save some space, as well as exiting from Gnus faster. However,
 ;; this will make it impossible to use other newsreaders than
 ;; Gnus. But hey, who would want to, right?
 gnus-save-newsrc-file nil)
;;}}

;;{{[GENERAL]
;; http://www.chemie.fu-berlin.de/chemnet/use/info/gnus/gnus_3.html#SEC18
(setq
 ;If non-nil, the startup message won't be displayed. That way, your
 ;boss might not notice that you are reading news instead of doing
 ;your job.
 gnus-inhibit-startup-message t)
 ;Message displayed by Gnus when no groups are available.
 ;gnus-no-groups-message "No Gnus is good news"
;;}}



(setq gnus-registry-install t
      nnmail-crosspost t
      gnus-agent nil)

;;{{Exiting http://www.stanford.edu/~rgm/comp/dotgnus.html
(setq gnus-interactive-exit t)
;; Quit active Gnus if exiting Emacs.
;; Note that an abortive exit will kill Gnus. Win some, lose some.
;; Now fixed by having Emacs prompt for confirmation before hook runs.
(defun my-gnus-kill-on-exit-emacs-fn ()
  "Kill Gnus when exiting Emacs. Added to `my-before-kill-emacs-hook'."
  (setq gnus-interactive-exit nil)
  (gnus-group-exit))

(add-hook 'my-before-kill-emacs-hook 'my-gnus-kill-on-exit-emacs-fn)


(defun my-gnus-after-exiting-gnus-hook-fn ()
  "Function added to `gnus-after-exiting-gnus-hook'."
  (remove-hook 'my-before-kill-emacs-hook 'gnus-group-exit)
  (mapcar (lambda (buff)
            (and (get-buffer buff) (kill-buffer buff)))
          '("bbdb" "*BBDB*" "*Compile-Log*" "posts"))
;;;   (let ((gnus-startup-jingle
;;;          (expand-file-name
;;;           "Library/WindowMaker/Sounds/Windows/chimes.wav"
;;;           (or (getenv "GNUSTEP_USER_ROOT") "~/GNUstep"))))
;;;     (gnus-play-jingle))
  )

(add-hook 'gnus-after-exiting-gnus-hook 'my-gnus-after-exiting-gnus-hook-fn)
;;}}


;;{{
(setq gnus-local-domain
      (or (getenv "DOMAINNAME") office-fqdn))
;;}}




;;{{ other file

(setq

 ;; message-send-mail-function 'message-send-mail-with-sendmail
 ;; message-sendmail-envelope-from 'header
 ;; message-sendmail-f-is-evil nil

 ;; tls-checktrust 'ask
 ;; tls-program '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
 ;;               "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3"
 ;;               "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof")
 gnus-agent-synchronize-flags nil
 ;; gnus-agent-queue-mail 'always
 ;; gnus-agent-prompt-send-queue t
 ;; gnus-asynchronous t
 ;; gnus-agent-go-online t
 ;; mm-text-html-renderer 'gnus-w3m
 gnus-summary-display-arrow t
 gnus-completing-read-function 'gnus-ido-completing-read
 mail-user-agent 'gnus-user-agent
 read-mail-command 'gnus
 ;;gnus-treat-display-smileys nil
 )
(autoload 'sendmail-send-it "sendmail")


;;}}

(provide 'gnus-misc-config)
