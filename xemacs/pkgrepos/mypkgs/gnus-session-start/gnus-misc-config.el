
;;
;; misc.el
;; Login : <spratap@spratap>
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


(setq
 mail-user-agent 'gnus-user-agent)


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

(provide 'gnus-misc-config)
