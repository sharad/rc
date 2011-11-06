;;
;; general.el
;; Login : <s@taj>
;; Started on  Sat Jun  5 20:43:49 2010 Sharad Pratap
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


(setq server-name "general")
(setq server-use-tcp t)
(setq server-host (system-name))
;;(when (not (server-running-p))
(server-start);;)
;;(if (functionp server-running-p)
;;    (when (not (server-running-p)
;;               (server-start)))
;;    (server-start))

(load-file "~/.xemacs/init.el")

;;{{ start: http://stackoverflow.com/questions/2231902/originate-edit-of-remote-file-using-emacs-tramp-from-ssh-session
;; You can set up your emacs-server to use a tcp connection (not
;; just a local socket), and then on the remote side, tell
;; emacsclient to connect to that tcp connection:

;; In your .emacs

;; (setq server-use-tcp t)
;; (setq server-host (system-name))
;; (when (not (server-running-p))
;;  (server-start))

;; And then on the remote side:

;; emacsclient -f ~/.emacs.d/server/server /`hostname`:/path/to/local/file

;; The above call to emacsclient brings up a file local to
;; the "remote" machine in your Emacs running in the "local"
;; machine. Obviously you can wrap the call to emacsclient in
;; whatever kind of script you want to make it easier.

;; If your home directory is not visible on the remote machine, you
;; will need to customize the server-auth-dir variable like so:

;; (setq server-auth-dir "/some/path/visible/on/both/machines")

;; For more documentation, see Emacsclient options.
;;}}

