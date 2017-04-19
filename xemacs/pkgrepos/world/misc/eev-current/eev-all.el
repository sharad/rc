;;; eev-all.el -- load all the (main) modules of eev. Experimental.

;; Copyright (C) 2007,2012 Free Software Foundation, Inc.
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
;; Version:    2012apr27
;; Keywords:   e-scripts, help, hyperlinks, hypertext, processes,
;;             shell, tex
;;
;; Latest version: <http://angg.twu.net/eev-current/eev-all.el>
;;       htmlized: <http://angg.twu.net/eev-current/eev-all.el.html>
;;       See also: <http://angg.twu.net/eev-current/README.html>

;; Comment (2007): This file is/was very new (2007aug31). Its intent
;; is to makes it a bit easier to try eev.

;; Update (2012): take a look at:
;;   http://angg.twu.net/debian/
;;   http://angg.twu.net/debian/README.html
;; The Debian package reimplements some ideas here in a better way,
;; and I haven't been testing the "tarball way" much - if you have
;; problems, please report! 8-\

;; The easy way to try eev is to download and unpack the tarball
;; somewhere, with, for example,
;;
;;   mkdir ~/eev-current/
;;   cd    ~/eev-current/
;;   wget http://angg.twu.net/eev-current.tar.gz
;;   tar -xvzf eev-current.tar.gz
;;
;; and then load it and activate it with:
;;
;;   (add-to-list 'load-path "~/eev-current/")
;;   (require 'eev-all)
;;   (eev-mode 1)
;;
;; The effects of "(require 'eev-all)" are: lots of functions - mainly
;; for elisp hyperlinks - become defined; some environment variables -
;; $EE, $EEVTMPDIR, etc - are set inside Emacs, and will be inherited
;; by programs that are started from Emacs; and a few glyphs are
;; defined.
;;
;; Turning eev-mode on (with `M-x eev-mode') has only two effects:
;; the keymap `eev-mode-map' becomes active, and `pop-up-windows'
;; becomes nil (this makes the keys `M-e', `M-E', `M-k', and `M-K',
;; that follow and return from elisp hyperlinks, behave better).
;; Turning eev-mode off deactivates the keymap and restores the
;; value of `pop-up-windows'.
;;
;; The "full instalation" for eev involves changing some rcfiles -
;; .emacs, .bashrc, etc. This can be done semi-automatically (see
;; eev-rctool and the INSTALL file) and is trivial to undo. If you try
;; eev without changing rcfiles then `M-x eev' and <F3> will not work
;; properly, in the sense that the shortcut "ee" to execute the
;; temporary script file - a shell function - will not be defined.




(require 'eev)		       ; (find-eev "eev.el")
(require 'eev-bounded)	       ; (find-eev "eev-bounded.el")
(require 'eev-insert)	       ; (find-eev "eev-insert.el")
(require 'eev-steps)	       ; (find-eev "eev-steps.el")
(require 'eev-glyphs)	       ; (find-eev "eev-glyphs.el")
(require 'eev-compose)	       ; (find-eev "eev-compose.el")
(require 'eev-langs)	       ; (find-eev "eev-langs.el")
(require 'eev-browse-url)      ; (find-eev "eev-browse-url.el")

;; This is temporary - some of the stepper functions have been
;; rewritten and the new code (in eev-mini-steps.el) is currently much
;; better than the (slightly) more documented code in eev-steps.el...
;; In a near future the rewrites will be migrated to eev-steps and
;; this won't be necessary anymore.
;;
;; (require 'eev-mini-steps)   ; (find-eev "eev-mini-steps.el")
;; (require 'eechannel)        ; (find-eev "eechannel.el")

;; 2012: eepitch.el and eev-template.el are new, and they have
;; definitions that override some older definitions in other files...
;; The older files will be adjusted soon (for some value of "soon").
;;
(require 'eepitch)             ; (find-eev "eepitch.el")
(require 'eev-template)        ; (find-eev "eev-template.el")

;; New, interesting, and harmless:
(require 'eev-intro)           ; (find-eev "eev-intro.el")

(eev-set-aliases)	       ; (find-eev "eev.el" "eev-set-aliases")
(eev-set-default-glyphs)       ; (find-efunction 'eev-set-default-glyphs)

;; (2008oct15) this is a trick to make the info docs for eev available
;; without having to install them globally...
(if (not (boundp 'Info-additional-directory-list))
    (setq Info-additional-directory-list nil))
(add-to-list 'Info-additional-directory-list (ee-eevfile "doc"))
;; (find-node "(eev)Top")

;; (eev-mode 1)		       ; (find-efunctiondescr 'eev-mode)

(provide 'eev-all)


;; Local Variables:
;; coding:            raw-text-unix
;; no-byte-compile:   t
;; End:
