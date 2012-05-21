;;
;; autoinstall.el
;; Login : <s@taj>
;; Started on  Thu Sep 16 14:23:43 2010 Sharad Pratap
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


;; from: http://www.emacswiki.org/emacs/AutoInstall
(deh-require-maybe 'auto-install
  (setq auto-install-use-wget t
        auto-install-directory "~/.xemacs/pkgrepos/world/auto-install/")
  ; (auto-install-update-emacswiki-package-name t)
  (when (and (xrequire 'anything)
             (xrequire 'anything-auto-install))
    (setq anything-sources
          (list
           anything-c-source-auto-install-from-emacswiki
           anything-c-source-auto-install-from-library))))


(user-provide 'autoinstall)

