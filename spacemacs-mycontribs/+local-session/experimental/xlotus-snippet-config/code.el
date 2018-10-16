;
;; snippet.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Tue Jul 27 12:25:14 2010 Sharad Pratap
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

;; (deh-require-maybe yasnippet ;; not yasnippet-bundle
;;     (yas/initialize)
;;     (yas/load-directory "~/.emacs.d/plugins/yasnippet-x.y.z/snippets"))


;; (xrequire 'yasnippet-bundle) ;; not yasnippet-bundle

(when (require 'expand-config)
  (deh-require-maybe (and yasnippet-bundle yasnippet) ;; not yasnippet-bundle
    ;; Develop in ~/.xemacs/data/yasnippet/snippets/mysnippets, but also
    ;; try out snippets in ~/Downloads/interesting-snippets
    (defconst *mysnippet-dirs* "~/.xemacs/data/yasnippet/snippets/my")
    (defconst *downloaded-snippet-dirs* "~/.xemacs/data/yasnippet/snippets/downloaded")
    (when (fboundp 'yas/initialize) (yas/initialize))
    (setq yas/root-directory
          (append ; '("~/.xemacs/packages/yasnippet/snippets")
           (directory-files *mysnippet-dirs* t "^[a-zA-Z0-9-]+$")
           (directory-files *downloaded-snippet-dirs* t "^[a-zA-Z0-9-]+$")))
    (mapc 'yas/load-directory yas/root-directory)))




;; Map `yas/load-directory' to every element


(provide 'snippet-config)
