;;
;; keyring.el
;; Login : <s@taj>
;; Started on  Wed Jun 23 23:29:19 2010 Sharad Pratap
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


;; start: http://schurger.org/wordpress/

;; * Load the module using pymacs


(deh-require-maybe 'pymacs
  (eval-after-load "pymacs"
    '(add-to-list 'pymacs-load-path "~/.xemacs/pymacs/"))

  (pymacs-load "gnome-keyring" "gnome-keyring-")
  ;; (pymacs-load "ropemacs" "rope-")
  ;; ;; * Trying from the interactive emacs lisp mode (M-x ielm)

  ;; ELISP> (gnome-keyring-set-password "my-password" "p4$$w0rD")
  ;; nil
  ;; ELISP> (gnome-keyring-get-password "my-password")
  ;; "p4$$w0rD"

  ;; ;; * And now, an example for ERC

  ;; (setq erc-password (gnome-keyring-get-password "my-password"))
  ;; (setq erc-prompt-for-password nil)

  ;; ;; Too easy ! :D
  )

;; (gnome-keyring-get-password "localhost"))

(user-provide 'keyring)

