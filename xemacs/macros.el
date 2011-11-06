;;
;; macros.el
;; Login : <s@taj>
;; Started on  Fri Nov 26 00:35:02 2010 Sharad Pratap
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


;; http://stackoverflow.com/questions/4283899/lisp-macro-set-or-nconc-not-able-to-get-working
(defmacro set-or-nconc (var &rest args)
  `(if (and (boundp ',var) (not (null ,var)))
       (nconc ,var ,@args)
     (setq ,var ,@args)))

