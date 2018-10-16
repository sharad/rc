;;
;; jde.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Tue Mar 29 11:32:09 2011 Sharad Pratap
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


(deh-require-maybe jde
  ;; from http://nets.ucar.edu/nets/intro/staff/siemsen/tools/editors/xemacs/jde.html#use
  (setq jde-bug-vm-includes-jpda-p t
        jde-bug-jdk-directory "/usr/local/jdk1.3/"
        jde-db-debugger (quote ("JDEbug" "" . "Executable"))
        jde-db-source-directories (quote ("/home/siemsen/nandisc" "/home/yplay-java" "/home/opennnms-all/jsnmp"))))



(provide 'jde-config)

