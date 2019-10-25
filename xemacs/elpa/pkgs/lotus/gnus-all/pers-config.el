;;
;; pers.el
;; Login : <s@taj>
;; Started on  Fri Apr 15 07:49:39 2011 Sharad Pratap
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


(deh-require-maybe gnus-pers

  (defvar gnus-personality-activate nil "")

  (defun gnus-personality-activate-toggle ()
    "Toggle setting tab widths between 4 and 8"
    (interactive)
    (setq gnus-personality-activate
          (not gnus-personality-activate)))



  (defun gnus-personality-init/sharad ()
    "Install Personality functionality into message mode."
    (interactive)
    (add-hook 'message-setup-hook
              (lambda ()
                (if gnus-personality-activate
                    (gnus-personality-electric-headers headers)))))


  (gnus-personality-init/sharad))


(provide 'pers-config)
