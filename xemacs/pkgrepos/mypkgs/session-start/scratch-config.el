;;
;; scratch.el
;; Login : <spratap@spratap>
;; Started on  Fri Jun 11 17:18:56 2010 Sharad Pratap
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


;; (defn (key-binding key t))

;; (setq x (read-key-sequence "safds: "))
;; (funcall (key-binding x t))



(defun dividebymb ()
  "divide by 1 mb"
  (interactive)
  (let* ((number (number-at-point))
         (res (/ number 1048576)))
    (when res
        (forward-word)
        (insert ?\  (number-to-string res) "MB"))))


(provide 'scratch-config)
