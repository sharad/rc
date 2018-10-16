;;
;; exsubr.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Wed Apr 20 19:17:33 2011 Sharad Pratap
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





(defun assq-delete-all-test (key alist &optional testf)
  "Delete from ALIST all elements whose car is `eq' to KEY.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (let ((testf (or testf 'eq)))
    (while (and (consp (car alist))
                (funcall testf (car (car alist)) key))
      (setq alist (cdr alist)))
    (let ((tail alist)
          tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
                 (funcall testf (car (car tail-cdr)) key))
            (setcdr tail (cdr tail-cdr))
            (setq tail tail-cdr))))
    alist))

(defun rassq-delete-all-test (value alist &optional testf)
  "Delete from ALIST all elements whose cdr is `eq' to VALUE.
Return the modified alist.
Elements of ALIST that are not conses are ignored."
  (let ((testf (or testf 'eq)))
    (while (and (consp (car alist))
                (funcall testf (cdr (car alist)) value))
      (setq alist (cdr alist)))
    (let ((tail alist) tail-cdr)
      (while (setq tail-cdr (cdr tail))
        (if (and (consp (car tail-cdr))
                 (eq (cdr (car tail-cdr)) value))
            (setcdr tail (cdr tail-cdr))
            (setq tail tail-cdr))))
    alist))


(provide 'exsubr-config)
