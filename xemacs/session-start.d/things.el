;;; things.el --- things

;; Copyright (C) 2011  Sharad Pratap

;; Author: Sharad Pratap <sadf sdf dsafdsf>
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:



(deh-require-maybe 'thingatpt
  ;;   Email addresses
  (defvar thing-at-point-fullemail-regexp
    "\\([a-zA-Z]+ \\)\\{1,2\\}<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
    "A regular expression probably matching an email address.
This does not match the real name portion, only the address, optionally
with angle brackets.")

  ;; Haven't set 'forward-op on 'email nor defined 'forward-email' because
  ;; not sure they're actually needed, and URL seems to skip them too.
  ;; Note that (end-of-thing 'email) and (beginning-of-thing 'email)
  ;; work automagically, though.

  (put 'fullemail 'bounds-of-thing-at-point
       (lambda ()
         (let ((thing (thing-at-point-looking-at thing-at-point-fullemail-regexp)))
           (if thing
               (let ((beginning (match-beginning 0))
                     (end (match-end 0)))
                 (cons beginning end))))))

  (put 'fullemail 'thing-at-point
       (lambda ()
         (let ((boundary-pair (bounds-of-thing-at-point 'fullemail)))
           (if boundary-pair
               (buffer-substring-no-properties
                (car boundary-pair) (cdr boundary-pair)))))))


;; Sharad Pratap <spratap@arubanetworks.com>


(provide 'things)
;;; things.el ends here
