;;; contacts.el --- Contacts

;; Copyright (C) 2011  Sharad Pratap

;; Author:
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

(deh-require-maybe 'bbdb
  (setq bbdb-use-pop-up t) ;; I want it
  (remove-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer)
  (add-hook 'gnus-article-prepare-hook 'bbdb/gnus-pop-up-bbdb-buffer-for-some-time)

  (defun bbdb/gnus-pop-up-bbdb-buffer-for-some-time ()
    (bbdb/gnus-pop-up-bbdb-buffer)
    (let ((w (get-buffer-window "*BBDB*")))
      (if w
          (run-at-time "4 sec" nil #'delete-window w))))
  (define-key gnus-summary-mode-map (kbd "s-c s-v")  'bbdb/gnus-pop-up-bbdb-buffer))

;;

(user-provide 'contacts)


;;; contacts.el ends here

;; (run-at-time "4 sec" nil #'message "Hello")
;; (cancel-function-timers #'message)
