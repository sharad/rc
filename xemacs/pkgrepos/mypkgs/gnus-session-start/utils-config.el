;;; utils-config.el ---

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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



(require 'thingatpt+)

(defun google-lucky (string)
  (concat string " [http://www.google.com/search?hl=en&&q="
          (or (string-replace-match "\s" string "+" t t) string)
          "&btnI=1]"))


(defun google (string)
  (concat string " [http://www.google.com/search?hl=en&&q="
          (or (string-replace-match "\s" string "+" t t) string)
          "]"))

(defun string-apply-fn (&optional fn)
  (interactive
   (let ((fn (symbol-function
              (intern (ido-completing-read "Modifier to apply: "
                                           '("google-lucky" "google"))))))
     (list fn)))
  (let* ((region-active (and (region-active-p)
                             (not (equal (region-beginning) (region-end)))))
         (bound (if region-active
                    (cons (region-beginning) (region-end))
                    (bounds-of-thing-at-point 'word)))
         (search-str (funcall #'buffer-substring (car bound) (cdr bound))))
    (funcall #'delete-region (car bound) (cdr bound))
    (insert (funcall fn search-str))))


(provide 'utils-config)
;;; utils-config.el ends here
