;;; utils.el ---

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d@gmail.com>
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


(defun google-lucky (search-str)
  (interactive
   (let* ((reg-str
           (if (and (region-active-p)
                    (not (equal (region-beginning)
                                (region-end))))
               (buffer-substring (region-beginning)
                                 (region-end))))
          (search-str (read-from-minibuffer "lucky: " (or reg-str (word-at-point) ""))))
     (list search-str)))
  (insert
   (concat search-str
           " [http://www.google.com/search?hl=en&&q="
           (string-replace-match "\s" search-str "+" t t)
           "&btnI=1]")))








(user-provide 'utils)
;;; utils.el ends here
