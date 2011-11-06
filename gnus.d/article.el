;;; article.el --- Article related setting

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


;; (create-image )


(defun gnus-article-mst-show-country ()
  ;; from http://dishevelled.net/elisp/gnus-mst-show-country.el
  (interactive)
  (let ((from (message-fetch-field "From" t)))
    (when from
      (let ((addr (car (ietf-drums-parse-address from))))
        (when addr
          (let* ((field (progn
                          (string-match "\\.\\(\\sw+\\)$" addr)
                          (match-string 1 addr)))
                 (country (tld-to-country field)))
            (when country
              (save-restriction
                (article-narrow-to-head)
                (goto-char (point-max))
                (insert (propertize (concat "X-Country: " country "\n")
                                    'face 'gnus-header-subject-face))
               ;; (previous-line 1)
                (line-move-1 1)
                (beginning-of-line)))))))))


;;{{ http://www.inference.phy.cam.ac.uk/cjb/dotfiles/dotgnus
;; Show the time since the article came in.
;; see http://www.gnu.org/software/emacs/manual/html_node/gnus/Customizing-Articles.html#Customizing-Articles
(setq
 gnus-treat-date-lapsed 'head
 gnus-treat-display-x-face 'head
 ;; gnus-treat-date-original 'head
 ;; gnus-treat-date-local 'head
 ;; Make sure Gnus doesn't display smiley graphics.
 gnus-treat-display-smileys t)
;;}}


;; gnus-visible-headers

;; "^From:\\|^Newsgroups:\\|^Subject:\\|^Date:\\|^Followup-To:\\|^Reply-To:\\|^Organization:\\|^Summary:\\|^Keywords:\\|^To:\\|^[BGF]?Cc:\\|^Posted-To:\\|^Mail-Copies-To:\\|^Mail-Followup-To:\\|^Apparently-To:\\|^Gnus-Warning:\\|^Resent-From:\\|^X-Sent:"

(user-provide 'article)

;;; article.el ends here
