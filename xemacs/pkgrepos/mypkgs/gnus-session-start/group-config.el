;;; group-config.el --- GNUS group related behaviours.

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



;;{{ http://www.gnu.org/software/emacs/manual/html_node/gnus/Group-Parameters.html
(deh-section "GNUS Group Parameters."
  (when (require 'summary-config)
    (setq gnus-parameters
          `(
            (".*"
             (gnus-summary-line-format ,sharad/gnus/global-summry-line-format)
             (gnus-summary-display-arrow t)
             (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
             (gnus-article-sort-functions '(gnus-article-sort-by-date gnus-article-sort-by-score)))
                                        ;"Gnus: %g [%A] %Z"

            ("nnimap.*\\.bugs"
             (gnus-summary-line-format ,sharad/gnus/bugzilla-summry-line-format))

            ("nnimap.*\\.sent-mail\\|.*sent"
             (gnus-summary-line-format ,sharad/gnus/sent-summry-line-format)
             (gnus-summary-display-arrow t)
             (gnus-summary-mode-line-format "Gnus: %p [%A / Sc:%4z] %Z")
                                        ;"Gnus: %g [%A] %Z"
             (gnus-extra-headers '(To Newsgroups X-Newsreader))
             (gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*"))
            ("nnshimbun.*"
             (encapsulate-images t))))))

(setq nnshimbun-group-parameters-alist
 '(
   ("^nnshimbun.*:" index-range all prefetch-articles off
    encapsulate-images on expiry-wait 6)))

;; ("mail\\..*"
;;  (gnus-show-threads nil)
;;  (gnus-use-scoring nil)
;;  (gnus-summary-line-format
;;   "%U%R%z%I%(%[%d:%ub%-23,23f%]%) %s\n")
;;  (gcc-self . t)
;;  (display . all))

;;  ("^nnimap:\\(foo.bar\\)$"
;;  (to-group . "\\1"))

;; ("mail\\.me"
;;  (gnus-use-scoring  t))

;; ("list\\..*"
;;  (total-expire . t)
;;  (broken-reply-to . t))


;;}}

(provide 'group-config)
;;; group-config.el ends here
