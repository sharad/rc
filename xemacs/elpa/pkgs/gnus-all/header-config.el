;;; header-config.el --- Header

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




(deh-section "Header and ignored From address."
  (setq
   gnus-extra-headers          '(To Newsgroups Content-Type Date)
   nnmail-extra-headers        '(To Newsgroups Content-Type Date)
   gnus-ignored-from-addresses "Sharad Pratap\\|sh4r4d.*\\|spratap.*")

;; (string-match gnus-ignored-from-addresses "spratapfd@arubanetwork" )

;; from http://www.ichimusai.org/pub/dot-gnus
(setq gnus-visible-headers
      '(
   	"^Cc:"
	"^Date:"
	"^Followup-To:"
	"^From:"
	"^Keywords:"
	"^Newsgroups:"
	"^Mailing-List:"
	"^Organization:"
	"^Posted-To:"
	"^Reply-To:"
	"^Subject:"
	"^Summary:"
	"^To:"
	"^X-Newsreader:"
	"^X-Url:"
        "^X-bugzilla"	; Show all X-headers
        ;; for attachment
        "^Content-Type"
        "^X-Face:"
        "^X-Face"
        )
      gnus-sorted-header-list
      '("^From:" "^Subject:" "^Summary:" "^Keywords:" "^Newsgroups:" "^Followup-To:" "^To:" "^Cc:" "^Date:" "^Organization:")))




(deh-section "Art"

  (setq gnus-article-x-face-command
        ;; http://git.gnus.org/cgit/gnus.git/plain/lisp/gnus-art.el?h=V5-8&id=9e60844ade6660e25359aefaf313daf3e92ff3a9
        ;; should be 'gnus-display-x-face-in-from else it will popup image outside
    (if (featurep 'xemacs)
        (if (or (gnus-image-type-available-p 'xface)
                (gnus-image-type-available-p 'pbm))
            'gnus-display-x-face-in-from
            "{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | ee -")
    (if (gnus-image-type-available-p 'pbm)
	'gnus-display-x-face-in-from
      "{ echo \
'/* Format_version=1, Width=48, Height=48, Depth=1, Valid_bits_per_item=16 */'\
; uncompface; } | icontopbm | display -"))))


;; gnus-extra-headers



(provide 'header-config)
;;; header.el ends here
