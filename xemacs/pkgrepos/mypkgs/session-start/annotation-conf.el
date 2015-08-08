;;; annotation-conf.el --- Annotation configurations

;; Copyright (C) 2015  sharad

;; Author: sharad <spratap@merunetworks.com>
;; Keywords:convenience

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




(deh-section "New"
  (deh-require-maybe (progn ipa org-pua)
    ;;http://www.emacswiki.org/emacs/InPlaceAnnotations
    )
  (deh-require-maybe alert
    ;;http://www.emacswiki.org/emacs/alert.el
    )
  (deh-require-maybe org-pua
    ;;http://www.emacswiki.org/emacs-es/org-pua.el
    ))


(provide 'annotation-conf)
;;; annotation-conf.el ends here
