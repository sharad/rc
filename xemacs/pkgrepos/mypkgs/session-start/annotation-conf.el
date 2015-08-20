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


(require 'macros-config)

(deh-section "New"
  (deh-require-maybe (progn ipa org-pua)
    ;;http://www.emacswiki.org/emacs/InPlaceAnnotations
    )
  (deh-require-maybe alert
    ;;http://www.emacswiki.org/emacs/alert.el
    )
  (deh-require-maybe org-pua
    ;;http://www.emacswiki.org/emacs-es/org-pua.el
    )

  (deh-require-maybe org-annotate-file)

  (deh-require-maybe annot
    ;; TODO: After adding annotation add it to a org file with link back to place where annotation were added.
    ;; it could help to search annotation when the original place is forgotten.
    ;; https://code.google.com/p/annot/
    ;; * [C-x a]    -  add a new annotation/highlight or edit an existing annotation on point.
    ;;                 You can also use [C-x C-a]. (annot-edit/add)
    ;; * [C-x r]    -  remove annotation at point. (annot-remove)
    ;; * [C-x w]    -  insert an image at point. (annot-add-image)
    (setq
     ;; annot-image-directory
     annot-directory (auto-config-dir "annot/" t)
     annot-enable-symlinking t)

    ;; (define-key ctl-x-map "a"    'annot-edit/add)
    ;; (define-key ctl-x-map "\C-a" 'annot-edit/add)
    ;; (define-key ctl-x-map "r"    'annot-remove)
    ;; (define-key ctl-x-map "w"    'annot-add-image)
    ;; (define-key ctl-x-map "A"    'annot-convert)

    ))


(provide 'annotation-conf)
;;; annotation-conf.el ends here
