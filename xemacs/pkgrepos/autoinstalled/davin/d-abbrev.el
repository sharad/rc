;;; d-dabbrev.el --- extensions to dabbrev

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: dabbrev extensions
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-dabbrev.el

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-dabbrev.el)

;;; Known Bugs:

;; None so far!

(defadvice dabbrev-expand (around d-dabbrev activate)

  (if (not (get-buffer " *dabbrev*"))
      (generate-new-buffer " *dabbrev*"))

  (save-excursion 
    (set-buffer " *dabbrev*")
    (erase-buffer)
    (insert (prin1-to-string kill-ring))
    (insert (prin1-to-string file-name-history))
    (insert (prin1-to-string (mapcar 'file-name-nondirectory file-name-history)))
    (insert (prin1-to-string command-history))
    (insert (prin1-to-string minibuffer-history))
    (insert (prin1-to-string search-ring))
    ;; CATT:(if (boundp 'apropos-pattern-quoted) (insert (prin1-to-string apropos-pattern-quoted)))
    (insert (if (boundp 'apropos-pattern-quoted) (prin1-to-string apropos-pattern-quoted) ""))
    ;; CATT: (if (not (boundp 'apropos-pattern-quoted)) (insert (prin1-to-string apropos-pattern-quoted)))
    ;;(insert (if (not (boundp 'apropos-pattern-quoted)) (prin1-to-string apropos-pattern-quoted) ""))
    (insert (prin1-to-string (mapcar 'buffer-name (buffer-list))))
    (insert (prin1-to-string (mapcar 'buffer-file-name (buffer-list))))
    (if (boundp 'd-delete-backspace-list) (insert (prin1-to-string d-delete-backspace-list)))
    )
  ad-do-it
  )

(provide 'd-dabbrev)
