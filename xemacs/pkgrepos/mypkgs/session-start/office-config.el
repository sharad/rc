;;
;; office.el
;; Login : <spratap@spratap>
;; Started on  Wed Dec  1 17:11:05 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;



;; (define-minor-mode office-mode
;;     "Prepare for working with collarative office project."
;;   :initial-value nil
;;   :lighter " Office"
;;   :global nil
;;   (cond
;;     (office-mode
;;      (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
;;      )
;;     (t
;;      (add-hook 'before-save-hook 'delete-trailing-whitespace t t))))


;; This is the mode to be enabled when I am working in some files on
;; which other peoples are also working.





(define-minor-mode office-mode
    "Prepare for working with collarative office project."
  :initial-value nil
  :lighter " Office"
  :global nil
  (when office-mode
    (set (make-local-variable 'before-save-hook) before-save-hook)
    (remove-hook 'before-save-hook 'delete-trailing-whitespace t)))


(defun create-bug (bug)
  (interactive "nBug number: ")
  (make-directory (concat "/home/s/paradise/bugs/" (number-to-string bug)) t)
  (unless (write-region (format "\n\n* Bug %d analysis\n\n" bug) nil
                        (concat "/home/s/paradise/bugs/" (number-to-string bug) "/an0.org")
                        nil nil nil t)
    (find-file (concat "/home/s/paradise/bugs/" (number-to-string bug) "/an0.org"))))

(provide 'office-config)

