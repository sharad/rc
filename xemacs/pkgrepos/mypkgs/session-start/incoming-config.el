;;
;; incoming.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Tue Jun 14 18:51:09 2011 Sharad Pratap
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


;;{{ http://www.emacswiki.org/emacs/RecipeForSkeletonMode
(defvar make-skeleton-saved-winconf nil)
(defvar make-skeleton-header ";; help for skeleton
;; (find-w3m \"http://www.panix.com/~tehom/my-code/skel-recipe.txt\")
;; (describe-function 'skeleton-insert)
;; These lines are ignored.
"
  "Help string for skeleton.")

(defun make-skeleton ()
  "Create skeleton of skeleton.
It is based on `A recipe for using skeleton.el'.
http://www.panix.com/~tehom/my-code/skel-recipe.txt

C-c C-e: Erase the skeleton contents.
C-c C-c: Finish the input.
"
  (interactive)
  (setq make-skeleton-saved-winconf (current-window-configuration))
  (switch-to-buffer "*make skeleton*")
  (make-skeleton-mode)
  (if (zerop (buffer-size))
      (make-skeleton-erase-buffer)))

(defun make-skeleton-finish ()
  (interactive)
  (set-window-configuration make-skeleton-saved-winconf)
  (insert "(define-skeleton ")
  (save-excursion
    (insert "-skeleton-\n"
            "\"Insert \" nil\n")
    (let ((lines (with-current-buffer "*make skeleton*"
                   ;; skip header
                   (goto-char (point-min))
                   (re-search-forward "^[^;]")
                   (beginning-of-line)
                   (split-string (buffer-substring (point) (point-max)) "\n"))))
      (dolist (line lines nil)
        (back-to-indentation)
        (insert (format "%S > \\n\n" line))))
    (insert ")\n")))

(defun make-skeleton-erase-buffer ()
  "Erase the skeleton contents."
  (interactive)
  (erase-buffer)
  (insert make-skeleton-header))


(define-derived-mode make-skeleton-mode fundamental-mode "skeleton"
  "Major mode for creating a skeleton of skeleton."
  (define-key make-skeleton-mode-map "\C-c\C-c" 'make-skeleton-finish)
  (define-key make-skeleton-mode-map "\C-c\C-e" 'make-skeleton-erase-buffer)
  )

;;}}

(provide 'incoming-config)

