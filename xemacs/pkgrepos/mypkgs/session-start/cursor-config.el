;;
;; cursor.el
;; Login : <s@taj>
;; Started on  Wed Sep 15 00:21:24 2010 Sharad Pratap
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



;;{{ from: http://www-sop.inria.fr/everest/Clement.Hurlin/linux.shtml
;; 05/08/07, emacs: I find useful to highlight the line (snapshot)
;; where the cursor points because it avoids getting lost when
;; switching between numerous windows. You can enable this with:
;; (hl-line-mode) ;; for the current buffer
;; (global-hl-line-mode) ;; for all buffers

;; http://www.emacswiki.org/emacs/hl-line%2b.el
;;  To use this library, put this in your Emacs init file (~/.emacs):
;;
(cond
  ((xrequire 'hl-line+) ; Load this file (it will load `hl-line.el')
   ;;
   ;;  To turn on `global-hl-line-mode' only when Emacs is idle, by
   ;;  default, add this line also to your init file:
   ;;
   ;; Highlight only when idle
   (hl-line-toggle-when-idle 1)
   (hl-line-when-idle-interval 70))
  ((xrequire 'hl-line)
   (hl-line-mode) ;; for the current buffer
   (global-hl-line-mode))) ;; for all buffers

(defun my-delete-timer ()
  (interactive)
  (dolist (timer timer-list)
    (let ()
      (when (yes-or-no-p (format "Remove timer: %s" timer))
        (message "removing timer %s" timer)
        (delete timer timer-list)
        (message "removed timer %s list is now %s" timer timer-list)))))

(provide 'cursor-config)
