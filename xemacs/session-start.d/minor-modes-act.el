;;; minor-modes-act.el ---

;; Copyright 2011 Sharad Pratap
;;
;; Author: sh4r4d@gmail.com
;; Version: $Id: minor-modes-act.el,v 0.0 2011/04/13 05:33:48 spratap Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:




(deh-require-maybe 'pabbrev
  (defun pabbrev-activate ()
    (unless buffer-read-only
      (pabbrev-mode 1)))
  (add-element-to-lists 'pabbrev-activate pgm-langs))

(when (and
       (executable-find "p4")
       (xrequire 'vc-p4))

  (if (and (setq vc-p4-require-p4config t)
           (not (getenv "P4CONFIG")))
      (setenv "P4CONFIG" ".p4conf"))

  (defun office-activate ()
    (let ((file (buffer-file-name)))
      (if (and file
               (with-timeout (4 nil) (vc-p4-registered file)))
          ;; if file is handled by perforce than assume it is
          ;; related to office perforce repository.
          (office-mode 1))))


  (if sharad-in-office-with-perforce
   (add-element-to-lists 'office-activate pgm-langs)))


;;; minor-modes-act.el ends here


(user-provide 'minor-modes-act)
