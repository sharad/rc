;;
;; buffer.el
;; Login : <s@taj>
;; Started on  Tue Jul 27 00:19:15 2010 Sharad Pratap
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


;; see http://code.google.com/p/maoxian/source/browse/trunk/lisps/elscreen/elscreen-w3m.el?r=86
;; see elscreen-w3m
;; elscreen-set-mode-to-nickname-alist is a compiled Lisp function.
;; (elscreen-set-mode-to-nickname-alist MODE-TO-NICKNAME-ALIST-SYMBOL)



;; I need it badly
(deh-require-maybe elscreen-server

;; (defun elscreen-swap ()
;;   "Interchange screens selected currently and previously."
;;   (interactive)
;;   (cond
;;    ((elscreen-one-screen-p)
;;     (elscreen-message "There is only one screen, cannot swap"))
;;    (t
;;     (let* ((current-screen (elscreen-get-current-screen))
;;            (previous-screen (elscreen-get-previous-screen))
;;            (current-screen-property
;;             (elscreen-get-screen-property current-screen))
;;            (previous-screen-property
;;             (elscreen-get-screen-property previous-screen)))
;;       (elscreen-set-screen-property current-screen previous-screen-property)
;;       (elscreen-set-screen-property previous-screen current-screen-property)
;;       (elscreen-goto-internal (elscreen-get-current-screen))))))

  (defun elscreen-move-right ()
    (interactive)
    (elscreen-next)
    (elscreen-swap)
    (elscreen-notify-screen-modification))

  (defun elscreen-move-left ()
    (interactive)
    (elscreen-previous)
    (elscreen-swap)
    ;; (elscreen-next)
    (elscreen-notify-screen-modification))
  )

;; thanks http://www.emacswiki.org/emacs-pt/EmacsLispScreen ElScreen-server


(provide 'screen-config)







