;;
;; display.el
;; Login : <spratap@spratap>
;; Started on  Wed Sep 15 11:59:42 2010 Sharad Pratap
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


;; Must see

;; http://www.emacswiki.org/emacs/SetFonts
;; http://www.emacswiki.org/emacs/CategoryDisplay
;; http://www.emacs.uniyar.ac.ru/doc/em24h/emacs098.htm



;;;; font size
;; (x-display-pixel-width) 1920
;; (x-display-pixel-height) 1080
;; (x-display-mm-width)  508
;; (x-display-mm-height) 286

;; set attributes
(defun mycustom-face-set ()
  "thisandthat."
  (interactive)
  (set-face-attribute 'default nil ;(/ (* (x-display-mm-width) 121) 600)
                      :height (/ (* (x-display-mm-height) 150) 400)
                      :width  'normal))


;; (mycustom-face-set)
;;:font FONT)
;; get attributes
;; (face-attribute 'default :font)
;; (face-attribute 'default :height)


(provide 'display-config)

