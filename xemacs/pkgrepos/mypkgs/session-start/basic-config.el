;;
;; basic.el
;; Login : <s@taj>
;; Started on  Sun Jun  6 11:18:12 2010 Sharad Pratap
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

;;{{{ start: http://lcavwww.epfl.ch/~ridolfi/personal/linuxstuff/.emacs
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;			Basic Customization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;====================================================================
;; MACROS
;; Some macros.


;;{{{ start: http://emacs-fu.blogspot.com/2008/12/using-packages-functions-only-if-they.html

(eval-when-compile
  (require 'cl))
;; If you place the macros somewhere in the beginning of your
;; .emacs, you can use them as follows (just some examples):
;; change cursor color based on mode (insert/overwrite)
(when (require-maybe 'cursor-chg)  ; Load this library
  (change-cursor-mode 1) ; On for overwrite/read-only/input mode
  (toggle-cursor-type-when-idle 1)) ; On when idle
;; and
(when-available 'set-fringe-mode  ; emacs22+
  (set-fringe-mode 2))            ; don't have too much space left of col1
;;}}}


(require 'startup-hooks)

(add-hook 'after-make-frame-functions '(lambda (frame)
                                        (message
                                         "Frame %s Ready for editing!"
                                         (frame-parameter frame 'name))) t)

(provide 'basic-config)
;;; basic-config.el ends here
