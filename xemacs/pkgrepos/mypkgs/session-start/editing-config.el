;;
;; editing.el
;; Login : <spratap@spratap>
;; Started on  Fri Apr  8 14:12:07 2011 Sharad Pratap
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


;; see http://www.emacswiki.org/emacs/DeleteSelectionMode
;; also try http://www.emacswiki.org/emacs/delsel.el

(delete-selection-mode 1)


(setq
 x-select-enable-clipboard t
 x-select-enable-primary t)

(deh-require-maybe light-symbol
  ;; ;; http://stackoverflow.com/a/385676/341107
  ;; (add-element-to-lists 'light-symbol-mode pgm-langs)
  ;; ;; (light-symbol-mode 1) - not works
  )


(deh-require-maybe hilit-chg
  ;; (add-element-to-lists '(lambda ()
  ;;                         (light-symbol-mode 1)
  ;;                         (highlight-changes-visible-mode t)
  ;;                         (highlight-changes-mode t)) pgm-langs)
  ;; (highlight-changes-mode t) - not works
  ;; (highlight-changes-visible-mode t)
  )


(deh-require-maybe show-wspace
  ;; http://emacswiki.org/emacs/ShowWhiteSpace
    )

(provide 'editing-config)

