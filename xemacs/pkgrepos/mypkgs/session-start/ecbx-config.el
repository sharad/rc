;;
;; ecb.el
;; Login : <s@taj>
;; Started on  Sun Jun  6 11:31:53 2010 Sharad Pratap
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

;; (mycustom-face-set)

(deh-require-maybe ecb
  (setq ecb-auto-activate nil
        ecb-tip-of-the-day nil)
  (deh-require-maybe speedbar
    (setq  ecb-use-speedbar-instead-native-tree-buffer nil))

;; Because ECB is a global minor-mode it can also be (de)activated/toggled by M-x ecb-minor-mode.

  ;; (add-element-to-lists 'ecb-minor-mode pgm-langs)
  ;; (add-element-to-lists 'ecb-toggle-ecb-windows pgm-langs)
  (setq ecb-toggle-layout-sequence
        '("sharad-leftright-analyse-etc"
          "sharad-leftright-analyse-etc-reverse"
          ; "left9"
          ; "left14"
          ; "sharad-leftright-analyse"
          )
        ecb-other-window-behavior 'smart ;'all
        )

;; http://www.xemacs.org/Documentation/packages/html/ecb_8.html#SEC140
  (deh-require-maybe winring
    ;; winring.el
    (ecb-winman-winring-enable-support)
    (winring-initialize))

  ;; (add-hook 'after-make-frame-functions
  ;;           #'(lambda (x)
  ;;               (unless ecb-minor-mode
  ;;                 (ecb-activate))) t)
  )



(provide 'ecbx-config)

