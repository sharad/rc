;;
;; mozrepl.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Tue Sep 14 17:38:02 2010 Sharad Pratap
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




;; Add this to your .emacs file:

(autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)

(xrequire 'moz)

(defun javascript-custom-setup ()
  (setq moz-repl-port 4747)
  (moz-minor-mode 1))

;; (defun javascript-custom-setup ()
;;   (deh-require-maybe moz
;;     `(moz-minor-mode 1))
;;
;;     )

(add-hook 'js-mode-hook 'javascript-custom-setup)

(deh-require-maybe flymake-js
  (add-hook 'javascript-mode-hook 'flymake-jslint-load))
(add-hook 'javascript-mode-hook 'javascript-custom-setup)
(add-hook 'inferior-moz-hook 'javascript-custom-setup)

;; Or if you’re trying espresso.el, add this:
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
(deh-require-maybe (and espresso flymake-js)
  ;; (add-hook 'espresso-mode-hook 'espresso-custom-setup)
  (add-hook 'espresso-mode-hook 'flymake-jslint-load)
  (add-hook 'espresso-mode-hook 'javascript-custom-setup)
  ;; (defun espresso-custom-setup ()
  ;;   (moz-minor-mode 1))
  ;; from: http://www.nongnu.org/espresso/
  (add-to-list 'auto-mode-alist '("\\.js$" . espresso-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . espresso-mode)))

;; Or if you’re using js2-mode, add this:
;; (autoload 'moz-minor-mode "moz" "Mozilla Minor and Inferior Mozilla Modes" t)
;; (add-hook 'js2-mode-hook 'js2-custom-setup)
(deh-require-maybe flymake-js
  (add-hook 'js2-mode-hook 'flymake-jslint-load))
(add-hook 'js2-mode-hook 'javascript-custom-setup)
;; (defun js2-custom-setup ()
;;   (moz-minor-mode 1))

;; C-c C-s: open a MozRepl interaction buffer and switch to it
;; C-c C-l: save the current buffer and load it in MozRepl
;; C-M-x: send the current function (as recognized by c-mark-function) to MozRepl
;; C-c C-c: send the current function to MozRepl and switch to the interaction buffer
;; C-c C-r: send the current region to MozRepl
;;
;; In the interaction buffer:
;;
;; C-c c: insert the current name of the REPL plus the dot operator (usually repl.)

(provide 'mozrepl-config)
