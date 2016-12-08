;;
;; doc.el
;; Login : <s@taj>
;; Started on  Thu Sep 16 14:13:55 2010 Sharad Pratap
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



;; eldoc-mode is an interactive compiled Lisp function in `eldoc.el'.

;; (eldoc-mode &optional ARG)

;; Toggle ElDoc mode on or off.
;; In ElDoc mode, the echo area displays information about a
;; function or variable in the text where point is.  If point is
;; on a documented variable, it displays the first line of that
;; variable's doc string.  Otherwise it displays the argument list
;; of the function called in the expression point is on.

;; With prefix ARG, turn ElDoc mode on if and only if ARG is positive.

;; Major modes for other languages may use ElDoc by defining an
;; appropriate function as the buffer-local value of
;; `eldoc-documentation-function'.




(defvar configuration|common|doc-config|package-list nil)




;;;###autoload
(defun configuration|common|doc-config|eldoc|config ()
  (use-package eldoc
      :defer t
      :config
      (use-package eldoc-extension
          :defer t)

      (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
      (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
      (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)
      (use-package c-eldoc             ;for c http://www.emacswiki.org/emacs/CEldocMode#toc5
          :defer t
          :config
          (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
          (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
          (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ "))

      (use-package cperl-mode          ;for perl http://www.emacswiki.org/emacs/CPerlMode#toc10
          :defer t
          :config
          (defun my-cperl-eldoc-documentation-function ()
            "Return meaningful doc string for `eldoc-mode'."
            (car
             (let ((cperl-message-on-help-error nil))
               (cperl-get-help))))

          (add-hook 'cperl-mode-hook
                    (lambda ()
                      (set (make-local-variable 'eldoc-documentation-function)
                           'my-cperl-eldoc-documentation-function))))))
;;;###autoload
(defun configuration|common|doc-config|eldoc|init ()
  (configuration|common|doc-config|eldoc|config)
  )
(push 'eldoc configuration|common|doc-config|package-list)



;;;###autoload
(defun configuration|common|doc-config|config ()
  )


;;;###autoload
(defun configuration|common|doc-config|init ()
  (configuration|common|doc-config|config)
  )
;; good
(push 'clweb configuration|common|doc-config|package-list)
;;;###autoload
(defun configuration|common|doc-config|packages ()
  configuration|common|doc-config|package-list
  )


(provide 'doc-config)
