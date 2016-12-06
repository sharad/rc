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
 x-select-enable-primary t
 ;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
 select-active-regions 'only)

;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
;; (global-set-key [mouse-2] 'mouse-yank-primary)

(deh-require-or-package-install light-symbol
  ;; ;; http://stackoverflow.com/a/385676/341107
  ;; (add-element-to-lists 'light-symbol-mode pgm-langs)
  ;; ;; (light-symbol-mode 1) - not works
  )

(when nil                               ;e.g.
  (deh-require-or-act light-symbol
    (lambda (p) (package-install p))))

(deh-require-maybe hilit-chg
  ;; (add-element-to-lists '(lambda ()
  ;;                         (light-symbol-mode 1)
  ;;                         (highlight-changes-visible-mode t)
  ;;                         (highlight-changes-mode t)) pgm-langs)
  ;; (highlight-changes-mode t) - not works
  ;; (highlight-changes-visible-mode t)

  ;;{{
  ;; http://www.emacswiki.org/emacs/TrackChanges
  (make-empty-face 'highlight-changes-saved-face)
  (setq highlight-changes-face-list '(highlight-changes-saved-face))

                                        ; Example: activate highlight changes with rotating faces for C programming
  (add-hook 'c-mode-hook
            (function (lambda ()
              (add-hook 'local-write-file-hooks 'highlight-changes-rotate-faces)
              (highlight-changes-mode t)
              ;; (... other stuff for setting up C mode ...)
              )))
  ;;}}

  ;;{{
  (defun DE-highlight-changes-rotate-faces ()
    (let ((toggle (eq highlight-changes-mode 'passive)))
      (when toggle (highlight-changes-mode t))
      (highlight-changes-rotate-faces)
      (when toggle (highlight-changes-mode nil))))

                                        ; Example for c-mode-hook:
  (add-hook 'c-mode-hook
            (function (lambda ()
              (add-hook 'local-write-file-hooks 'DE-highlight-changes-rotate-faces)
              (highlight-changes-mode t)
              (highlight-changes-mode nil)
              ;; (... other stuff for setting up C mode ...)
              )))
  ;;}}

  ;;{{
  ;; Following function can make the highlight vanish after save file --coldnew
  (defun highlight-changes-remove-after-save ()
    "Remove previous changes after save."
    (make-local-variable 'after-save-hook)
    (add-hook 'after-save-hook
              (lambda ()
		(highlight-changes-remove-highlight (point-min) (point-max)))))

  ;;}}

  )


(deh-require-maybe show-wspace
  ;; http://emacswiki.org/emacs/ShowWhiteSpace
    )


(deh-require-maybe subword
  ;; (global-subword-mode 1)
  (add-element-to-lists '(lambda nil (subword-mode 1)) pgm-langs))


(deh-require-maybe paren
  ;; Hilight matching parenthesis
  ;; (unless (featurep 'xemacs) (show-paren-mode 1))
  (show-paren-mode 1)

  ;; http://www.emacswiki.org/emacs/ShowParenMode#toc1
  (defadvice show-paren-function (after show-matching-paren-offscreen activate)
    "If the matching paren is offscreen, show the matching line in the
        echo area. Has no effect if the character before point is not of
        the syntax class ')'."
    (interactive)
    (let* ((cb (char-before (point)))
           (matching-text (and cb
                               (char-equal (char-syntax cb) ?\) )
                               (blink-matching-open))))
      (when matching-text (message matching-text)))))


(deh-require-maybe corral
  ;; (https://github.com/nivekuil/corral)
  (global-set-key-if-unbind (kbd "M-9") 'corral-parentheses-backward)
  (global-set-key-if-unbind (kbd "M-0") 'corral-parentheses-forward)
  (global-set-key-if-unbind (kbd "M-[") 'corral-brackets-backward)
  (global-set-key-if-unbind (kbd "M-]") 'corral-brackets-forward)
  (global-set-key-if-unbind (kbd "M-{") 'corral-braces-backward)
  (global-set-key-if-unbind (kbd "M-}") 'corral-braces-forward)
  (global-set-key-if-unbind (kbd "M-\"") 'corral-double-quotes-backward))

(provide 'editing-config)
