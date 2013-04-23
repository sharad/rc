;;
;; expand.el
;; Login : <s@taj>
;; Started on  Thu Dec  2 01:56:31 2010 Sharad Pratap
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

;; This file require work
;; resolve pabber yes-expand hippi-expan.....



(require 'template-simple)


;;; Actually TAB is originally binded to indent-for-tab-command from indent.el
;;; But Pabbrev mode override it to pabbrev-expand-maybe that call
;;; pabbrev-get-previous-binding -> indent-for-tab-command

;; M-SPC not available, window manager take it away

; (global-set-key-if-unbind (kbd "M-'") 'just-one-space)


(deh-require-maybe auto-complete
  (global-auto-complete-mode t))

(deh-require-maybe pde-indent-dwim
  (global-set-key-if-unbind (kbd "C-M-=") 'pde-indent-dwim))

(when (require 'comint)
  ;; Correct create comment!
  (global-set-key-if-unbind (kbd "M-;") 'comment-dwim)
  (global-set-key-if-unbind (kbd "C-c f") 'comint-dynamic-complete))

(deh-require-maybe yasnippet
  ;; inplace of tab I want it to use C->
  (setq yas/trigger-key "C->")
  ;; do not want it.
  ;; (setq yas/trigger-key "")
  )

;; TAB used for indent-for-tab-command


;; Left key C-TAB C-M-TAB

;; nearest key to dabbrev-expand
(deh-require-maybe hippie-exp
  (global-set-key-if-unbind (kbd "C-;") 'hippie-expand)
  (setq hippie-expand-try-functions-list
        '(yas/hippie-try-expand
          try-expand-list
          try-expand-list-all-buffers
          try-expand-dabbrev-visible
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-complete-file-name
          try-complete-file-name-partially
          try-complete-lisp-symbol
          try-complete-lisp-symbol-partially
          try-expand-whole-kill
          try-expand-dabbrev
          try-expand-line
          try-expand-line-all-buffers)))

;; (autoload 'comint-dynamic-complete "comint" "Complete for file name" t)

(setq comint-completion-addsuffix '("/" . ""))


;;{{
;; /usr/share/emacs23/site-lisp/dictionaries-common/flyspell.el
(defun yas/expandable-at-point ()
  "Return non-nil if a snippet can be expanded here."
  ;; (car (yas/current-key))
  (yas/current-key))

(defvar yas-overlays nil)
(make-variable-buffer-local 'yas-overlays)

(remove-hook 'post-command-hook (function yas-post-command-hook) )

(defun yas-check-word-p ()
  "Return t when the word at `point' has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
  (let ()
    (cond
     ((<= (- (point-max) 1) (point-min))
      ;; The buffer is not filled enough.
      nil)
     ((> (current-column) 1)
      (save-excursion
        (backward-char 2)
        (and
             (looking-at "[[:alpha:]][^[:alpha:]]")
             (forward-word 1)
             (cdr (yas/current-key)))))
      ;; Yes because we have reached or typed a word delimiter.
     (t nil))))

(defun yas-post-command-hook ()
  "The `post-command-hook' used by flyspell to check a word on-the-fly."
  (interactive)
  (when yas/minor-mode
    (with-local-quit

      ;; (let (o)
      ;;   (while (setq o (pop yas-overlays))
      ;;     (delete-overlay o)))

      (dolist (o yas-overlays)
        (delete-overlay o))
      (let ((command this-command)
            ;; Prevent anything we do from affecting the mark.
            deactivate-mark
            (positions (yas-check-word-p) )
            overlay)
        (if (and
             positions
             (null yas-overlays))
            (progn
              (push
               (apply 'make-yas-overlay (append positions '(highlight highlight)))
               yas-overlays)
              (backward-char 1))
            (setq yas-overlays nil))))))




(defun make-yas-overlay (beg end face mouse-face)
  "Allocate an overlay to highlight an incorrect word.
BEG and END specify the range in the buffer of that word.
FACE and MOUSE-FACE specify the `face' and `mouse-face' properties
for the overlay."
  (let ((overlay (make-overlay beg end nil t nil)))
    (overlay-put overlay 'face face)
    (overlay-put overlay 'mouse-face mouse-face)
    (overlay-put overlay 'flyspell-overlay t)
    (overlay-put overlay 'evaporate t)
    (overlay-put overlay 'help-echo "mouse-2: yas/expansion at point")
    ;; (overlay-put overlay 'keymap yas-mouse-map)
    ;; (when (eq face 'flyspell-incorrect)
    ;;   (and (stringp flyspell-before-incorrect-word-string)
    ;;        (overlay-put overlay 'before-string
    ;;                     flyspell-before-incorrect-word-string))
    ;;   (and (stringp flyspell-after-incorrect-word-string)
    ;;        (overlay-put overlay 'after-string
    ;;                     flyspell-after-incorrect-word-string)))
    overlay))
;;}}

(provide 'expand-config)
