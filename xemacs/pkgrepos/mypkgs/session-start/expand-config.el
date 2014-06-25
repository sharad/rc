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
  (define-key ac-mode-map (kbd "<C-M-tab>") 'auto-complete)
  (global-auto-complete-mode t)

  (defun my-c-mode-cedet-hook ()
    ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
    (add-to-list 'ac-sources 'ac-source-gtags)
    (add-to-list 'ac-sources 'ac-source-semantic))
  (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)


  (deh-section "config autocomplete"
    ;; http://root42.blogspot.hu/2012/07/nice-c-autocomplete-configuration-for.html
    (defcustom mycustom-system-include-paths '("./include/" "/opt/local/include" "/usr/include" )
      "This is a list of include paths that are used by the clang auto completion."
      :group 'mycustom
      :type '(repeat directory))

    ;; (add-to-list 'load-path "~/bin/emacs/auto-complete")
    (deh-require-maybe auto-complete-config
      ;; (add-to-list 'ac-dictionary-directories "~/bin/emacs/auto-complete/ac-dict")
      (ac-config-default)
      (deh-require-maybe auto-complete-clang
        (setq clang-completion-suppress-error 't)
        (setq ac-clang-flags
              (mapcar '(lambda (item)
                        (concat "-I" item))
                      (append mycustom-system-include-paths)))

        (defun my-ac-clang-mode-common-hook ()
          (define-key c-mode-base-map (kbd "M-/") 'ac-complete-clang))

        (add-hook 'c-mode-common-hook 'my-ac-clang-mode-common-hook)))))


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
  (global-set-key-if-unbind (kbd "<C-tab>") 'hippie-expand)
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




(deh-section "yas/info"
  ;;{{
  ;; /usr/share/emacs23/site-lisp/dictionaries-common/flyspell.el
  (defun yas/expandable-at-point ()
    "Return non-nil if a snippet can be expanded here."
    ;; (car (yas/current-key))
    (yas/current-key))

  (defvar yas-overlays nil)
  (make-variable-buffer-local 'yas-overlays)

  ;; (defun showthiscommand ()
  ;;   (message "this-command %s" this-command))

  ;; (remove-hook 'post-command-hook 'showthiscommand)

  (defun yas/word-key (word)
    "Get the key under current position. A key is used to find
the template of a snippet in the current snippet-table."
    (let ((start (point))
          (end (point))
          (syntaxes yas/key-syntaxes)
          syntax
          done
          templates)
      (while (and (not done) syntaxes)
        (setq syntax (car syntaxes))
        (setq syntaxes (cdr syntaxes))
        (save-excursion
          (skip-syntax-backward syntax)
          (setq start (point)))
        (setq templates
              (mapcan #'(lambda (table)
                          (yas/fetch table word))
                      (yas/get-snippet-tables)))
        (if templates
            (setq done t)
            (setq start end)))
      (list templates
            start
            end)))


  (defun yas-check-word-p ()
    "Return t when the word at `point' has to be checked.
The answer depends of several criteria.
Mostly we check word delimiters."
    (and
     (not (<= (- (point-max) 1) (point-min)))
     (> (current-column) 1)
     (let ((cc (char-after (point)))
           (pc (char-after (1- (point)))))
       (and (char-isalnum-p pc)
            (not (char-isalnum-p cc))))
     (let ((con (yas/current-key)))
       (if (car con)
           (cdr con)))))

  (defun yas-post-command-hook ()
    "The `post-command-hook' used by flyspell to check a word on-the-fly."
    (interactive)
    (when yas/minor-mode
      (with-local-quit
        (when (> (length yas-overlays) 2)
          (delete-overlay (pop yas-overlays)))
        (when (or
               (eq (aref (this-command-keys) 0) ?\ )
               (eq this-command #'self-insert-command)
               (eq this-command #'tempo-x-space))
          (let (deactivate-mark ;; Prevent anything we do from affecting the mark.
                (positions (yas-check-word-p)))
            (when positions
              (let ((yas-overlay (apply 'make-yas-overlay (append positions '(highlight highlight)))))
                (setq yas-overlays (nconc yas-overlays (list yas-overlay)))
                (message "%s %d" yas-overlay (length yas-overlays)))))))))

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

(define-minor-mode yas-suggest-mode
    "Prepare for working with collarative office project."
  :init-value 1
  :lighter nil ;; " rl"
  :global nil
  (if yas-suggest-mode
      (add-hook 'post-command-hook (function yas-post-command-hook) )
      (progn
        (dolist (o yas-overlays)
          (delete-overlay o))
        (setq yas-overlays nil)
       (remove-hook 'post-command-hook (function yas-post-command-hook)))))

(defun yas-suggest-activate ()
  (yas-suggest-mode 1))

(add-element-to-lists 'yas-suggest-activate pgm-langs))


(deh-section "pabber"
  (setq pabbrev-read-only-error t)

  (deh-section "desktop-settings"


    ;; (defmacro desktop-get-readonly-proof-mode (modefn)
    ;;   `(defun ,(intern (concat "desktop-handler-" (symbol-name modefn))) (desktop-buffer-locals)
    ;;      (unless desktop-buffer-read-only
    ;;        (,modefn 1))))

  (eval-when-compile
   '(require 'session-config))

    (require 'session-config)

    ;; (defun desktop-handler-pabbrev-mode (desktop-buffer-locals)
    ;;   (unless desktop-buffer-read-only
    ;;     (pabbrev-mode 1)))
    ;; (desktop-get-readonly-proof-mode pabbrev-mode)
    ;; (add-to-list 'desktop-minor-mode-handlers '(pabbrev-mode . desktop-handler-pabbrev-mode))
    ;; (desktop-get-readonly-proof-mode flymake-mode)
    ;; (add-to-list 'desktop-minor-mode-handlers '(flymake-mode . desktop-handler-flymake-mode))

    (add-to-list 'desktop-minor-mode-handlers (cons 'pabbrev-mode
                                                    (desktop-get-readonly-proof-mode pabbrev-mode)))))




(provide 'expand-config)
