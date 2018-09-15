;;; packages.el --- lotus-things layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-things-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-things/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-things/pre-init-PACKAGE' and/or
;;   `lotus-things/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-thingsS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-things-packages
  '(
    thingatpt
    (thing-cmdslotus-todo/)
    (thing-edit :location local)
    (copywithoutsel :location local)
    )
  "The list of Lisp packages required by the lotus-things layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-things/init-thingatpt ()
  (use-package thingatpt
      :defer t
      :config
      (progn
        ;;   Email addresses
        (defvar thing-at-point-fullemail-regexp
          "\\([a-zA-Z]+ \\)\\{1,2\\}<?[-+_.~a-zA-Z][-+_.~:a-zA-Z0-9]*@[-.a-zA-Z0-9]+>?"
          "A regular expression probably matching an email address.
This does not match the real name portion, only the address, optionally
with angle brackets.")

        ;; Haven't set 'forward-op on 'email nor defined 'forward-email' because
        ;; not sure they're actually needed, and URL seems to skip them too.
        ;; Note that (end-of-thing 'email) and (beginning-of-thing 'email)
        ;; work automagically, though.

        (put 'fullemail 'bounds-of-thing-at-point
             (lambda ()
               (let ((thing (thing-at-point-looking-at thing-at-point-fullemail-regexp)))
                 (if thing
                     (let ((beginning (match-beginning 0))
                           (end (match-end 0)))
                       (cons beginning end))))))

        (put 'fullemail 'thing-at-point
             (lambda ()
               (let ((boundary-pair (bounds-of-thing-at-point 'fullemail)))
                 (if boundary-pair
                     (buffer-substring-no-properties
                      (car boundary-pair) (cdr boundary-pair)))))))

      (progn
        ;; from: http://emacswiki.org/emacs/CopyWithoutSelection

        ;; Introduction

        ;; The following functions will copy the word, the line, the
        ;; paragraph and the string and the Parenthesis at point, and will
        ;; paste them to the mark. If it is used in shell-mode, stuff will
        ;; be pasted at end of shell prompt.

        ;; Give a prefix 1 will prevent paste.

        ;; And besides, there are also something to do with some basic
        ;; shortcuts.  Selecting words or sexps without moving the cursor

        ;; You can select word or sexp after point and put it in the
        ;; kill-ring by typing:

        ;;          C-M-SPC M-w

        ;; This does not move the cursor. If you want to select the next two
        ;; words after point, just type C-M-SPC C-M-SPC M-w. Killing next
        ;; word or sexp can be done with C-M-SPC C-w, or better with C-M-k.
        ;; the base functions

        (defun get-point (symbol &optional arg)
          "get the point"
          (funcall symbol arg)
          (point)
          )

        (defun copy-thing (begin-of-thing end-of-thing &optional arg)
          "copy thing between beg & end into kill ring"
          (save-excursion
            (let ((beg (get-point begin-of-thing 1))
                  (end (get-point end-of-thing arg)))
              (copy-region-as-kill beg end)))
          )

        (defun paste-to-mark(&optional arg)
          "Paste things to mark, or to the prompt in shell-mode"
          (let ((pasteMe
                 (lambda()
                   (if (string= "shell-mode" major-mode)
                       (progn (comint-next-prompt 25535) (yank))
                       (progn (goto-char (mark)) (yank) )))))
            (if arg
                (if (= arg 1)
                    nil
                    (funcall pasteMe))
                (funcall pasteMe))
            ))


        ;; Copy Word

        (defun copy-word (&optional arg)
          "Copy words at point into kill-ring"
          (interactive "P")
          (copy-thing 'backward-word 'forward-word arg)
          ;;(paste-to-mark arg)
          )

        ;; Key binding

        (global-set-key (kbd "C-c w")         (quote copy-word))

        ;; Copy Line

        (defun copy-line (&optional arg)
          "Save current line into Kill-Ring without mark the line "
          (interactive "P")
          (copy-thing 'beginning-of-line 'end-of-line arg)
          (paste-to-mark arg)
          )

        ;; Key binding

        ;; (global-set-key (kbd "C-c l")         (quote copy-line))

        ;; Tip: WholeLineOrRegion copies the current line when the region is
        ;; not active. Thus, C-a M-w C-y C-y … duplicates the current
        ;; line (similar to vi’s ‘yyp’), while C-w deletes it (‘dd’). When
        ;; the region is active, M-w and C-w act as usual. :) Copy Paragraph

        (defun copy-paragraph (&optional arg)
          "Copy paragraphes at point"
          (interactive "P")
          (copy-thing 'backward-paragraph 'forward-paragraph arg)
          (paste-to-mark arg)
          )

        ;; key binding

        ;; (global-set-key (kbd "C-c p")         (quote copy-paragraph))

        ;; Copy String

        (defun beginning-of-string(&optional arg)
          "  "
          (re-search-backward "[ \t]" (line-beginning-position) 3 1)
          (if (looking-at "[\t ]")  (goto-char (+ (point) 1)) )
          )
        (defun end-of-string(&optional arg)
          " "
          (re-search-forward "[ \t]" (line-end-position) 3 arg)
          (if (looking-back "[\t ]") (goto-char (- (point) 1)) )
          )

        (defun thing-copy-string-to-mark(&optional arg)
          " Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
          (interactive "P")
          (copy-thing 'beginning-of-string 'end-of-string arg)
          (paste-to-mark arg)
          )


        ;; Key binding

        (global-set-key (kbd "C-c s")         (quote thing-copy-string-to-mark))

        ;; Copy Parenthesis

        ;; Similar with new copy string to mark

        (defun beginning-of-parenthesis(&optional arg)
          "  "
          (re-search-backward "[[<(?\"]" (line-beginning-position) 3 1)
          (if (looking-at "[[<(?\"]")  (goto-char (+ (point) 1)) )
          )
        (defun end-of-parenthesis(&optional arg)
          " "
          (re-search-forward "[]>)?\"]" (line-end-position) 3 arg)
          (if (looking-back "[]>)?\"]") (goto-char (- (point) 1)) )
          )

        (defun thing-copy-parenthesis-to-mark(&optional arg)
          " Try to copy a parenthesis and paste it to the mark
     When used in shell-mode, it will paste parenthesis on shell prompt by default "
          (interactive "P")
          (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
          (paste-to-mark arg)
          )
        ;; (global-set-key (kbd "C-c a")         (quote thing-copy-parenthesis-to-mark))
        )))

(defun lotus-things/init-thing-cmds ()
  (use-package thing-cmds
    :defer t
    :config
    (progn
      )))

(defun lotus-things/init-thing-edit ()
  (use-package thing-edit
    :defer t
    :config
    (progn
      )))

(defun lotus-things/init-copywithoutsel ()
  (use-package copywithoutsel
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
