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
    thing-cmds
    thing-edit
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
                    (car boundary-pair) (cdr boundary-pair)))))))))

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

;;; packages.el ends here
