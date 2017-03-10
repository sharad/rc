;;; packages.el --- lotus-comment layer packages file for Spacemacs.
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
;; added to `lotus-comment-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-comment/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-comment/pre-init-PACKAGE' and/or
;;   `lotus-comment/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-comment-packages
  '(
    (newcomment :location local)
    hide-comnt
    )
  "The list of Lisp packages required by the lotus-comment layer.

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

(defun lotus-comment/init-newcomment ()
  (use-package newcomment
      :defer t
      :config
      (progn
        (defun comment-and-go-down ()
          "Comments the current line and goes to the next one" (interactive)
          (condition-case nil (comment-region (point-at-bol) (point-at-eol)) (error nil))
          (next-line 1))
        (defun uncomment-and-go-up ()
          "Uncomments the current line and goes to the previous one" (interactive)
          (condition-case nil (uncomment-region (point-at-bol) (point-at-eol)) (error nil))
          (next-line -1))
        ;shift-down comments the current line and goes down
        ;(define-key global-map [(shift down)] 'comment-and-go-down)
        ;shift-up uncomments the current line and goes up
        ;(define-key global-map [(shift up)] 'uncomment-and-go-up)
        )))


(defun lotus-comment/post-init-hide-comnt ()
  (use-package hide-comnt
      :defer t
      :config
      (progn
        )))



;;; packages.el ends here
