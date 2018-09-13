;;; packages.el --- lotus-annotation layer packages file for Spacemacs.
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
;; added to `lotus-annotation-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-annotation/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-annotation/pre-init-PACKAGE' and/or
;;   `lotus-annotation/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-annotation-packages
  '(
    ipa
    org-pua
    alert
    org-annotate-file
    (annot :location local)
    )
  "The list of Lisp packages required by the lotus-annotation layer.

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

(defun lotus-annotation/init-ipa ()
  (use-package ipa
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-annotation/init-org-pua ()
  (use-package org-pua
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-annotation/init-alert ()
  (use-package alert
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-annotation/init-annot ()
  (use-package annot
      :defer t
      :config
      (progn
        ;; TODO: After adding annotation add it to a org file with link back to place where annotation were added.
        ;; it could help to search annotation when the original place is forgotten.
        ;; https://code.google.com/p/annot/
        ;; * [C-x a]    -  add a new annotation/highlight or edit an existing annotation on point.
        ;;                 You can also use [C-x C-a]. (annot-edit/add)
        ;; * [C-x r]    -  remove annotation at point. (annot-remove)
        ;; * [C-x w]    -  insert an image at point. (annot-add-image)
        (setq
         ;; annot-image-directory
         annot-directory (auto-config-dir "annot/" t)
         annot-enable-symlinking t)

        ;; (define-key ctl-x-map "a"    'annot-edit/add)
        ;; (define-key ctl-x-map "\C-a" 'annot-edit/add)
        ;; (define-key ctl-x-map "r"    'annot-remove)
        ;; (define-key ctl-x-map "w"    'annot-add-image)
        ;; (define-key ctl-x-map "A"    'annot-convert)
        )))

(defun lotus-annotation/init-ipa ()
  (use-package ipa
      :defer t
      :config
      (progn
        (progn
          ))))

;;; packages.el ends here
