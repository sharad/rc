;;; packages.el --- LAYER layer packages file for Spacemacs.
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
;; added to `LAYER-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `LAYER/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `LAYER/pre-init-PACKAGE' and/or
;;   `LAYER/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-testing-packages
  '(
    (ert :location local)
    ertx
    (ert-x :location local)
    (ert-async :location local)
    (ert-expectations :location local)
    ert-junit
    ert-modeline
    ert-runner
    fakir
    )
  "The list of Lisp packages required by the LAYER layer.

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

(defun lotus-testing/init-ert ()
  (use-package ert
    :defer t
    :config
    (progn
      )))

(defun lotus-testing/init-ertx ()
  (use-package ertx
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-x ()
  (use-package ert-x
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-async ()
  (use-package ert-async
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-expectations ()
  (use-package ert-expectations
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-junit ()
  (use-package ert-junit
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-modeline ()
  (use-package ert-modeline
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-ert-runner ()
  (use-package ert-runner
    :defer t
    :config
    (progn)))

(defun lotus-testing/init-fakir ()
  (use-package fakir
    :defer t
    :config
    (progn)))

;;; packages.el ends here
