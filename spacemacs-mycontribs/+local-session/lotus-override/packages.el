;;; packages.el --- lotus-override layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: s <>
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
;; added to `lotus-override-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-override/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-override/pre-init-PACKAGE' and/or
;;   `lotus-override/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-override-packages
  '(
    emacsql-sqlite
    (lsdb    :location local)
    git-gutter+
    git-link
    org-agenda
    vc
    vc-hooks
    vc-git
    )
  "The list of Lisp packages required by the lotus-override layer.

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

(defun lotus-override/init-emacsql-sqlite ()
  (use-package emacsql-sqlite
    :defer t
    :config
    (lotus-override/init-emacsql-sqlite-config)))

(defun lotus-override/init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (lotus-override/init-lsdb-config)))))

(defun lotus-override/post-init-git-gutter+ ()
  (use-package git-gutter+
    :defer t
    :config
    (progn
      (progn
        (lotus-override/post-init-git-gutter+-config)))))

(defun lotus-override/post-init-git-link ()
  (use-package git-link
    :defer t
    :config
    (progn
      (progn
        (lotus-override/post-init-git-link-config)))))

(defun lotus-override/post-init-org-agenda ()
  (use-package org-agenda-hooks
    :defer t
    :config
    (lotus-override/post-init-org-agenda-config)))

(defun lotus-override/post-init-vc ()
  (use-package vc
    :defer t
    :config
    (lotus-override/post-init-vc-config)))

(defun lotus-override/post-init-vc-hooks ()
  (use-package vc-hooks
    :defer t
    :config
    (lotus-override/post-init-vc-hooks-config)))

(defun lotus-override/post-init-vc-git ()
  (use-package vc-git
    :defer t
    :config
    (lotus-override/post-init-vc-git-config)))

