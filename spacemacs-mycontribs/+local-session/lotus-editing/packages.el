;;; packages.el --- lotus-editing layer packages file for Spacemacs.
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
;; added to `lotus-editing-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-editing/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-editing/pre-init-PACKAGE' and/or
;;   `lotus-editing/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-editingS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org


;; http://www.emacswiki.org/emacs/CopyAndPaste#toc5
;; (global-set-key [mouse-2] 'mouse-yank-primary)

(defconst lotus-editing-packages '((common-win :location local)
                                   (light-symbol :location local)
                                   hilit-chg
                                   highlight-symbol
                                   ;; idle-highlight-mode
                                   symbol-overlay
                                   (show-wspace :location local)
                                   paren
                                   corral
                                   (autorevert :location local)
                                   (simple :location local)
                                   parinfer)
  "The list of Lisp packages required by the lotus-editing layer.

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

(defun lotus-editing/init-common-win ()
  (use-package common-win
    :defer t
    :init
    (lotus-editing/init-common-win-init)
    :config
    (lotus-editing/init-common-win-config)))

(defun lotus-editing/init-light-symbol ()
  (use-package light-symbol
    :defer t
    :init
    (lotus-editing/init-light-symbol-init)
    :config
    (lotus-editing/init-light-symbol-config)))

(defun lotus-editing/init-hilit-chg ()
  (use-package hilit-chg
    :defer t
    :init
    (lotus-editing/init-hilit-chg-init)
    :config
    (lotus-editing/init-hilit-chg-config)))

(defun lotus-editing/init-highlight-symbol ()
  (use-package highlight-symbol
    :defer t
    :init
    (lotus-editing/init-highlight-symbol-init)
    :config
    (lotus-editing/init-highlight-symbol-config)))

(defun lotus-editing/init-symbol-overlay ()
  ;; https://stackoverflow.com/questions/385661/how-to-highlight-all-occurrences-of-a-word-in-an-emacs-buffer
  (use-package symbol-overlay
    :defer t
    :init
    (lotus-editing/post-init-symbol-overlay-init)
    :config
    (lotus-editing/post-init-symbol-overlay-config)))

(defun lotus-editing/init-show-wspace ()
  ;; http://emacswiki.org/emacs/ShowWhiteSpace
  (use-package show-wspace
    :defer t
    :init
    (lotus-editing/init-show-wspace-init)
    :config
    (lotus-editing/init-show-wspace-config)))

(defun lotus-editing/init-paren ()
  (use-package paren
    :defer t
    :init
    (lotus-editing/init-paren-init)
    :config
    (lotus-editing/init-paren-config)))

(defun lotus-editing/init-corral ()
  ;; https://github.com/nivekuil/corral
  (use-package corral
    :defer t
    :config
    (progn)))


(defun lotus-editing/init-autorevert ()
  (use-package autorevert
    :defer t
    :init
    (lotus-editing/init-autorevert-init)
    :config
    (lotus-editing/init-autorevert-config)))

(defun lotus-editing/init-simple ()
  (use-package simple
    :defer t
    :init
    (lotus-editing/init-simple-init)
    :config
    (lotus-editing/init-simple-config)))

(defun lotus-editing/post-init-parinfer ()
  (use-package parinfer
    :defer t
    :init
    (lotus-editing/post-init-parinfer-init)
    :config
    (lotus-editing/post-init-parinfer-config)))

;;; packages.el ends here
