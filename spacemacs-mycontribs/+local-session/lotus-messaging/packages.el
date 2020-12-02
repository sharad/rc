;;; packages.el --- lotus-messaging layer packages file for Spacemacs.
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
;; added to `lotus-messaging-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-messaging/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-messaging/pre-init-PACKAGE' and/or
;;   `lotus-messaging/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-messaging-packages
  '(
    erc
    (erc-join :location local)
    (erc-services :location local)
    (erc-nick-notify :location local)
    (h4x0r :location local)
    bitlbee
    passwds

    rcirc)
  "The list of Lisp packages required by the lotus-messaging layer.

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

(defun lotus-messaging/post-init-erc ()
  (use-package erc
    :defer t
    :commands (lotus-messaging-start-or-switch)
    :config
    (lotus-messaging/post-init-erc-config)))

(defun lotus-messaging/init-erc-join ()
  (use-package erc-join
    :defer t
    :config
    (lotus-messaging/init-erc-join-config)))

(defun lotus-messaging/init-erc-services ()
  (use-package erc-services
    :defer t
    :config
    (lotus-messaging/init-erc-services-config)))

(defun lotus-messaging/init-erc-nick-notify ()
  (use-package erc-nick-notify
    :defer t
    :config
    (lotus-messaging/init-erc-nick-notify-config)))

(defun lotus-messaging/init-h4x0r ()
  (use-package h4x0r
    :defer t
    :config
    (lotus-messaging/init-h4x0r-config)))

(defun lotus-messaging/init-bitlbee ()
  (use-package bitlbee
    :defer t
    :config
    (lotus-messaging/init-bitlbee-config)))

(defun lotus-messaging/post-init-passwds ()
  (use-package passwds
    :defer t
    :config
    (lotus-messaging/post-init-passwds-config)))

(defun lotus-messaging/post-init-rcirc ()
  (use-package rcirc
    :defer t
    :config
    (lotus-messaging/post-init-rcirc-config)))

;;; packages.el ends here
