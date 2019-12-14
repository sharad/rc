;;; packages.el --- lotus-encrypt layer packages file for Spacemacs.
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
;; added to `lotus-encrypt-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-encrypt/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-encrypt/pre-init-PACKAGE' and/or
;;   `lotus-encrypt/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-encrypt-packages
  '(
    ;; (PACKAGE :location local)
    ;; epa
    ;; (epa-file :location local)
    lotus-crypt-utils

    ;; pass               20190611.2001 new        melpa      Major mode for password-store.el
    ;; passmm             20181130.1612 new        melpa      A minor mode for pass (Password Store).
    ;; passthword         20141201.923  new        melpa      Simple password manager
    ;; password-mode      20170412.629  new        melpa      Hide password text using overlays
    ;; password-store-otp 20180815.610  new        melpa      Password store (pass) OTP extension support
    ;; password-vault     20160126.1820 new        melpa      A Password manager for Emacs.

    )
  "The list of Lisp packages required by the lotus-encrypt layer.

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

(defun lotus-encrypt/init-lotus-crypt-utils ()
  (use-package lotus-crypt-utils
    :defer t
    :config
    (progn
      (progn
        (add-to-list 'auto-mode-alist
                     '("\\.asc\\(~\\|\\.~[0-9]+~\\)?\\'" nil epa-file)))
      (progn
        (epa-file-disable))
      (progn
        (epa-passphrase-cleanup-start)
        (epa-add-exception-for "~/.authinfo.gpg" 100000)
        (setq epa-file-cache-passphrase-for-symmetric-encryption t))
      (progn
        (setq
         ;; https://colinxy.github.io/software-installation/2016/09/24/emacs25-easypg-issue.html
         epa-pinentry-mode 'loopback)
        (setq vc-follow-symlinks nil))))

  (use-package startup-hooks
    :defer t
    :config
    (progn
      (epa-passphrase-cleanup-start))))

;;; packages.el ends here
