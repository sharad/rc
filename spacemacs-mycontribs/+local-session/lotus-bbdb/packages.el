;;; packages.el --- lotus-bbdb layer packages file for Spacemacs.
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
;; added to `lotus-bbdb-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-bbdb/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-bbdb/pre-init-PACKAGE' and/or
;;   `lotus-bbdb/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-bbdbS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-bbdb-packages
  '(
    (bbdb :location local)
    bbdb-merge
    bbdb-vcard-import
    )
  "The list of Lisp packages required by the lotus-bbdb layer.

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

(defun lotus-bbdb/init-bbdb ()
  (use-package PACKAGE
      :defer t
      ;; :post
      ;; (progn
      ;;   ;; save bbdb
      ;;   (add-hook 'kill-emacs-hook 'bbdb-save-db))
      :config
      (progn
        (setq
         bbdb-use-pop-up nil
         bbdb/mail-auto-create-p t
         bbdb/news-auto-create-p t))))

(defun lotus-bbdb/init-bbdb-merge ()
  (use-package bbdb-merge
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-bbdb/init-bbdb-vcard-import ()
  (use-package bbdb-vcard-import
      :defer t
      :config
      (progn
        (progn
          ))))

;;; packages.el ends here
