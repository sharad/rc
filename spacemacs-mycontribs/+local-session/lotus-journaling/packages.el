;;; packages.el --- lotus-journaling layer packages file for Spacemacs.
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
;; added to `lotus-journaling-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-journaling/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-journaling/pre-init-PACKAGE' and/or
;;   `lotus-journaling/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-journalingS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-journaling-packages
  '(
    (records :location local)
    ;; org-journal
    )
  "The list of Lisp packages required by the lotus-journaling layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil
c
    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-journaling/init-records ()
  (use-package records
      :defer t
      :config
      (prognc
          (setq
   records-init-file (expand-file-name "~/.emacs.d/records")
   records-directory (expand-file-name "~/.Organize/emacs/records")))))

(when nil
(defun lotus-journaling/init-org-journal ()
  (use-package org-journal
      :defer t
      :config
      (progn
        (when (functionp 'org-publish-get-attribute) ;; available in publishing-config.el
          (defvar org-journal-dir (org-publish-get-attribute "journal" "org" :base-directory))
          (setq
           org-journal-file-format "%Y-%m-%d.org"
           org-journal-file-pattern (org-journal-format-string->regex org-journal-file-format)
           org-journal-dir (org-publish-get-attribute "journal" "org" :base-directory))
          (org-journal-update-auto-mode-alist)))))
)

;;; packages.el ends here
