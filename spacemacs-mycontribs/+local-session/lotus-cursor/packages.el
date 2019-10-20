;;; packages.el --- lotus-cursor layer packages file for Spacemacs.
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
;; added to `lotus-cursor-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-cursor/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-cursor/pre-init-PACKAGE' and/or
;;   `lotus-cursor/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-cursorS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-cursor-packages
  '(
    hl-line
    ;; hl-line+ ;; package not found
    )
  "The list of Lisp packages required by the lotus-cursor layer.

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

(defun lotus-cursor/init-hl-line ()
  (use-package hl-line
    :defer t
    :config
    (progn
      (hl-line-mode) ;; for the current buffer
      (global-hl-line-mode) ;; for all buffers
      )))

;; package not found
;; (defun lotus-cursor/init-hl-line+ ()
;;   ;;{{ from: http://www-sop.inria.fr/everest/Clement.Hurlin/linux.shtml
;;   ;; 05/08/07, emacs: I find useful to highlight the line (snapshot)
;;   ;; where the cursor points because it avoids getting lost when
;;   ;; switching between numerous windows. You can enable this with:
;;   ;; (hl-line-mode) ;; for the current buffer
;;   ;; (global-hl-line-mode) ;; for all buffers

;;   ;; http://www.emacswiki.org/emacs/hl-line%2b.el
;;   ;;  To use this library, put this in your Emacs init file (~/.emacs):
;;   ;;
;;   (use-package hl-line+
;;       :defer t
;;       :config
;;       (progn
;;         ;;
;;         ;;  To turn on `global-hl-line-mode' only when Emacs is idle, by
;;         ;;  default, add this line also to your init file:
;;         ;;
;;         ;; Highlight only when idle
;;         (hl-line-toggle-when-idle 1)
;;         (hl-line-when-idle-interval 70))
;;       ))

;;; packages.el ends here
