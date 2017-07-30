;;; packages.el --- lotus-cache layer packages file for Spacemacs.
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
;; added to `lotus-cache-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-cache/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-cache/pre-init-PACKAGE' and/or
;;   `lotus-cache/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-cacheS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-cache-packages
  '(
    filecache
    )
  "The list of Lisp packages required by the lotus-cache layer.

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

(defun lotus-cache/init-filecache ()
  (use-package filecache
      :defer t
      :config
      (progn
        ;; Use filecache:

        ;; filecache remembers visited places. Add the directory into the cache:

        ;; Whenever you want to load a file, you can enter C-x C-f C-<TAB> in
        ;; the minibuffer. The completion is done for the given directory.
        (defvar file-cache-directories nil "file-cache-directories")

        (with-eval-after-load "init-setup"
         (add-to-enable-startup-interrupting-feature-hook
         '(lambda ()
           (condition-case e
               (dolist (dir file-cache-directories)
                 (file-cache-add-directory dir))
             ('error (message "problem happened in %s fun call."
                              'file-cache-add-directory)))))))))

;;; packages.el ends here
