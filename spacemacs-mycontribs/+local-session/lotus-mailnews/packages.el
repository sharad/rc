;;; packages.el --- lotus-mailnews layer packages file for Spacemacs.
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
;; added to `lotus-mailnews-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-mailnews/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-mailnews/pre-init-PACKAGE' and/or
;;   `lotus-mailnews/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-mailnews-packages
  '(
    (gnus :location local)
    lsdb
    )
  "The list of Lisp packages required by the lotus-mailnews layer.

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


(defun lotus-mailnews/pre-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (progn
      (progn
        (progn
          (setq gnus-init-file "~/.gnus.el"))
        (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
        (setq
         gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
        (setq
         gnus-directory      (concat gnus-home-directory "News/"))
        (setq
         nndraft-directory (concat gnus-directory "drafts/")))))



  (progn                                ;unconditionally
    (progn
      (progn
        (setq gnus-init-file "~/.gnus.el"))
      (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
      (setq
       gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
      (setq
       gnus-directory      (concat gnus-home-directory "News/"))
      (setq
       nndraft-directory (concat gnus-directory "drafts/")))))

(defun lotus-mailnews/post-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (progn
      (progn
        (progn
          (setq gnus-init-file "~/.gnus.el"))
        (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
        (setq
         gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
        (setq
         gnus-directory      (nnheader-concat gnus-home-directory "News/"))
        (setq
         nndraft-directory  (nnheader-concat gnus-directory "drafts/")))
      (progn
        (add-hook
         'gnus-article-prepare-hook
         'gnus-treat-mail-gravatar)))))

(defun lotus-mailnews/post-init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (setq
         lsdb-file
         (lotus-cache-file "lsdb/lsdb"))
        (unless (file-exists-p lsdb-file)
          (auto-config-dir
           (file-name-directory lsdb-file) t))))))

;;; packages.el ends here
