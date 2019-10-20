;;; packages.el --- lotus-bookmark layer packages file for Spacemacs.
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
;; added to `lotus-bookmark-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-bookmark/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-bookmark/pre-init-PACKAGE' and/or
;;   `lotus-bookmark/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-bookmarkS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

;; (error '(require 'misc-config))

(defconst lotus-bookmark-packages
  '(
    ;; (PACKAGE :location local)
    saveplace
    (breadcrumb :location local)
    bm
    ;; bookmark+ ;; package not found
    (linkd :location local)
    )
  "The list of Lisp packages required by the lotus-bookmark layer.

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

(defun lotus-bookmark/post-init-saveplace ()
  (use-package saveplace
      :defer t
      :config
      (progn
        (setq save-place-file (lotus-cache-file "save-place/places")))))

(defun lotus-bookmark/init-breadcrumb ()
  (use-package breadcrumb
      :defer t
      :config
      (progn
        (setq bc-bookmark-file (lotus-cache-file "breadcrumb/breadcrumbe.el"))

        (autoload 'bc-set               "breadcrumb" "Set bookmark in current point."   t)
        (autoload 'bc-previous          "breadcrumb" "Go to previous bookmark."         t)
        (autoload 'bc-next              "breadcrumb" "Go to next bookmark."             t)
        (autoload 'bc-local-previous    "breadcrumb" "Go to previous local bookmark."   t)
        (autoload 'bc-local-next        "breadcrumb" "Go to next local bookmark."       t)
        (autoload 'bc-goto-current      "breadcrumb" "Go to the current bookmark."      t)
        (autoload 'bc-list              "breadcrumb" "List all bookmarks in menu mode." t)
        (autoload 'bc-clear             "breadcrumb" "Clear all bookmarks."             t))))

(defun lotus-bookmark/post-init-bm ()   ;core-mod
  (use-package bm
               :defer t
               :config
               (progn
                 (setq bm-repository-file (lotus-cache-file "bm/bm-repository")))))


(defun lotus-bookmark/init-bookmark ()
  (use-package bookmark
      :defer t
      :config
      (progn
        (setq bookmark-file (lotus-cache-file "bookmark/bookmarks.el"))
        (setq bookmark-default-file (lotus-cache-file "bookmark/bookmarks.el")))))

;; package not found
;; (defun lotus-bookmark/init-bookmark+ ()
;;   (use-package bookmark+
;;       :defer t
;;       :config
;;       (progn
;;         ;; available in elpa
;;         (setq bmkp-bmenu-state-file    (lotus-cache-file "bookmark+/emacs-bmk-bmenu-state.el")
;;               bmkp-bmenu-commands-file (lotus-cache-file "bookmark+/emacs-bmk-bmenu-commands.el"))
;;
;;         (remove-hook 'kill-emacs-hook 'bookmark-bmenu-save))))

(defun lotus-bookmark/init-linkd ()
  (use-package linkd
      :defer t
      :config
      (progn
        ;; http://dto.github.com/notebook/linkd.html
        ;; http://www.emacswiki.org/emacs/LinkdMode

        (setq linkd-use-icons t
              linkd-icons-directory "~/.xemacs/pkgrepos/world/misc/linkd/icons/") ;; or wherever you put it
        )))

;;; packages.el ends here
