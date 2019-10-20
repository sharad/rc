;;; packages.el --- lotus-doc layer packages file for Spacemacs.
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
;; added to `lotus-doc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-doc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-doc/pre-init-PACKAGE' and/or
;;   `lotus-doc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-docS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-doc-packages
  '(
    eldoc
    ;; eldoc-extension ;; package not found
    c-eldoc
    (clweb :location local)                               ;good
    )
  "The list of Lisp packages required by the lotus-doc layer.

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

(defun lotus-doc/post-init-eldoc ()
  (use-package eldoc
      :defer t
      :config
      (progn
        (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
        (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
        (with-eval-after-load "ielm"
          (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode))
        (with-eval-after-load "cperl"
          ;; for perl http://www.emacswiki.org/emacs/CPerlMode#toc10
          (defun my-cperl-eldoc-documentation-function ()
            "Return meaningful doc string for `eldoc-mode'."
            (car
             (let ((cperl-message-on-help-error nil))
               (cperl-get-help))))

          (add-hook 'cperl-mode-hook
                    (lambda ()
                      (set (make-local-variable 'eldoc-documentation-function)
                           'my-cperl-eldoc-documentation-function)))))))

(defun lotus-doc/post-init-c-eldoc ()
  ;; for c http://www.emacswiki.org/emacs/CEldocMode#toc5o
  (use-package c-eldoc
      :defer t
      :config
      (progn
        (add-hook 'c-mode-hook 'c-turn-on-eldoc-mode)
        (add-hook 'c++-mode-hook 'c-turn-on-eldoc-mode)
        (setq c-eldoc-includes "`pkg-config gtk+-2.0 --cflags` -I./ -I../ "))))

;; package not found
;; (defun lotus-doc/init-eldoc-extension ()
;;   (use-package eldoc-extension
;;       :defer t
;;       :config
;;       (progn
;;         )))

(defun lotus-doc/init-clweb ()
  (use-package clweb
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
