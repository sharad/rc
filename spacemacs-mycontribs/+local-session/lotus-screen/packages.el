;;; packages.el --- lotus-screen layer packages file for Spacemacs.
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
;; added to `lotus-screen-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-screen/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-screen/pre-init-PACKAGE' and/or
;;   `lotus-screen/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-screenS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-screen-packages
  '(
    elscreen
    (elscreen-server :location local))
    
  "The list of Lisp packages required by the lotus-screen layer.

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

(defun lotus-screen/init-elscreen-server ()
  ;; I need it badly

  ;; see http://code.google.com/p/maoxian/source/browse/trunk/lisps/elscreen/elscreen-w3m.el?r=86
  ;; see elscreen-w3m
  ;; elscreen-set-mode-to-nickname-alist is a compiled Lisp function.
  ;; (elscreen-set-mode-to-nickname-alist MODE-TO-NICKNAME-ALIST-SYMBOL)
  ;; thanks http://www.emacswiki.org/emacs-pt/EmacsLispScreen ElScreen-server
  (use-package elscreen-server
    :defer t
    :config
    (progn)))
      

(defun lotus-screen/init-elscreen ()
  (use-package elscreen
    :defer t
    :init
    (progn
      (turn-off-evil-mode)
      (elscreen-start))
    :config
    (progn
      (defun elscreen-move-right ()
        (interactive)
        (elscreen-next)
        (elscreen-swap)
        (elscreen-notify-screen-modification))

      (defun elscreen-move-left ()
        (interactive)
        (elscreen-previous)
        (elscreen-swap)
        (elscreen-notify-screen-modification))
      (defun elscree-reset ()
        (interactive)
        (setq org-agenda-buffer nil)
        (elscreen-start))
      (progn
        (use-package org-agenda
          :defer t
          :config
          (progn
            (add-to-enable-login-session-interrupting-feature-hook
             #'(lambda ()
                 (when (and org-agenda-buffer
                            (not (buffer-live-p org-agenda-buffer)))
                   (setq org-agenda-buffer nil)
                   (elscreen-start))))))))))

;;; packages.el ends here
