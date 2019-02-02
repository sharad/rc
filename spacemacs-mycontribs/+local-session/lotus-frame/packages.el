;;; packages.el --- lotus-frame layer packages file for Spacemacs.
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
;; added to `lotus-frame-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-frame/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-frame/pre-init-PACKAGE' and/or
;;   `lotus-frame/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-frame-packages
  '(
    frame-utils
    )
  "The list of Lisp packages required by the lotus-frame layer.

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

(defun lotus-frame/init-frame-utils ()
  (use-package frame-utils
      :defer t
      :config
      (progn
        (progn
          (defun frame-utils-config ()

            (setq frame-utils-notify 'message-notify)

            (defun make-mail-chat-frame (&optional force)
              (interactive "P")
              (frame-launcher "mail-chat"
                              '("gnus" "erc")
                              (if force
                                  #'(lambda (group)
                                      (toggle-ibuffer-group group t))
                                  #'toggle-ibuffer-group)))


            (defun make-mail-compose-frame ())

            (with-eval-after-load "sessions-mgr"
              (defadvice frame-launcher (around frame-launcher activate)
                (let ((*frame-session-restore* nil)  ;not need to restore elsession for frames
                      (org-donot-try-to-clock-in t)) ;no clock require to be clocked-in.
                  ad-do-it))))

          (frame-utils-config)))))

;;; packages.el ends here
