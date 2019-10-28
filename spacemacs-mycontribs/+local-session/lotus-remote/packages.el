;;; packages.el --- lotus-remote layer packages file for Spacemacs.
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
;; added to `lotus-remote-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-remote/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-remote/pre-init-PACKAGE' and/or
;;   `lotus-remote/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-remoteS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-remote-packages
  '(
    ;; (PACKAGE :location local)
    tramp
    (tramp-cache :location local)
    (tramp-cmds :location local)
    (tramp-compat :location local)
    (tramp-fish :location local)
    (tramp-ftp :location local)
    ;; tramp-gvfs
    ;; (tramp-gw :location local)
    ;; (tramp-imap :location local)
    (tramp-smb :location local)
    (tramp-uu :location local)
    (trampver :location local)
    (tramp-sh :location local)
    ido
    password-cache)

  "The list of Lisp packages required by the lotus-remote layer.

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

(defun lotus-remote/init-tramp ()
  (use-package tramp
    :defer t
    :commands (tramp-file-prefix)
    :config
    (lotus-remote/init-tramp-config)))

(defun lotus-remote/init-tramp-cache ()
  (use-package tramp-cache
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-cmds ()
  (use-package tramp-cmds
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-compat ()
  (use-package tramp-compat
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-fish ()
  (use-package tramp-fish
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-ftp ()
  (use-package tramp-ftp
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-gvfs ()
  (use-package tramp-gvfs
      :defer t
      :config
      (progn
        )))

;; (defun lotus-remote/init-tramp-gw ()
;;   (use-package tramp-gw
;;       :defer t
;;       :config
;;       (progn
;;         )))

;; (defun lotus-remote/init-tramp-imap ()
;;   (use-package tramp-imap
;;       :defer t
;;       :config
;;       (progn
;;         )))

(defun lotus-remote/init-tramp-smb ()
  (use-package tramp-smb
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-uu ()
  (use-package tramp-uu
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-trampver ()
  (use-package trampver
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-tramp-sh ()
  (use-package tramp-sh
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/post-init-ido ()
  (use-package ido
      :defer t
      :config
      (progn
        )))

(defun lotus-remote/init-password-cache ()
  (use-package password-cache
      :defer t
      :config
      (progn
        )))

;; (let ((str "/scp:spratap@susengg-01:/home/spratap/releases/5.1/src/wnc/coord/")
;;       (regexs (list
;;                tramp-file-name-regexp
;;                tramp-file-name-regexp-unified
;;                tramp-file-name-regexp-url
;;                tramp-root-regexp
;;                tramp-domain-regexp
;;                tramp-user-regexp
;;                tramp-prefix-domain-regexp
;;                "\\`/[^:/][^:/]+:\\'"
;;                "\\`/[^/]+[@:][^:/]+:/")))
;;   (message "start")
;;   (dolist (r regexs)
;;     (string-match r str)
;;     (message "aa: %s %s" r (match-string 0 str))))
;; (ido-is-tramp-root "/scp:spratap@susengg-01:")
;; (ido-is-root-directory "/")



;;; packages.el ends here
