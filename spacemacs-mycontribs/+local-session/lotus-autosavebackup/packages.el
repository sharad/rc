;;; packages.el --- xlotus-autosavebackup layer packages file for Spacemacs.
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
;; added to `xlotus-autosavebackup-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `xlotus-autosavebackup/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `xlotus-autosavebackup/pre-init-PACKAGE' and/or
;;   `xlotus-autosavebackup/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-autosavebackup-packages
  '(
    (real-auto-save :location local)
    rcs-backup)

  "The list of Lisp packages required by the xlotus-autosavebackup layer.

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

(defun lotus-autosavebackup/init-real-auto-save ()
  (use-package real-auto-save
      :defer t
      :config
      (progn
        ;;link: http://www.litchie.net/programs/real-auto-save.html
        ;; (add-hook 'text-mode-hook 'turn-on-real-auto-save)
        ;; (add-hook 'muse-mode-hook 'turn-on-real-auto-save)
        ;; (remove-hook 'text-mode-hook 'turn-on-real-auto-save)
        ;; (remove-hook 'muse-mode-hook 'turn-on-real-auto-save)
        (setq real-auto-save-interval 10))))

(defun lotus-autosavebackup/init-rcs-backup ()
  (use-package rcs-backup
    :init
    (use-package startup-hooks
      :defer t
      :config
      (add-to-enable-login-session-interrupting-feature-hook
       '(lambda ()
          (rcs-backup-mode t))
       t))
    :defer t
    :commands (rcs-backup-mode)
    :config
    (progn
      (rcs-backup-mode t))))


(defun lotus-autosavebackup/init-files ()
  (use-package files
      :defer t
      :config
      (progn
        ;; Explanation: when emacs does a backup, by default it renames the
        ;; original file into the backup file name, then create a new file and
        ;; insert the current data into it. This effectively destroys the
        ;; creation date of your file.
        (setq backup-by-copying t   ; don't clobber symlinks
              version-control nil     ; use versioned backups
              delete-old-versions t
              kept-new-versions 2
              kept-old-versions 2))))

;;; packages.el ends here
