;;; packages.el --- lotus-finance layer packages file for Spacemacs.
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
;; added to `lotus-finance-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-finance/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-finance/pre-init-PACKAGE' and/or
;;   `lotus-finance/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-financeS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-finance-packages
  '(
    (creditcard :location local)
    (balance :location local)
    ;; http://code.google.com/p/emacs-easy-budget/
    (emacs-easy-budget :location local)
    (sample :location local)
    (date-util :location local)
    (ledger :location local)
    )
  "The list of Lisp packages required by the lotus-finance layer.

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

(defun lotus-finance/init-creditcard ()
  (use-package creditcard
      :defer t
      :config
      (progn
        (autoload 'creditcard-mode "creditcard")
        (setq auto-mode-alist
              (append '(("\\.bal$" . creditcard-mode)) auto-mode-alist)))))

(defun lotus-finance/init-balance ()
  (use-package balance
      :defer t
      :config
      (progn
        (autoload 'balance-mode "balance")
        (setq auto-mode-alist
              (append '(("\\.bal$" . balance-mode)) auto-mode-alist))
        ;;
        ;; Then, doing a find-file on "sample.bal" will automatically load the balance
        ;; elisp and enter Balance major mode for the buffer visiting the file.
        ;; See the balance-mode mode help for details.
        ;; Balance will, if requested, remove a lot of old transactions and put it in
        ;; a backup file.  To automatically open a backup file in balance-mode, put
        ;;
        (setq auto-mode-alist
              (append '(("\\.bal\\.[0-9]+$" . balance-mode)) auto-mode-alist))
        ;; in your .emacs file.
        )))

(defun lotus-finance/init-emacs-easy-budget ()
  (use-package emacs-easy-budget
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-finance/init-sample ()
  (use-package sample
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-finance/init-date-util ()
  (use-package date-util
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-finance/init-ledger ()
  (use-package ledger
      :defer t
      :config
      (progn
        ;; http://www.emacswiki.org/emacs/LedgerMode

        ;; Ledger is a command-line accounting tool by JohnWiegley, it
        ;; provides a double-entry accounting ledger. The input file is a
        ;; very simple text file.  You can get it from here:
        ;; https://github.com/jwiegley/ledger/wiki Ledger comes with a
        ;; ledger-mode and a function to add new entries. Here is an
        ;; alternate entry function. The accounts are in German. All
        ;; accounts for my expenses start with “Ausgaben:”. All accounts for
        ;; my capital start with “Vermögen:” (basically I can take money
        ;; from my cash reserves, or from one of my bank accounts).

        (defun ledger-add-entry (title in amount out)
          (interactive
           (let ((accounts (mapcar 'list (ledger-accounts))))
             (list (read-string "Entry: " (format-time-string "%Y-%m-%d " (current-time)))
                   (let ((completion-regexp-list "^Ausgaben:"))
                     (completing-read "What did you pay for? " accounts))
                   (read-string "How much did you pay? " "CHF ")
                   (let ((completion-regexp-list "^Vermögen:"))
                     (completing-read "Where did the money come from? " accounts)))))
          (insert title)
          (newline)
          (indent-to 4)
          (insert in "  " amount)
          (newline)
          (indent-to 4)
          (insert out)))))

;;; packages.el ends here
