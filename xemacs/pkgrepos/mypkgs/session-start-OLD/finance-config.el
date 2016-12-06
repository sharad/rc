;;; finance-config.el --- Finance Config

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;;{{ from: http://www.emacswiki.org/emacs/BalanceMode

(deh-require-maybe creditcard
  (autoload 'creditcard-mode "creditcard")
  (setq auto-mode-alist
        (append '(("\\.bal$" . creditcard-mode)) auto-mode-alist)))

(deh-require-maybe balance
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
)

;;}}



;;{{
(deh-require-maybe
    ;; http://code.google.com/p/emacs-easy-budget/
    (and emacs-easy-budget ample date-util)
    )
;;}}


(deh-require-maybe ledger
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
    (insert out)))


(provide 'finance-config)
;;; finance.el ends here
