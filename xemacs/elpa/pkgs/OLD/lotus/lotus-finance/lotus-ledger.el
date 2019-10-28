;;; lotus-ledger.el --- Finance Ledger               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Sharad

;; Author: Sharad <sh4r4d@gmail.com>
;; Keywords: convenience, tools

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

(provide 'lotus-ledger)


;; (require 'ledger)


;; https://c-tan.com/post/ledger-org-babel-example/
(push '
 (ledger . t)
 org-babel-load-languages)





;; https://lists.gnu.org/archive/html/emacs-orgmode/2013-03/msg00019.html
;; https://github.com/4DA/emacs-stuff/blob/master/ledger-completed-capture.el
(setq ledger-expense-completions
      (list
       "" ;; needed for first | for mapconcat
       "Income:Savings"
       "Income:Salary"
       "Assets:Cash" "Assets:Bank"
       "Expenses:Food"
       "Expenses:Food:Restraunts"
       "Expenses:Food:Groceries"
       "Expenses:Food:Sweets"
       "Expenses:Food:FastFood"
       "Expenses:Flat:Rent"
       "Expenses:Flat:Utilities"
       "Expenses:Outfit:Apparel"
       "Expenses:Outfit:Accessories"
       "Expenses:Goods"
       "Expenses:Electronics:Gadgets"
       "Expenses:Electronics:Toys"
       "Expenses:Electronics:Parts"
       "Expenses:Telecom:Internet"
       "Expenses:Telecom:Phone"
       "Expenses:Tools"
       "Expenses:Transport"
       "Expenses:Services"
       "Expenses:Entertainment"
       "Expenses:Dog"
       "Expenses:Dog:Food"
       "Expenses:Dog:Treatement"
       "Expenses:Dog:Medicine"
       "Expenses:Moto"
       "Expenses:Moto:Gas"
       "Expenses:Moto:Servicing"
       "Expenses:Moto:Expendables"
       "Expenses:Moto:Parts"))

(setq capture-expense-template
        "%%(org-read-date)  %%^{What}
      Expenses:%%^{Expense%s}  %%^{Amount}
      Account:Cash")

(defun return-capture-expense-template ()
  (let ((compstring
         (mapconcat 'identity ledger-expense-completions  "|")))
    (format capture-expense-template compstring)))

(setq org-capture-templates nil)
(setq org-capture-templates
        (append '(("l" "Ledger entries")
                  ("lc" "Cash" plain)
                  (file "~/org/stat/expenses.ldg")
                  (function return-capture-expense-template)
                  :empty-lines-before 1
                  :empty-lines-after 1)))

;; https://news.ycombinator.com/item?id=10510394


;;; lotus-ledger.el ends here
