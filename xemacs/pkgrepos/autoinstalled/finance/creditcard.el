;; creditcard-mode.el

;; Creditcard -- Major mode for recording transactions and balancing a bank
;;            account.  Based on creditcard-mode by Jason Baietto 
;;            (jason@ssd.csd.harris.com) and Bob Newell
;;
;; $Date: 2007/03/24 23:23:52 $
;; $Revision: 1.25 $
;;
;; This program is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by the Free
;; Software Foundation.
;;
;; This program is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
;; FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License for
;; more details.
;;
;; You should have received a copy of the GNU General Public License along
;; with this program; if not, write to the Free Software Foundation, Inc., 675
;; Mass Ave, Cambridge, MA 02139, USA.
;;
;; Put this file in your emacs load path and add the following lines to your
;; .emacs file:
;;
;;    (autoload 'creditcard-mode "creditcard")
;;    (setq auto-mode-alist
;;       (append '(("\\.bal$" . creditcard-mode)) auto-mode-alist))
;;
;; Then, doing a find-file on "sample.bal" will automatically load the creditcard
;; elisp and enter Creditcard major mode for the buffer visiting the file.
;; See the creditcard-mode mode help for details.
;; Creditcard will, if requested, remove a lot of old transactions and put it in 
;; a backup file.  To automatically open a backup file in creditcard-mode, put
;;
;;    (autoload 'creditcard-mode "creditcard")
;;    (setq auto-mode-alist
;;       (append '(("\\.bal\\.[0-9]+$" . creditcard-mode)) auto-mode-alist))
;; in your .emacs file.


(require 'calendar)

;; Definitions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar creditcard-header
"Date           Place                Notes                       Amount
====================================================================")

(defvar creditcard-footer
"====================================================================")

;(defvar creditcard-post-footer
;"Total (reported): 
;Total (unmarked): 
;Total   (owed)  : ")

(defvar creditcard-date-column 0
   "Column where transaction date begins.")

(defvar creditcard-date-width 12
  "Width of date column.")

(defvar creditcard-paid-column 
  (+ creditcard-date-column creditcard-date-width 1)
  "Column where status appears.")

(defvar creditcard-paid-width 1
  "Width of status column.")

(defvar creditcard-where-column 
  (+ creditcard-paid-column creditcard-paid-width 1)
  "Column where transaction place begins.")

(defvar creditcard-where-width 20
  "Width of where column.")

(defvar creditcard-description-column 
  (+ creditcard-where-column creditcard-where-width 1)
  "Column where transaction description begins.")

(defvar creditcard-description-width 20
  "Width of description column.")

(defvar creditcard-sign-column 
  (+ creditcard-description-column creditcard-description-width 1)
  "Column where transaction sign begins.")

(defvar creditcard-sign-width 1
  "Width of sign column.")

(defvar creditcard-amount-column 
  (+ creditcard-sign-column creditcard-sign-width 1)
  "Column where transaction amount begins.")

(defvar creditcard-amount-width 9
  "Width of amount column.")

(defvar creditcard-tab-stop-list
   (list
      creditcard-date-column
      creditcard-where-column
;      creditcard-paid-column
      creditcard-description-column
;      creditcard-sign-column
      creditcard-amount-column)
   "List of tab stops that define the start of all transaction fields.
Redefine this in your .emacs file to add or change fields.")

(defvar creditcard-months
  (list "Jan" "Feb" "Mar" "Apr" "May" "Jun"
        "Jul" "Aug" "Sep" "Oct" "Nov" "Dec"))

(defvar creditcard-months-regexp (regexp-opt creditcard-months t))

(defvar creditcard-mode-map nil
   "Keymap for creditcard buffer.")

(if creditcard-mode-map
   ()
   (setq creditcard-mode-map (make-sparse-keymap))
   (define-key creditcard-mode-map "\C-c\C-b" 'creditcard-backward-field)
   (define-key creditcard-mode-map "\C-c\C-c" 'creditcard-recalculate)
   (define-key creditcard-mode-map "\C-c\C-x" 'creditcard-toggle-paid)
   (define-key creditcard-mode-map "\C-c;"    'creditcard-toggle-comment-out)
   (define-key creditcard-mode-map "\C-c+"    'creditcard-toggle-credit)
   (define-key creditcard-mode-map "\C-c\C-f" 'creditcard-forward-field)
   (define-key creditcard-mode-map "\C-c\C-a" 'creditcard-append-transaction)
   (define-key creditcard-mode-map [tab]      'creditcard-forward-field)
   (define-key creditcard-mode-map "\C-x\C-s" 'creditcard-save-buffer)
   (define-key creditcard-mode-map "\C-k"     'creditcard-kill-row)
   (define-key creditcard-mode-map "\C-y"     'creditcard-yank-row)
   (define-key creditcard-mode-map "\C-c\C-u" 'creditcard-next-unpaid)
   (define-key creditcard-mode-map "\C-m"     'creditcard-toggle-hidden-paid)   
   (define-key creditcard-mode-map "="        'creditcard-change-field))

(defvar creditcard-mode-abbrev-table nil
   "Abbrev table used while in creditcard mode.")

(define-abbrev-table 'creditcard-mode-abbrev-table nil)

;; Code
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun creditcard-mode()
"Major mode for editing a buffer containing creditcard usage.
The following bindings provide the main functionality of this mode:

   \\[creditcard-append-transaction]\
        Append transaction to end of buffer.
   \\[creditcard-recalculate]\
        Recalculate creditcard of all transactions in buffer.
   \\[creditcard-change-field]     \
        Change the contents of the current field.
   \\[creditcard-toggle-credit]\
        Change whether the current line is a debit (the default),
        a credit (indicated by \"+\"), or sets the creditcard (indicated
        by \"=\").

When a transaction is noted in a statement, it should be noted with
   \\[creditcard-toggle-paid]\
        Toggle whether or not a check is marked as paid.
When a creditcard buffer is first opened, any sequences of paid checks will
be replaced by ellipses.  Typing RET on the ellipses will display the paid
checks, and any paid checks can be hidden with RET.

Checks can be move about with
    \\[creditcard-order-latest]\
        Put the latest entries (those since the last paid) in order by date.
    \\[creditcard-order-buffer]\
        Put all entries in order by date.
    \\[creditcard-kill-row]\
        Delete the current check.
    \\[creditcard-yank-row]\
        Copy the last killed check.

In addition, these bindings are defined to simplify editing
transactions:

   \\[creditcard-forward-field]\
        Move forward to start of next field (same as TAB).
   \\[creditcard-backward-field]\
        Move backward to start of previous field (same as shift-TAB).

Transactions occur on a single line and have the following fields (in
order):

   date           The transaction date in DD MMM YYYY format.
   paid           Status of transaction, x is paid.
   where          The place the transaction took place.
   description    A possibly blank transaction description.
   sign           This field must either be \"+\", \"-\", \" \" or \"=\".
                  + means credit, - or space means debit, 
                    and = resets creditcard.
   amount         The transaction amount.  You must enter this field.
 (\\[creditcard-recalculate]).

Changing any amount and recalculating again will update the total.
Transactions may be commented out by \\[creditcard-toggle-comment-out] (and 
uncommented out with the same command). This makes it easy to reconcile your
account with the bank, which is usually several transactions behind you.

To keep the file from getting too large, the command \\[creditcard-backup] will
make a copy of the current file (in the form `filename.n' for the smallest
number `n' for which the file doesn't already exist) and then replace any 
sequence of paid lines by a single line with an appropriate sign and amount.

Entering creditcard-mode runs the `creditcard-mode-hooks' if any exist.  There is
also a creditcard-mode-abbrev-table for the truly warped.
"
   (interactive)
   (setq major-mode 'creditcard-mode)
   (setq mode-name "CreditCard")
   (use-local-map creditcard-mode-map)
   (make-local-variable 'tab-stop-list)
   (add-to-invisibility-spec '(creditcard . t))
   (setq tab-stop-list creditcard-tab-stop-list)
   (make-local-variable 'indent-tabs-mode)
   (setq indent-tabs-mode nil)
   (setq indent-line-function 'creditcard-forward-field)
   (setq local-abbrev-table creditcard-mode-abbrev-table)
   (auto-fill-mode -1)
   (when (= (point-min) (point-max))
       (insert creditcard-header "\n" 
               creditcard-footer "\n")
;               creditcard-post-footer)
       (forward-line -1))
   (setq buffer-read-only t)
   (creditcard-fontify-buffer)
   (goto-char (point-max))
   (search-backward creditcard-footer)
   (forward-line -1)
   (creditcard-hide-paid)
   (run-hooks 'creditcard-mode-hooks))

(defun creditcard-current-line()
"Return the current buffer line at point.  The first line is 0." 
   (save-excursion
      (beginning-of-line)
      (count-lines (point-min) (point))))

(defun creditcard-forward-field()
"Move the cursor to the next data entry field for the transaction on the
current line."
   (interactive)
   (move-to-tab-stop))

(defun creditcard-last (list)
"Return last element in a list."
   (cond
      ((null list) '())
      ((null (cdr list)) (car list))
      (t (creditcard-last (cdr list)))))

(defun creditcard-find-largest-less-than (list item)
"Search a sorted LIST of numbers, return the largest number that is still less
than ITEM."
   (let ((list-car (car list))
         (list-cdr (cdr list))
         (last nil))
      (while (and list-car (< list-car item))
         (setq last list-car)
         (setq list-car (car list-cdr))
         (setq list-cdr (cdr list-cdr)))
      last))

(defun creditcard-backward-field ()
"Move the cursor to the previous data entry field for the transaction on the
current line."
   (interactive)
   (let* ((col (current-column))
          (prev 
           (creditcard-find-largest-less-than creditcard-tab-stop-list col)))
      (if prev
         (move-to-column prev)
         (move-to-column (creditcard-last creditcard-tab-stop-list)))))

(defun creditcard-get-string (beg end)
  (let* ((true-beg (save-excursion
                     (goto-char beg)
                     (skip-chars-forward " \t" end)
                     (point)))
         (true-end (save-excursion
                     (goto-char end)
                     (skip-chars-backward " \t" true-beg)
                     (point))))
    (buffer-substring-no-properties true-beg true-end)))

(defun creditcard-insert-field 
  (prompt col-beg col-width use-old history &optional change)
  (let* ((line-beg (line-beginning-position))
         (beg (+ line-beg col-beg))
         (end (+ beg col-width))
         (def-input (if use-old
                        (creditcard-get-string beg end)
                      ""))
         (new-input (let ((inhibit-read-only nil))
                      (read-string (concat prompt ": ") 
                                   def-input
                                   history)))
         (inhibit-read-only t))
    (if (string= prompt "Amount")
        (setq new-input (format (concat 
                                 "%" 
                                 (number-to-string col-width) 
                                 ".2f") (string-to-number new-input)))
      (if (> (length new-input) col-width)
          (setq new-input (substring new-input 0 col-width))
        (setq new-input (format (concat 
                                 "%-"
                                 (number-to-string col-width)
                                 "s") new-input))))
    (if (and
         (eq change t)
         (string= prompt "Where")
         (eq (string-match "Payment" new-input) 0))
        (creditcard-make-credit))
    (save-excursion
      (move-to-column (+ col-beg col-width) t))
      (delete-region beg end)
      (insert new-input)
    (unless (creditcard-paid-p)
      (remove-overlays beg end)
      (overlay-put (make-overlay beg end nil t)
                   'face
                   (cond
                    ((string= prompt "Date")
                     'creditcard-date-face)
                    ((string= prompt "Where")
                     'creditcard-where-face)
                    ((string= prompt "Description")
                     'creditcard-description-face)
                    ((string= prompt "Sign")
                     'creditcard-sign-face)
                    ((string= prompt "Amount")
                     'creditcard-amount-face))))
    (goto-char beg)
    (if (and
         (eq change 'postpone)
         (string= prompt "Where")
         (eq (string-match "Payment" new-input) 0))
        'credit)))

(defun creditcard-insert-date ()
  (creditcard-insert-field "Date" 
                        creditcard-date-column 
                        creditcard-date-width 
                        t
                        'creditcard-date-history))

(defvar creditcard-where-history nil)
(defun creditcard-insert-where (&optional change)
  (creditcard-insert-field "Where" 
                        creditcard-where-column 
                        creditcard-where-width 
                        t
                        'creditcard-where-history
                        change))

(defvar creditcard-description-history nil)
(defun creditcard-insert-description ()
  (creditcard-insert-field "Description" 
                        creditcard-description-column 
                        creditcard-description-width 
                        t
                        'creditcard-description-history))

;(defun creditcard-insert-sign ()
;  (creditcard-insert-field "Sign" creditcard-sign-column creditcard-sign-width t))

(defvar creditcard-amount-history nil)
(defun creditcard-insert-amount ()
  (creditcard-insert-field "Amount" 
                        creditcard-amount-column 
                        creditcard-amount-width 
                        t
                        'creditcard-amount-history))

(defun creditcard-change-field ()
  (interactive)
  (if (save-excursion
        (beginning-of-line)
        (looking-at (concat "^[0-9]+ " creditcard-months-regexp " [0-9]+")))
      (let ((col (current-column)))
        (cond ((< col creditcard-where-column)
               (creditcard-insert-date))
              ((< col creditcard-description-column)
               (creditcard-insert-where t))
              ((< col creditcard-sign-column)
               (creditcard-insert-description))
              ((< col creditcard-amount-column)
               nil)
;               (creditcard-insert-sign))
              (t
               (creditcard-insert-amount))))))

(defun creditcard-paid-p ()
  (save-excursion
    (move-to-column creditcard-paid-column)
    (looking-at "x")))

(defun creditcard-next-unpaid ()
  (interactive)
  (forward-line 1)
  (while (creditcard-paid-p)
    (forward-line 1)))

(defun creditcard-toggle-paid ()
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[0-9]+ " creditcard-months-regexp " [0-9]+")))
    (save-excursion
      (move-to-column creditcard-paid-column)
      (let ((pt (point))
            (inhibit-read-only t))
        (if (string= (buffer-substring pt (1+ pt)) "x")
            (progn
              (delete-char 1)
              (insert " "))
          (delete-char 1)
          (insert "x"))
        (creditcard-fontify-line)))
    (creditcard-next-unpaid)))

(defun creditcard-hide-paid-lines ()
  (interactive)
  (save-excursion
    (when (creditcard-paid-p) ;(creditcard-paid-but-not-backed-up-p)
      (forward-line -1)
      (while (and
              (creditcard-paid-p) ;(creditcard-paid-but-not-backed-up-p)
              (> (point) (point-min)))
        (forward-line -1))
      (if (not (creditcard-paid-p)) ;(creditcard-paid-but-not-backed-up-p))
          (forward-line 1))
      (let ((beg (point))
            end)
        (while (creditcard-paid-p) ;(creditcard-paid-but-not-backed-up-p)
          (forward-line 1))
        (overlay-put (make-overlay beg (1- (point)) nil t)
                     'invisible 'creditcard)))))

(defun creditcard-show-hidden-lines ()
  (let ((beg
         (save-excursion
           (forward-line 0)
           (while (and
                   (get-char-property (point) 'invisible)
                   (> (point) (point-min)))
             (forward-line -1))
           (point)))
        (end
         (save-excursion
           (forward-line 1)
           (while (get-char-property (point) 'invisible)
             (forward-line 1))
           (point))))
    (remove-overlays beg end 'invisible 'creditcard)))

(defun creditcard-toggle-hidden-paid ()
  (interactive)
  (if (not (get-char-property (line-beginning-position) 'invisible))
      (creditcard-hide-paid-lines)
    (creditcard-show-hidden-lines)))
    
(defun creditcard-hide-paid ()
  (interactive)
  (save-excursion
    (let (beg)
      (goto-char (point-min))
      (forward-line 2)
      (while (not (looking-at creditcard-footer))
        (while (and
                (not (creditcard-paid-p))
                (not (looking-at creditcard-footer)))
          (forward-line 1))
        (setq beg (point))
        (while (creditcard-paid-p)
          (forward-line 1))
      (unless (= beg (point))
        (overlay-put (make-overlay beg (1- (point)) nil t)
                     'invisible 'creditcard))
      (if (not (looking-at creditcard-footer))
          (forward-line 1))))))
  
(defun creditcard-insert-line (date paid where description sign amount)
  (let ((inhibit-read-only t))
    (forward-line 0)
    (insert "\n")
    (forward-char -1)
    (move-to-column creditcard-date-column t)
    (insert date)
    (move-to-column creditcard-paid-column t)
    (insert paid)
    (move-to-column creditcard-where-column t)
    (insert where)
    (move-to-column creditcard-description-column t)
    (insert description)
    (move-to-column creditcard-sign-column t)
    (insert sign)
    (move-to-column creditcard-amount-column t)
    (insert (format (concat 
                     "%" 
                   (number-to-string creditcard-amount-width) 
                   ".2f") amount))
    (creditcard-fontify-line)))

(defun creditcard-get-date ()
  (save-excursion
    (move-to-column creditcard-date-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt creditcard-date-width)))))

(defun creditcard-get-sign ()
  (save-excursion
    (move-to-column creditcard-sign-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt creditcard-sign-width)))))

(defun creditcard-get-where ()
  (save-excursion
    (move-to-column creditcard-where-column)
    (let ((pt (point)))
      (buffer-substring-no-properties pt
                                      (+ pt creditcard-where-width)))))

(defun creditcard-remove-paid ()
  (let (date sign beg)
    (save-excursion
      (goto-char (point-min))
      (forward-line 2)
      (while (not (looking-at creditcard-footer))
        (while (and
                (not (creditcard-paid-p))
                (not (looking-at creditcard-footer)))
          (forward-line 1))
        (setq beg (point))
        (setq sign "")
        (while (creditcard-paid-p)
          (setq date (creditcard-get-date))
          (if (string= (creditcard-get-sign) "=")
              (setq sign "="))
          (forward-line 1))
        (unless (= beg (point))
          (let ((inhibit-read-only t)
                (amt (creditcard-recalculate-region beg (point))))
            (delete-region beg (point))
            (if (string= sign "=")
                (if (< amt 0)
                    (progn
                      (creditcard-insert-line date "x" "paid" "" "" (- amt))
                      (creditcard-insert-line date "x" "paid" "" sign 0))
                  (creditcard-insert-line date "x" "paid" "" sign amt))
              (if (< amt 0)
                  (setq amt (- amt))
                (setq sign "+"))
              (creditcard-insert-line date "x" "paid" "" sign amt))))
        (if (not (looking-at creditcard-footer))
            (forward-line 1))))))

(defun creditcard-backup ()
  (interactive)
  (save-buffer)
  (let ((file (buffer-file-name))
        (n 1))
    (while (file-exists-p 
            (concat file "." (number-to-string n)))
      (setq n (1+ n)))
    (save-excursion
      (find-file (concat file "." (number-to-string n)))
      (insert-file-contents file)
      (save-buffer)
      (kill-buffer (current-buffer))))
  (creditcard-remove-paid)
  (save-buffer))

(defun creditcard-toggle-credit ()
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[0-9]+ " creditcard-months-regexp " [0-9]+")))
    (save-excursion
      (move-to-column creditcard-sign-column)
      (let* ((pt (point))
             (inhibit-read-only t)
             (sgn (buffer-substring pt (1+ pt))))
        (delete-char 1)
        (if (string= sgn " ")
            (insert "+")
          (if (string= sgn "+")
              (insert "=")
            (insert " ")))
        (creditcard-fontify-line)))))

(defun creditcard-make-credit ()
  (interactive)
  (when (save-excursion
          (beginning-of-line)
          (looking-at (concat "^[0-9]+ " creditcard-months-regexp " [0-9]+")))
    (save-excursion
      (move-to-column creditcard-sign-column)
      (let* ((inhibit-read-only t))
        (delete-char 1)
        (insert "+")))
    (creditcard-fontify-line)))

(defvar creditcard-date-history nil)

(defun creditcard-append-transaction (&optional arg)
"Add a transaction to the end of current buffer using today's date."
   (interactive "P")
   (let* ((inhibit-read-only t)
          (date (calendar-current-date))
          (month (car date))
          (cr nil)
          (day (car (cdr date)))
          (year (car (cdr (cdr date)))))
     (if arg
         (progn
           (beginning-of-line)
           (unless 
               (or
                (looking-at 
                 (concat "^[0-9]+ " creditcard-months-regexp " [0-9]+"))
                (and (looking-at "^==")
                     (save-excursion
                       (forward-line -1)
                       (looking-at "^=="))))
             (error "Can't insert transaction on this line.")))
       (goto-char (point-max))
       (re-search-backward "^=="))
     (insert "\n")
     (forward-line -1)
     (insert (format (concat "%-" 
                             (number-to-string creditcard-date-width)
                             "s")
                     (let ((inhibit-read-only nil))
                       (read-string "Date (DD MMM YYYY): "
                                    (format "%02d %s %04d"
                                            day (nth (1- month) 
                                                     creditcard-months) year)
                                    'creditcard-date-history))))
     (overlay-put (make-overlay (line-beginning-position)
                                (+ (line-beginning-position) 
                                   creditcard-date-width)
                                nil t)
                  'face 'creditcard-date-face)
     (move-to-column creditcard-where-column t)
     (setq cr
           (creditcard-insert-field "Where" 
                                 creditcard-where-column 
                                 creditcard-where-width 
                                 nil
                                 'creditcard-where-history
                                 'postpone))
     (move-to-column creditcard-description-column t)
     (creditcard-insert-field "Description" 
                           creditcard-description-column 
                           creditcard-description-width 
                           nil
                           'creditcard-description-history)
;     (move-to-column creditcard-sign-column t)
;     (creditcard-insert-field "Sign" 
;                           creditcard-sign-column 
;                           creditcard-sign-width nil)
     (move-to-column creditcard-amount-column t)
     (creditcard-insert-field "Amount" 
                           creditcard-amount-column 
                           creditcard-amount-width 
                           nil
                           'creditcard-amount-history)
     (beginning-of-line)
     (if (eq cr 'credit)
         (creditcard-make-credit))))


(defun creditcard-find-next-transaction (&optional arg)
"Find next line that looks like a complete transaction and return a list of
the line start, numeric data start and line end points."
   (let ((inhibit-read-only t)
         (found nil)
         (line-regexp (concat "^"
                              (if arg "%?" "")
                              "[0-9]+ "
                              creditcard-months-regexp
                              " [0-9]+.*$"))
         line-start
         line-end
         data-start)
      (while (and
               (not found)
               (search-forward-regexp line-regexp (point-max) t))
         (setq line-start (match-beginning 0))
         (untabify (match-beginning 0) (match-end 0))
         (setq line-end (progn (end-of-line) (point)))
         (setq data-start (+ line-start creditcard-sign-column))
         (if (> line-end data-start)
               (setq found t)))
      (if found
         (list line-start data-start line-end)
         nil)))

(defun creditcard-parse-transaction-data (data)
"Given a STRING representing the sign and amount, return a list of the sign 
and amount as point numbers."
   (let ((data-regexp "\\([-+= ]\\)[ \t]*\\([0-9.]+\\)?[ \t]*\\([-]?[0-9.]+\\)?")
         (balance nil)
         (reset nil)
         string sign amount)
      (string-match data-regexp data)
      (if (match-beginning 1)
         (setq sign (substring data (match-beginning 1) (match-end 1)))
         (error "line %d, missing sign" (1+ (creditcard-current-line))))
      (if (equal sign " ")
          (progn 
            (setq sign "-")))
      (if (equal "=" sign)
         (progn
            (setq sign "+")
            (setq reset t)))
      (if (match-beginning 2)
         (progn
            (setq string (substring data (match-beginning 2) (match-end 2)))
            (setq amount (string-to-number (concat sign string))))
         (error "line %d, missing amount" (1+ (creditcard-current-line))))
      (if reset (setq sign "="))
      (list sign amount)))

(defun creditcard-form-transaction-data (sign amount)
"Given SIGN and AMOUNT, return a string suitable for placing in the
numeric region of a transaction, based on the defined input columns."
   (let* ((amount (abs amount))
         (width1 (- creditcard-amount-column creditcard-sign-column))
         (value (concat "%" (number-to-string creditcard-amount-width) ".2f"))
         (format-string (concat "%-" (number-to-string width1) 
                                "s" value)))
     (if (string= sign "-")
         (setq sign " "))
     (format format-string sign amount "")))

(defface creditcard-comment-out-face
  '((t (:slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-header-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-footer-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-date-face
  '((t (:slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-paid-face
  '((t (:slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-where-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-description-face
  '((t (:bold t
        :foreground "blue")))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-sign-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-amount-face
  '((t (:bold t)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-current-creditcard-face
  '((t (:bold t
        :slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defface creditcard-paid-face
  '((t (:foreground "dim gray"
        :slant italic)))
;  '((t (:weight light
;        :slant italic)))
  "Face used to highlight search matches in search result buffer.")

(defun creditcard-fontify-line ()
  (let ((beg (line-beginning-position))
        (end (line-end-position)))
    (remove-overlays beg end)
    (if (creditcard-paid-p)
        (overlay-put (make-overlay beg end nil t)
                     'face 'creditcard-paid-face)
      (if (string= (buffer-substring beg (1+ beg)) "%")
          (overlay-put (make-overlay beg end nil t)
                     'face 'creditcard-comment-out-face)
        (overlay-put (make-overlay (+ beg creditcard-date-column)
                                   (+ beg creditcard-date-column creditcard-date-width)
                                   nil t)
                     'face 'creditcard-date-face)
        (overlay-put (make-overlay (+ beg creditcard-paid-column)
                                   (+ beg creditcard-paid-column creditcard-paid-width)
                                   nil t)
                     'face 'creditcard-paid-face)
        (overlay-put (make-overlay (+ beg creditcard-where-column)
                                   (+ beg creditcard-where-column creditcard-where-width)
                                   nil t)
                     'face 'creditcard-where-face)
        (overlay-put (make-overlay (+ beg creditcard-description-column)
                                   (+ beg creditcard-description-column 
                                      creditcard-description-width)
                                   nil t)
                     'face 'creditcard-description-face)
        (overlay-put (make-overlay (+ beg creditcard-sign-column)
                                   (+ beg creditcard-sign-column creditcard-sign-width)
                                   nil t)
                     'face 'creditcard-sign-face)
        (overlay-put (make-overlay (+ beg creditcard-amount-column)
                                   (+ beg creditcard-amount-column creditcard-amount-width)
                                   nil t)
                     'face 'creditcard-amount-face)))))
  
(defun creditcard-fontify-buffer ()
  (save-excursion
    (goto-char (point-min))
    (forward-line 2)
    (overlay-put 
     (make-overlay (point-min) (point) nil t) 'face 'creditcard-header-face)
    (while (creditcard-find-next-transaction t)
      (creditcard-fontify-line))
    (forward-line 1)
    (overlay-put 
     (make-overlay (point) (point-max) nil t) 'face 'creditcard-footer-face)))

(defun creditcard-fontify-region (beg end)
  (save-excursion
    (goto-char beg)
    (beginning-of-line)
    (forward-line 2)
    (overlay-put 
     (make-overlay (point-min) (point) nil t) 'face 'creditcard-header-face)
    (while (and
            (creditcard-find-next-transaction t)
            (< (point) end))
      (creditcard-fontify-line))))

(defun creditcard-before (line1 line2)
  (let (num1 num2
        (date1 (substring line1 0 2))
        (date2 (substring line2 0 2))
        (month1 (substring line1 3 6))
        (month2 (substring line2 3 6))
        (year1 (substring line1 7 11))
        (year2 (substring line2 7 11)))
    (setq month1 (- 13 (length (member month1 creditcard-months))))
    (setq month2 (- 13 (length (member month2 creditcard-months))))
    (setq num1 (concat year1 (format "%02d" month1) date1))
    (setq num2 (concat year2 (format "%02d" month2) date2))
    (<= (string-to-number num1) (string-to-number num2))))

(defun creditcard-compare-rows ()
  (let ((line1 (buffer-substring-no-properties 
                (line-beginning-position) (line-end-position)))
        (line2 (save-excursion
                 (forward-line 1)
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position)))))
    (creditcard-before line1 line2)))

(defun creditcard-switch-rows ()
  (let ((inhibit-read-only t))
    (save-excursion
      (let ((pt (line-beginning-position)))
        (forward-line 1)
        (let ((oldline (buffer-substring pt (point))))
          (delete-region pt (point))
          (forward-line 1)
          (insert oldline))))))

(defun creditcard-order-region (beg end)
  "Put the records in order by date."
  (let (pt n)
    (save-excursion
      (goto-char beg)
      (setq pt (line-beginning-position))
      (goto-char end)
      (setq n (+ 1 (count-lines pt (line-beginning-position))))
      (let ((switch t)
            (ord)
            (i))
        (while (and switch (> n 1))
          (setq switch nil)
          (goto-char pt)
          (setq i 1)
          (while (< i n)
            (unless (creditcard-compare-rows)
              (creditcard-switch-rows)
              (setq switch t))
            (forward-line 1)
            (setq i (1+ i)))
          (setq n (- n 1)))))))
  
(defun creditcard-order-buffer ()
  (interactive)
  (remove-overlays (point-min) (point-max))
  (save-excursion
    (let (beg end)
      (goto-char (point-min))
      (re-search-forward 
       (concat "^%?[0-9]+ " creditcard-months-regexp " [0-9]+"))
      (setq beg (line-beginning-position))
      (goto-char (point-max))
      (re-search-backward 
       (concat "^%?[0-9]+ " creditcard-months-regexp " [0-9]+"))
      (setq end (line-beginning-position))
      (remove-overlays beg end)
      (creditcard-order-region beg end))
    (creditcard-fontify-buffer)))

(defun creditcard-order-latest ()
  (interactive)
  (save-excursion
    (let (beg end fend)
      (goto-char (point-max))
      (re-search-backward 
       (concat "^%?[0-9]+ " creditcard-months-regexp " [0-9]+"))
      (beginning-of-line)
      (setq end (point))
      (setq fend (1+ (line-end-position)))
      (forward-line -1)
      (while
          (and
;           (not (get-char-property (point) 'invisible))
           (not (creditcard-paid-p))
           (looking-at 
            (concat "^%?[0-9]+ " creditcard-months-regexp " [0-9]+")))
        (forward-line -1))
      (forward-line 1)
      (let ((pt (point)))
        (creditcard-order-region pt end)
        (creditcard-fontify-region pt fend)))))

(defun creditcard-toggle-comment-out ()
  (interactive)
  (let* ((inhibit-read-only t)
         (beg (line-beginning-position))
         (end (+ beg creditcard-date-width 1)))
    (save-excursion
      (beginning-of-line)
      (remove-overlays beg (line-end-position))
      (if (looking-at "^%")
          (progn
            (delete-char 1)
            (forward-char creditcard-date-width)
            (insert " ")
            (creditcard-fontify-line))
        (insert "%")
        (forward-char creditcard-date-width)
        (delete-char 1)
        (overlay-put (make-overlay beg (line-end-position) nil t)
                     'face 'creditcard-comment-out-face)))))

(defvar creditcard-killed-rows nil)
(make-variable-buffer-local 'creditcard-killed-rows)

(defun creditcard-kill-row ()
  (interactive)
  (let ((inhibit-read-only t)
        (pt (line-beginning-position)))
    (push (buffer-substring-no-properties pt (line-end-position))
          creditcard-killed-rows)
    (remove-overlays pt (line-end-position))
    (forward-line 1)
    (delete-region pt (point))))

(defun creditcard-yank-row ()
  (interactive)
  (if (<= (creditcard-current-line) 1)
      (message "Can't yank row here.")
    (goto-char (line-beginning-position))
    (if creditcard-killed-rows
        (let ((inhibit-read-only t))
          (insert (pop creditcard-killed-rows) "\n")
          (forward-line -1)
          (creditcard-fontify-line))
      (message "No killed rows."))))

(defun creditcard-recalculate ()
"Recalculate all transactions.  The final balance will be placed at
the end of the buffer."
   (interactive)
   (let ((inhibit-read-only t)
         (balance 0)
         (tbalance 0)
         line-points)
      (save-excursion
        (goto-char (point-min))
        (while (setq line-points (creditcard-find-next-transaction))
          (let* ((line-start (nth 0 line-points))
                 (data-start (nth 1 line-points))
                 (data-end (nth 2 line-points))
                 (paid-flag (buffer-substring 
                             (+ line-start creditcard-paid-column) 
                             (+ 1 line-start creditcard-paid-column)))
                 (data-string (buffer-substring data-start data-end))
                 (data-values (creditcard-parse-transaction-data data-string))
                 (sign (nth 0 data-values))
                 (amount (nth 1 data-values)))
            (setq balance (if (equal sign "=")
                              (- amount)
                            (if (equal paid-flag "x")
                                balance
                              (+ balance amount))))
            (setq tbalance (if (equal sign "=")
                              (- amount)
                             (+ tbalance amount)))))
        (goto-char (point-min))
        (re-search-forward "^Total")
        (beginning-of-line)
        (kill-region (point) (point-max))
        (insert "Total (reported): " (format "%15.2f" (- balance tbalance)) "\n")
        (insert "Total (unmarked): " (format "%15.2f" (- balance)) "\n")
        (insert "Total     (owed): " (format "%15.2f" (- tbalance)) "\n"))))
;        (message "pt 1")
;        (if 
;        (re-search-forward "^Total (reported):" nil t)
;        (progn
;        (message "pt 2")
;        (delete-region (point) (line-end-position))
;        (message "pt 3")
;        (insert " " (format "%15.2f" (- balance tbalance)))
;        (message "pt 4")
;)
;          (re-search-forward "^===" nil nil 2)
;          (forward-line 1)
;          (insert "Total (reported): " (format "%15.2f" (- balance tbalance)) "\n"))
;        (re-search-forward "^Total (unmarked):")
;        (message "pt 5")
;        (delete-region (point) (line-end-position))
;        (message "pt 6")
;        (insert " " (format "%15.2f" (- balance)))
;        (message "pt 7")
;        (re-search-forward "^Total   (owed)  :")
;        (message "pt 8")
;        (delete-region (point) (line-end-position))
;        (message "pt 9")
;        (insert " " (format "%15.2f" (- tbalance))))))

(defun creditcard-recalculate-region (beg end)
"Recalculate all transactions.  The final balance will be placed at
the end of the buffer."
   (interactive "r")
   (let ((inhibit-read-only t)
         (balance 0)
         line-points)
      (save-excursion
        (save-restriction
          (narrow-to-region beg end)
          (goto-char (point-min))
          (while (setq line-points (creditcard-find-next-transaction))
            (let* ((line-start (nth 0 line-points))
                   (data-start (nth 1 line-points))
                   (data-end (nth 2 line-points))
                   (paid-flag (buffer-substring 
                               (+ line-start creditcard-paid-column) 
                               (+ 1 line-start creditcard-paid-column)))
                   (data-string (buffer-substring data-start data-end))
                   (data-values (creditcard-parse-transaction-data data-string))
                   (sign (nth 0 data-values))
                   (amount (nth 1 data-values)))
              (setq balance (if (equal sign "=")
                                (- amount)
                              (+ balance amount)))))
;          (message (number-to-string balance))))))
          balance))))

(defun creditcard-save-buffer ()
  (interactive)
  (let ((inhibit-read-only t))
    (save-buffer)))

(provide 'creditcard)
