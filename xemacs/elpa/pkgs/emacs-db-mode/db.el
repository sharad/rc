;;; db.el --- Emacs Database Interface

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

;; http://www.squidoo.com/emacs-db-mode


;; Emacs Database Mode | A New Emacs Mode For Working With Databases
;; Emacs is a text editor, but more than that, it is a framework that
;; enables you to develop applications that can manipulate text in a
;; variety of ways. For example, when I am working with databases, I
;; want to create queries and send them to the database. I'll need a
;; text editor to create the query in the first place so it makes
;; sense that should add functionality to emacs that can easily send
;; my query straight to the database.

;; Let me demonstrate with a fairly substantial example that mimics
;; typical usage of comint mode. Normally comint is used for
;; interacting with an interpreter. I do a lot of database work with
;; Sybase so a new emacs mode that helped with that would be
;; great. Adapting this for other databases should be fairly
;; straightforward.

;; I can leverage off the useful sql-mode that already provides syntax
;; highlighting and database interaction. However, as far as I know,
;; it doesn't really help me with my favourite usage style where I
;; have a command window containing my queries and a separate results
;; window.

;; When you have finished looking at this lens, have a look at the
;; db-mode extension for working with multiple databases.  Contents at
;; a Glance

;;     Emacs Core Libraries
;;     Constants
;;     Choosing Between Different Databases
;;     Low Level Wrappers
;;     Post Connection Hooks
;;     Inserting Text Into The Result Buffer

;; More
;; Emacs Core Libraries
;; I use these libraries for almost all comint based code.

(require 'cl)
(require 'comint)
(require 'derived)
;; Constants
;; Specify where the interpreter lives

(defconst *db-cmd* "/usr/bin/isql")
;; Choosing Between Different Databases
;; I have a bunch of different databases I need to connect to so these are the variables and accessors I use.

(defvar db-server "")
(defvar db-database "")
(defvar db-user "")
(defvar db-password "")

(defun db-set-params (server database user password)
  (setq db-server server
        db-database database
        db-user user
        db-password password))

(db-set-params "DB_SERVER" "database" "user" "password")
;; Low Level Wrappers
;; I wrap the low level stuff for accessing the buffer and associated process.

(defvar db-buffer "db")

(defsubst db-get-buffer () (concat "*" db-buffer "*"))

(defsubst db-get-process ()
  (get-buffer-process (db-get-buffer)))
;; Post Connection Hooks
;; And I allow for a set of functions to be called on connection to the database.

(defvar db-post-connect-hooks nil
  "List of functions to call after connecting to the database")
;; Inserting Text Into The Result Buffer
;; I need to insert text into the database buffer from time to time. ordinary-insertion-filter is taken from this link about filter functions.

(defun ordinary-insertion-filter (proc string)
  (with-current-buffer (process-buffer proc)
    (let ((moving (= (point) (process-mark proc))))
      (save-excursion
        ;; Insert the text, advancing the process marker.
        (goto-char (process-mark proc))
        (insert string)
        (set-marker (process-mark proc) (point)))
      (if moving (goto-char (process-mark proc))))))

(defun db-insert-text (str)
  (ordinary-insertion-filter (db-get-process) str))
;; Ugly Results Without Filtering
;; Emacs will be connecting to the stdin and stdout of isql, but the problem is that when we send a query to isql, it doesn't echo back what the query was. For example:

;; echo 'select *
;;   from person
;; where surname = "Doe"' | isql


;; gives the following output

;; 1> 2> 3> name surname
;; ---- -------
;; John Doe

;; (1 row affected)
;; Required (prettier) Result
;; I think those prompts 1> 2> 3> look ugly so I want to remove them and I also want to echo the query before the results.

;; select *
;;   from person
;;  where surname = "Doe"

;; name surname
;; ---- -------
;; John Doe

;; (1 row affected)
;; Adding The Filter
;; To remove the prompts we add a simple filter to comint-preoutput-filter-functions.

(defconst db-re-isql-prompt (concat "^ *"
                                    "\\([0-9]+> +\\)"
                                    "\\([0-9]+> +\\)*"))

(defun db-output-filter (output)
  (let ((match (string-match db-re-isql-prompt output)))
    (when (and (numberp match) (= 0 match))
      (setq output (concat (substring output 0 (match-beginning 0))
                           (substring output (match-end 0))))))
  (when (string-match "affected)[ \n]+\\(1>\\)" output)
    (setq output (concat (substring output 0 (match-beginning 1)) "\n")))
  output)

(add-hook 'comint-preoutput-filter-functions 'db-output-filter)
;; (remove-hook 'comint-preoutput-filter-functions 'db-output-filter)


;; I will add the echo query when the query is sent to the stdin of isql.
;; Connecting to the Database
;; Connecting to the db uses the typical (apply 'make-comint ...) call we mentioned before. At this point I add the required configuration to the output buffer. I particularly like to toggle-truncate-lines as then my query result will line up nicely.

(defun db-cmd-args ()
  (list "-w9999"
        (concat "-S" db-server)
        (concat "-D" db-database)
        (concat "-U" db-user)
        (concat "-P" db-password)))

(defun db-connect-to-db ()
  (interactive)
  (when (not (comint-check-proc (db-get-buffer)))
    (message "Connecting to the database...")
    (apply 'make-comint db-buffer *db-cmd* nil (db-cmd-args))
    (set-buffer (db-get-buffer))
    (toggle-truncate-lines 1)
    (setq comint-move-point-for-output t)
    (add-hook 'comint-output-filter-functions
              'comint-postoutput-scroll-to-bottom nil t)
    (run-hooks 'db-post-connect-hooks))
  t)
;; Adding Quote Words Syntax
;; Because I am controlling all of the input that isql sees, I can add some new syntax. I often do a lot of WHERE field in ("val1", "val2", ...) type queries and it is annoying to add all the quotes and commas. Wouldn't it be nice if I had something similar to qw(...) syntax in perl?

;; (disclaimer: this might be a bad idea, I haven't thought it through in a lot of detail. It is more to demonstrate what is possible)

(defun db-replace-quote-words (sql)
  (let ((case-fold-search nil))
    (with-temp-buffer
      (insert sql)
      (goto-char (point-min))
      (while (re-search-forward "qw(" nil t)
        (let (begin end words)
          (setq begin (point))
          (backward-char)
          (forward-sexp)
          (setq end (point))
          (goto-char begin)
          (while (re-search-forward "[^ \n]+" end t)
            (push (match-string 0) words))
          (delete-region (- begin 3) end)
          (insert (format "(%s)"
                          (mapconcat (lambda (s)
                                       (concat "\""
                                               (replace-quote s)
                                               "\""))
                                     (reverse words) ", ")))))
      (buffer-substring-no-properties (point-min) (point-max)))))


;; I replace all the qw(...) sections prior to sending the query to isql.
;; Sending Queries to the Database

(defun db-send-sql-region (start end)
  (interactive "r")
  (db-connect-to-db)
  (db-set-window-layout)

  (let* ((sql (buffer-substring-no-properties start end)))
    (setq sql (db-replace-quote-words sql))
    (db-insert-text sql)
    (when (string-match "^go" sql)
      (db-insert-text "\n\n"))
    (comint-send-string (db-get-process) sql)
    (comint-send-string (db-get-process) "\n")))

;; Send The Current SQL
;; I added db-send-sql-current that sends the current query. It assumes that queries are seperated by at least one blank line.

(defun db-send-sql-current ()
  (interactive)
  (save-excursion
    (backward-paragraph)
    (when (not (bobp))
      (next-line))
    (move-beginning-of-line nil)
    (let ((s (point)))
      (forward-paragraph)
      (when (not (eobp))
        (next-line -1))
      (move-end-of-line nil)
      (db-send-sql-region s (point)))))

;; Deriving DB Mode
;; I derive my new mode from sql-mode to get the syntax-highlighting for free.

(define-derived-mode db-mode sql-mode "Database"
  "Major mode for interacting with databases.
Special commands:
\\{db-mode-map}")

;; Useful Key Chords
;; C-x C-e mimics evaluation in other emacs inferior interpreter modes.

(define-key db-mode-map (kbd "C-x C-e") 'db-send-sql-current)
(define-key db-mode-map (kbd "C-c C-r") 'db-send-sql-region)

;; An Easy Way to Start DB Mode
;; enter-db-mode is a convenience function.

(defun db-set-window-layout ()
  (switch-to-buffer-other-window (db-get-buffer) t)
  (other-window -1))

;;;###autoload
(defun enter-db-mode ()
  (interactive)
  (switch-to-buffer "*sql*" t)
  (db-mode)
  (db-connect-to-db)
  (db-set-window-layout))

;; binding in binding.el





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; http://www.squidoo.com/multiple-databases
;; HowTo Query Multiple Databases With Emacs Db Mode
;; Set The Database Parameters
;; Let me remind you how we set the database parameters using db-set-params.

(defvar db-server "")
(defvar db-database "")
(defvar db-user "")
(defvar db-password "")

(defun db-set-params (server database user password)
  (setq db-server server
        db-database database
        db-user user
        db-password password))
;; Database Static Data
;; Right - so in my company we have a bunch of databases that store client order details. Each region has its own orders database and there are two user accounts that I generally use - one for querying the database (restricted) and the other for updating the data (admin).

;; Okay, so the first thing we need is something that maps short aliases to database details - server, database, user and password. I'll store the details in a vector.

;; Let's say (for the purposes of the example) that all of the accounts have the same password :)

(defconst ro-pwd "********")
(defconst rw-pwd "********")

(defun _db-one-info (region server user password)
  (cons region (vector server "orders" user password)))
;; The Admin and Restricted Users
;; We have a convenience function for creating the restricted and admin users.

(defun _db-region-info (region p-server q-server)
  (list (_db-one-info (concat region "-ro") p-server "restricted" ro-pwd)
        (_db-one-info (concat region "-rw") p-server "admin" rw-pwd)))
;; Creating The Database Aliases
;; Then we create the aliases for London, Frankfurt and Mexico and we filter out the aliases using mapcar.

(defconst *db-info*
  (append (_db-region-info "ldn" "LDN_ORDERS_DB" "LDN_ORDERS_QA")
          (_db-region-info "fft" "FFT_ORDERS_DB" "FFT_ORDERS_QA")
          (_db-region-info "mex" "MEX_ORDERS_DB" "MEX_ORDERS_QA")))

(defconst *db-valid-regions*
  (mapcar (lambda (e) (car e)) *db-info*))

(defvar db-region "ldn-ro")
;; The Resultant Data
;; This is what the static data looks like (in the actual data there are more production databases and QA aswell):

;; (insert (format "%s" *db-info*))

;; ((ldn-ro . [LDN_ORDERS_DB orders restricted ********])
;;  (ldn-rw . [LDN_ORDERS_DB orders admin ********])
;;  (fft-ro . [FFT_ORDERS_DB orders restricted ********])
;;  (fft-rw . [FFT_ORDERS_DB orders admin ********])
;;  (mex-ro . [MEX_ORDERS_DB orders restricted ********])
;;  (mex-rw . [MEX_ORDERS_DB orders admin ********]))


;; Retrieving The Database Details
;; And finally (for the static data) we have helper functions to select each piece of data depending on the region.

(defsubst db-get-param (region ref)
  (let ((tuple (assoc region *db-info*)))
    (if tuple
        (aref (cdr tuple) ref)
      "UNKNOWN")))

(defsubst db-get-server (region) (db-get-param region 0))
(defsubst db-get-database (region) (db-get-param region 1))
(defsubst db-get-user (region) (db-get-param region 2))
(defsubst db-get-password (region) (db-get-param region 3))
;; Setting the Mode Line
;; Another feature we want is that the mode line displays which region we are looking at.

(defvar db-mode-string "")

(defun db-set-mode-string ()
  (setq db-mode-string (format "(db-region: %s)" db-region)))
;; mode-line-format
;; My default mode-line-format looks like this:

;; (%e - mode-line-mule-info mode-line-client
;;       mode-line-modified mode-line-remote
;;       mode-line-frame-identification
;;       mode-line-buffer-identification
;;       mode-line-position (vc-mode vc-mode)
;;       mode-line-modes
;;       (which-func-mode ( which-func-format --))
;;       (global-mode-string (-- global-mode-string)) -%-)


;; As you can see, it is a list with a bunch of variables, some function calls and some special codes beginning with or containing a %. For example -%- produces an infinite number of dashes. Almost all modelines have this as a terminator.

;; If you add a variable to the mode-line-format, updating the variable automatically updates the modeline.
;; The Mode Line Update Function
;; The first thing we need to do is see if the db-mode-string variable is part of the mode-line-format variable already using memq. If it is, we can just set it and force a mode-line-update. If not, we need to insert the variable before the final -%-.

(defun db-update-mode-line ()
  (unless (memq 'db-mode-string mode-line-format)
    (setq mode-line-format (delete "-%-" mode-line-format))
    (setq mode-line-format
          (append mode-line-format '(db-mode-string "-%-"))))
  (db-set-mode-string)
  (force-mode-line-update))

(add-hook 'db-mode-hook 'db-update-mode-line)
;; Selecting The Database By Its Alias
;; Finally, the main function - db-set-region
;; db-set-region enables you to choose between multiple databases by the alias you selected. It uses ido so if you have enabled flex matching you only need to type enough characters to distinguish the alias you need. For example, if I type x, then w there is only one alias that contains those two letters and that is mex-rw.

;; It kills any active database process and sets up the parameters appropriately for next time we connect.

(defun db-set-region (&optional region)
  (interactive)
  (unless region
    (setq region
          (ido-completing-read (format "New region (%s): " db-region)
                               *db-valid-regions*
                               nil t)))
  (if (and (stringp region) (> (length region) 0))
      (let ((proc (db-get-process)))
        (when proc
          (kill-process proc)
          (kill-buffer (db-get-buffer)))
        (setq db-region region)
        (db-set-params (db-get-server db-region)
                       (db-get-database db-region)
                       (db-get-user db-region)
                       (db-get-password db-region))
        (db-set-mode-string)
        (message "Database region set to %s" region))
    (error "Failed to set region")))





(provide 'db)
;;; db.el ends here
