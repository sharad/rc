;;; sqlite.el --- sqlite

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: tools

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


;; from http://mysite.verizon.net/mbcladwell/sqlite.html#contents


(deh-section "Sqlite"
  (require 'widget)
  (require 'derived)
  (eval-when-compile
    (require 'wid-edit))


  (defvar sqlite-program "sqlite3" "Full path name of the SQLITE executable.")

  (defvar sqlite-db  "/tmp/example.db" "Full path name of the SQLITE database.")

  (defvar sqlite-process-buffer "*sqlite-process*" "*Name of the SQLITE process buffer. This is where SQL commands are sent.")

  (defvar sqlite-output-buffer "*sqlite-output-buffer*" "Name of the buffer to which all SQLITE output is redirected.")

  (defun start-sqlite ()
    (apply 'make-comint "sqlite-process"  sqlite-program  nil `(,sqlite-db ))
    (comint-redirect-send-command-to-process ".mode tabs" sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil)
    (get-buffer-create sqlite-output-buffer))

  (defun sqlite-insert (sql-command &optional show)
    (unless (get-buffer sqlite-process-buffer)
      (apply 'make-comint "sqlite-process"  sqlite-program  nil `(,sqlite-db )))
    ;; (get-buffer-create sqlite-output-buffer)
    ;; (set-buffer sqlite-output-buffer)			;1
    ;; (erase-buffer)		                        ;2
    ;; (comint-redirect-send-command-to-process sql-command sqlite-output-buffer (get-buffer-process sqlite-process-buffer) (not show))  ;3
    ;; (accept-process-output    ;need to wait to obtain results
    ;;  (get-buffer-process sqlite-process-buffer) 1)
    (comint-send-string (get-buffer sqlite-process-buffer) sql-command))



  (defun sqlite-query (sql-command)
    (unless (get-buffer sqlite-output-buffer)
      (start-sqlite))
    (set-buffer sqlite-output-buffer)			;1
    (erase-buffer)		                        	;2
    (comint-redirect-send-command-to-process sql-command sqlite-output-buffer (get-buffer-process sqlite-process-buffer) nil)  ;3
    (accept-process-output (get-buffer-process sqlite-process-buffer) 1)  ;need to wait to obtain results
    (let*  ((begin (goto-char (point-min)))		;4
            (end (goto-char (point-max)))
            (num-lines (count-lines begin end))
            (counter 0)
            (results-rows ()))
      (goto-char (point-min))
      (while ( < counter num-lines)
        (setq results-rows (cons (chomp (thing-at-point 'line)) results-rows))
        (forward-line)
        (setq counter (+ 1 counter)))
      (car `(,results-rows))))



  (defun chomp (str)
    "Trim whitespace from string"
    (let ((s (if (symbolp str)(symbol-name str) str)))
      (save-excursion
        (while (and
                (not (null (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
                (> (length s) (string-match "^\\( \\|\f\\|\t\\|\n\\)" s)))
          (setq s (replace-match "" t nil s)))
        (while (and
                (not (null (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
                (> (length s) (string-match "\\( \\|\f\\|\t\\|\n\\)$" s)))
          (setq s (replace-match "" t nil s))))
      s))


  (defvar sqlite-mode-hook nil
    "*List of functions to call when entering sqlite mode*")

  (defvar sqlite-menu-map nil
    "Menu for sqlite mode.")


  (if sqlite-menu-map
      nil
      (setq sqlite-menu-map (make-sparse-keymap "sqlite"))
      (define-key sqlite-menu-map [sqlite-reports]
        '("Sqlite reports" . sqlite-reports))
      (define-key sqlite-menu-map [add-antibody]
        '("Add Antibody" . add-antibody))
      (define-key sqlite-menu-map [add-gene]
        '("Add Gene" . add-gene))
      (define-key sqlite-menu-map [add-abresult]
        '("Add Antibody Results" . add-abresult)))

  (defvar sqlite-mode-map nil
    "Keybindings for sqlite mode")

  (if sqlite-mode-map
      nil
      (setq sqlite-mode-map (copy-keymap widget-keymap)))

  (define-key sqlite-mode-map [menu-bar sqlite]
    (cons "sqlite" sqlite-menu-map))


  (define-derived-mode sqlite-mode text-mode "sqlite"
                       "Major mode using sqlite for tau antibodies.
Special commands:
\\{sqlite-mode-map}"))


(provide 'sqlite-config)
;;; sqlite.el ends here

(testing
 (sqlite-insert "create table abc (mid text, taraa text);")
 (sqlite-insert "create table abc (mid text, taraa text);")
 (sqlite-insert "insert into midtarget values ('abc','cm');"))
