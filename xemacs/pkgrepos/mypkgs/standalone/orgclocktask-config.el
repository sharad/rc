;;; orgclocktask-config.el --- Org task clock reporting

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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

;; YEs

;;; Code:

(require 'init-config "~/.xemacs/init-config.el")
(require 'org-table)
(require 'org-clock)

(deh-section "original clocktable"

  (defcustom org-clock-clocktable-alt-formatter 'org-clocktable-alt-write-default
  "Function to turn clocking data into a table.
For more information, see `org-clocktable-write-default'."
  :group 'org-clocktable
  :version "24.1"
  :type 'function)

  (defun org-clocktable-alt-write-default (ipos tables params)
    "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
    ;; This function looks quite complicated, mainly because there are a
    ;; lot of options which can add or remove columns.  I have massively
    ;; commented this function, the I hope it is understandable.  If
    ;; someone wants to write their own special formatter, this maybe
    ;; much easier because there can be a fixed format with a
    ;; well-defined number of columns...
    (let* ((hlchars '((1 . "*") (2 . "/")))
           (lwords (assoc (or (plist-get params :lang)
                              (org-bound-and-true-p org-export-default-language)
                              "en")
                          org-clock-clocktable-language-setup))
           (multifile (plist-get params :multifile))
           (block (plist-get params :block))
           (sort (plist-get params :sort))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (header (plist-get  params :header))
           (narrow (plist-get params :narrow))
           (ws (or (plist-get params :wstart) 1))
           (ms (or (plist-get params :mstart) 1))
           (link (plist-get params :link))
           (maxlevel (or (plist-get params :maxlevel) 3))
           (emph (plist-get params :emphasize))
           (level-p (plist-get params :level))
           (org-time-clocksum-use-effort-durations
            (plist-get params :effort-durations))
           (timestamp (plist-get params :timestamp))
           (properties (plist-get params :properties))
           (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
           (rm-file-column (plist-get params :one-file-with-archives))
           (indent (plist-get params :indent))
           (case-fold-search t)
           range-text total-time tbl level hlc formula pcol
           file-time entries entry headline
           recalc content narrow-cut-p tcol)

      ;; Implement abbreviations
      (when (plist-get params :compact)
        (setq level nil indent t narrow (or narrow '40!) ntcol 1))

      ;; Some consistency test for parameters
      (unless (integerp ntcol)
        (setq params (plist-put params :tcolumns (setq ntcol 100))))

      (when (and narrow (integerp narrow) link)
        ;; We cannot have both integer narrow and link
        (message
         "Using hard narrowing in clocktable to allow for links")
        (setq narrow (intern (format "%d!" narrow))))

      (when narrow
        (cond
          ((integerp narrow))
          ((and (symbolp narrow)
                (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
           (setq narrow-cut-p t
                 narrow (string-to-number (substring (symbol-name narrow)
                                                     0 -1))))
          (t
           (error "Invalid value %s of :narrow property in clock table"
                  narrow))))

      (when block
        ;; Get the range text for the header
        (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

      ;; Compute the total time
      (setq total-time (apply '+ (mapcar 'cadr tables)))

      ;; Now we need to output this tsuff
      (goto-char ipos)

      ;; Insert the text *before* the actual table
      (insert-before-markers
       (or header
           ;; Format the standard header
           (concat
            "#+CAPTION: "
            (nth 9 lwords) " ["
            (substring
             (format-time-string (cdr org-time-stamp-formats))
             1 -1)
            "]"
            (if block (concat ", for " range-text ".") "")
            "\n")))

      ;; Insert the narrowing line
      (when (and narrow (integerp narrow) (not narrow-cut-p))
        (insert-before-markers
         "|"                            ; table line starter
         (if multifile "|" "")          ; file column, maybe
         (if level-p   "|" "")          ; level column, maybe
         (if timestamp "|" "")          ; timestamp column, maybe
         (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
         (format "<%d>| |\n" narrow)))  ; headline and time columns

      ;; Insert the table header line
      (insert-before-markers
       "|"                              ; table line starter
       (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
       (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
       (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
       (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
       (concat (nth 4 lwords) "|"
               (nth 5 lwords) "|\n"))                 ; headline and time columns

      ;; Insert the total time in the table
      (insert-before-markers
       "|-\n"                            ; a hline
       "|"                               ; table line starter
       (if multifile (concat "| " (nth 6 lwords) " ") "")
					; file column, maybe
       (if level-p   "|"      "")        ; level column, maybe
       (if timestamp "|"      "")        ; timestamp column, maybe
       (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
       (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
       (format org-clock-total-time-cell-format
               (org-minutes-to-clocksum-string (or total-time 0))) ; the time
       "|\n")                          ; close line

      ;; Now iterate over the tables and insert the data
      ;; but only if any time has been collected
      (when (and total-time (> total-time 0))

        (while (setq tbl (pop tables))
          ;; now tbl is the table resulting from one file.
          (setq file-time (nth 1 tbl))
          (when (or (and file-time (> file-time 0))
                    (not (plist-get params :fileskip0)))
            (insert-before-markers "|-\n")  ; a hline because a new file starts
            ;; First the file time, if we have multiple files
            (when multifile
              ;; Summarize the time collected from this file
              (insert-before-markers
               (format (concat "| %s %s | %s%s"
                               (format org-clock-file-time-cell-format (nth 8 lwords))
                               " | *%s*|\n")
                       (file-name-nondirectory (car tbl))
                       (if level-p   "| " "") ; level column, maybe
                       (if timestamp "| " "") ; timestamp column, maybe
                       (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
                       (org-minutes-to-clocksum-string (nth 1 tbl))))) ; the time

            ;; Get the list of node entries and iterate over it
            (setq entries (nth 2 tbl))
            (while (setq entry (pop entries))
              (setq level (car entry)
                    headline (nth 1 entry)
                    hlc (if emph (or (cdr (assoc level hlchars)) "") ""))
              (when narrow-cut-p
                (if (and (string-match (concat "\\`" org-bracket-link-regexp
                                               "\\'")
                                       headline)
                         (match-end 3))
                    (setq headline
                          (format "[[%s][%s]]"
                                  (match-string 1 headline)
                                  (org-shorten-string (match-string 3 headline)
                                                      narrow)))
                    (setq headline (org-shorten-string headline narrow))))
              (insert-before-markers
               "|"                      ; start the table line
               (if multifile "|" "")    ; free space for file name column?
               (if level-p (format "%d|" (car entry)) "")   ; level, maybe
               (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
               (if properties
                   (concat
                    (mapconcat
                     (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
                     properties "|") "|") "")  ;properties columns, maybe
               (if indent (org-clocktable-indent-string level) "") ; indentation
               hlc headline hlc "|"                                ; headline
               (make-string (min (1- ntcol) (or (- level 1))) ?|)
					; empty fields for higher levels
               hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
               "|\n"                                               ; close line
               )))))
      ;; When exporting subtrees or regions the region might be
      ;; activated, so let's disable ̀delete-active-region'
      (let ((delete-active-region nil)) (backward-delete-char 1))
      (if (setq formula (plist-get params :formula))
          (cond
            ((eq formula '%)
             ;; compute the column where the % numbers need to go
             (setq pcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)
                           (min maxlevel (or ntcol 100))))
             ;; compute the column where the total time is
             (setq tcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)))
             (insert
              (format
               "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
               pcol            ; the column where the % numbers should go
               (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
               tcol            ; column of the total time
               tcol (1- pcol)  ; range of columns where times can be found
               ))
             (setq recalc t))
            ((stringp formula)
             (insert "\n#+TBLFM: " formula)
             (setq recalc t))
            (t (error "Invalid formula in clocktable")))
          ;; Should we rescue an old formula?
          (when (stringp (setq content (plist-get params :content)))
            (when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
              (setq recalc t)
              (insert "\n" (match-string 1 (plist-get params :content)))
              (beginning-of-line 0))))
      ;; Back to beginning, align the table, recalculate if necessary
      (goto-char ipos)
      (skip-chars-forward "^|")
      (org-table-align)
      (when org-hide-emphasis-markers
        ;; we need to align a second time
        (org-table-align))
      (when sort
        (save-excursion
          (org-table-goto-line 3)
          (org-table-goto-column (car sort))
          (org-table-sort-lines nil (cdr sort))))
      (when recalc
        (if (eq formula '%)
            (save-excursion
              (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
              (org-table-goto-column pcol nil 'force)
              (insert "%")))
        (org-table-recalculate 'all))
      (when rm-file-column
        ;; The file column is actually not wanted
        (forward-char 1)
        (org-table-delete-column))
      total-time))


  (defun org-clocktable-alt-with-content-note-write (ipos tables params)
    "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
    ;; This function looks quite complicated, mainly because there are a
    ;; lot of options which can add or remove columns.  I have massively
    ;; commented this function, the I hope it is understandable.  If
    ;; someone wants to write their own special formatter, this maybe
    ;; much easier because there can be a fixed format with a
    ;; well-defined number of columns...
    (let* ((hlchars '((1 . "*") (2 . "/")))
           (lwords (assoc (or (plist-get params :lang)
                              (org-bound-and-true-p org-export-default-language)
                              "en")
                          org-clock-clocktable-language-setup))
           (multifile (plist-get params :multifile))
           (block (plist-get params :block))
           (sort (plist-get params :sort))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (header (plist-get  params :header))
           (narrow (plist-get params :narrow))
           (ws (or (plist-get params :wstart) 1))
           (ms (or (plist-get params :mstart) 1))
           (link (plist-get params :link))
           (maxlevel (or (plist-get params :maxlevel) 3))
           (emph (plist-get params :emphasize))
           (level-p (plist-get params :level))
           (org-time-clocksum-use-effort-durations
            (plist-get params :effort-durations))
           (timestamp (plist-get params :timestamp))
           (properties (plist-get params :properties))
           (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
           (rm-file-column (plist-get params :one-file-with-archives))
           (indent (plist-get params :indent))
           (case-fold-search t)
           range-text total-time tbl level hlc formula pcol
           file-time entries entry headline content notes
           recalc content narrow-cut-p tcol)

      ;; Implement abbreviations
      (when (plist-get params :compact)
        (setq level nil indent t narrow (or narrow '40!) ntcol 1))

      ;; Some consistency test for parameters
      (unless (integerp ntcol)
        (setq params (plist-put params :tcolumns (setq ntcol 100))))

      (when (and narrow (integerp narrow) link)
        ;; We cannot have both integer narrow and link
        (message
         "Using hard narrowing in clocktable to allow for links")
        (setq narrow (intern (format "%d!" narrow))))

      (when narrow
        (cond
          ((integerp narrow))
          ((and (symbolp narrow)
                (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
           (setq narrow-cut-p t
                 narrow (string-to-number (substring (symbol-name narrow)
                                                     0 -1))))
          (t
           (error "Invalid value %s of :narrow property in clock table"
                  narrow))))

      (when block
        ;; Get the range text for the header
        (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

      ;; Compute the total time
      (setq total-time (apply '+ (mapcar 'cadr tables)))

      ;; Now we need to output this tsuff
      (goto-char ipos)

      ;; Insert the text *before* the actual table
      (insert-before-markers
       (or header
           ;; Format the standard header
           (concat
            "#+CAPTION: "
            (nth 9 lwords) " ["
            (substring
             (format-time-string (cdr org-time-stamp-formats))
             1 -1)
            "]"
            (if block (concat ", for " range-text ".") "")
            "\n")))

      ;; Insert the narrowing line
      (when (and narrow (integerp narrow) (not narrow-cut-p))
        (insert-before-markers
         "|"                            ; table line starter
         (if multifile "|" "")          ; file column, maybe
         (if level-p   "|" "")          ; level column, maybe
         (if timestamp "|" "")          ; timestamp column, maybe
         (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
         (format "<%d>| |\n" narrow)))  ; headline and time columns

      ;; Insert the table header line
      (insert-before-markers
       "|"                              ; table line starter
       (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
       (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
       (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
       (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
       (concat (nth 4 lwords) "|"
               (nth 5 lwords) "|\n"))                 ; headline and time columns

      ;; Insert the total time in the table
      (insert-before-markers
       "|-\n"                            ; a hline
       "|"                               ; table line starter
       (if multifile (concat "| " (nth 6 lwords) " ") "")
					; file column, maybe
       (if level-p   "|"      "")        ; level column, maybe
       (if timestamp "|"      "")        ; timestamp column, maybe
       (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
       (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
       (format org-clock-total-time-cell-format
               (org-minutes-to-clocksum-string (or total-time 0))) ; the time
       "|\n")                          ; close line

      ;; Now iterate over the tables and insert the data
      ;; but only if any time has been collected
      (when (and total-time (> total-time 0))

        (while (setq tbl (pop tables))
          ;; now tbl is the table resulting from one file.
          (setq file-time (nth 1 tbl))
          (when (or (and file-time (> file-time 0))
                    (not (plist-get params :fileskip0)))
            (insert-before-markers "|-\n")  ; a hline because a new file starts
            ;; First the file time, if we have multiple files
            (when multifile
              ;; Summarize the time collected from this file
              (insert-before-markers
               (format (concat "| %s %s | %s%s"
                               (format org-clock-file-time-cell-format (nth 8 lwords))
                               " | *%s*|\n")
                       (file-name-nondirectory (car tbl))
                       (if level-p   "| " "") ; level column, maybe
                       (if timestamp "| " "") ; timestamp column, maybe
                       (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
                       (org-minutes-to-clocksum-string (nth 1 tbl))))) ; the time

            ;; Get the list of node entries and iterate over it
            (setq entries (nth 2 tbl))
            (while (setq entry (pop entries))
              (setq level (car entry)
                    headline (nth 1 entry)
                    content  (nth 5 entry)
                    notes    (nth 6 entry)
                    hlc (if emph (or (cdr (assoc level hlchars)) "") ""))
              (when narrow-cut-p
                (if (and (string-match (concat "\\`" org-bracket-link-regexp
                                               "\\'")
                                       headline)
                         (match-end 3))
                    (setq headline
                          (format "[[%s][%s]]"
                                  (match-string 1 headline)
                                  (org-shorten-string (match-string 3 headline)
                                                      narrow)))
                    (setq headline (org-shorten-string headline narrow))))
              (insert-before-markers
               "|"                      ; start the table line
               (if multifile "|" "")    ; free space for file name column?
               (if level-p (format "%d|" (car entry)) "")   ; level, maybe
               (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
               (if properties
                   (concat
                    (mapconcat
                     (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
                     properties "|") "|") "")  ;properties columns, maybe
               (if indent (org-clocktable-indent-string level) "") ; indentation
               hlc headline hlc "|"                                ; headline
               ;; content "|" notes "|"                               ; new added sharad
               (make-string (min (1- ntcol) (or (- level 1))) ?|)
					; empty fields for higher levels
               hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
               "|\n"                                               ; close line
               )))))
      ;; When exporting subtrees or regions the region might be
      ;; activated, so let's disable ̀delete-active-region'
      (let ((delete-active-region nil)) (backward-delete-char 1))
      (if (setq formula (plist-get params :formula))
          (cond
            ((eq formula '%)
             ;; compute the column where the % numbers need to go
             (setq pcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)
                           (min maxlevel (or ntcol 100))))
             ;; compute the column where the total time is
             (setq tcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)))
             (insert
              (format
               "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
               pcol            ; the column where the % numbers should go
               (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
               tcol            ; column of the total time
               tcol (1- pcol)  ; range of columns where times can be found
               ))
             (setq recalc t))
            ((stringp formula)
             (insert "\n#+TBLFM: " formula)
             (setq recalc t))
            (t (error "Invalid formula in clocktable")))
          ;; Should we rescue an old formula?
          (when (stringp (setq content (plist-get params :content)))
            (when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
              (setq recalc t)
              (insert "\n" (match-string 1 (plist-get params :content)))
              (beginning-of-line 0))))
      ;; Back to beginning, align the table, recalculate if necessary
      (goto-char ipos)
      (skip-chars-forward "^|")
      (org-table-align)
      (when org-hide-emphasis-markers
        ;; we need to align a second time
        (org-table-align))
      (when sort
        (save-excursion
          (org-table-goto-line 3)
          (org-table-goto-column (car sort))
          (org-table-sort-lines nil (cdr sort))))
      (when recalc
        (if (eq formula '%)
            (save-excursion
              (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
              (org-table-goto-column pcol nil 'force)
              (insert "%")))
        (org-table-recalculate 'all))
      (when rm-file-column
        ;; The file column is actually not wanted
        (forward-char 1)
        (org-table-delete-column))
      total-time))

  (defun org-report-content-max-length (text)
    (if text
        (let ((maxlen 0))
          (apply #'max (mapcar #'length (split-string text "\n"))))
        0))
  (eval-when-compile
   (defmacro custom-set-max (max value)
    `(let ((val ,value))
       (if (< ,max val)
           (setq ,max val)))))
  (defun org-report-get-max-line-length (tbl)
    (let ((max 0)
          (len 0))
      (custom-set-max
       max
       (+ (length "* File: ")
          (length (file-relative-name (car tbl) default-directory))))
      (dolist (hl (nth 2 tbl))
        (let ((level (car hl))
              (headline (nth 1 hl))
              (content  (nth 5 hl))
              (notes    (nth 6 hl) ))
          (custom-set-max
           max
           (+ 1 level (length headline)))
          (custom-set-max
           max
           (+ 1 level (org-report-content-max-length content)))
          (dolist (note notes)
            (custom-set-max
             max
             (+ 1 level (org-report-content-max-length note))))))
      max))

  (defun org-plain-alt-with-content-note-write (ipos tables params)
    "Write out a clock table at position IPOS in the current buffer.
TABLES is a list of tables with clocking data as produced by
`org-clock-get-table-data'.  PARAMS is the parameter property list obtained
from the dynamic block definition."
    ;; This function looks quite complicated, mainly because there are a
    ;; lot of options which can add or remove columns.  I have massively
    ;; commented this function, the I hope it is understandable.  If
    ;; someone wants to write their own special formatter, this maybe
    ;; much easier because there can be a fixed format with a
    ;; well-defined number of columns...
    (let* ((hlchars '((1 . "*") (2 . "/")))
           (lwords (assoc (or (plist-get params :lang)
                              (org-bound-and-true-p org-export-default-language)
                              "en")
                          org-clock-clocktable-language-setup))
           (multifile (plist-get params :multifile))
           (block (plist-get params :block))
           (sort (plist-get params :sort))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (header (plist-get  params :header))
           (narrow (plist-get params :narrow))
           (ws (or (plist-get params :wstart) 1))
           (ms (or (plist-get params :mstart) 1))
           (link (plist-get params :link))
           (maxlevel (or (plist-get params :maxlevel) 3))
           (emph (plist-get params :emphasize))
           (level-p (plist-get params :level))
           (org-time-clocksum-use-effort-durations
            (plist-get params :effort-durations))
           (timestamp (plist-get params :timestamp))
           (properties (plist-get params :properties))
           (ntcol (max 1 (or (plist-get params :tcolumns) 100)))
           (rm-file-column (plist-get params :one-file-with-archives))
           (indent (plist-get params :indent))
           (case-fold-search t)
           range-text total-time tbl level hlc formula pcol
           file-time entries entry headline content notes
           (gap "                                  ")
           (max-report-line-len 0)
           recalc content narrow-cut-p tcol)

      ;; Implement abbreviations
      (when (plist-get params :compact)
        (setq level nil indent t narrow (or narrow '40!) ntcol 1))

      ;; Some consistency test for parameters
      (unless (integerp ntcol)
        (setq params (plist-put params :tcolumns (setq ntcol 100))))

      (when (and narrow (integerp narrow) link)
        ;; We cannot have both integer narrow and link
        (message
         "Using hard narrowing in clocktable to allow for links")
        (setq narrow (intern (format "%d!" narrow))))

      (when narrow
        (cond
          ((integerp narrow))
          ((and (symbolp narrow)
                (string-match "\\`[0-9]+!\\'" (symbol-name narrow)))
           (setq narrow-cut-p t
                 narrow (string-to-number (substring (symbol-name narrow)
                                                     0 -1))))
          (t
           (error "Invalid value %s of :narrow property in clock table"
                  narrow))))

      (when block
        ;; Get the range text for the header
        (setq range-text (nth 2 (org-clock-special-range block nil t ws ms))))

      ;; Compute the total time
      (setq total-time (apply '+ (mapcar 'cadr tables)))

      (setq max-report-line-len (apply #'max (mapcar 'org-report-get-max-line-length tables)))

      ;; Now we need to output this tsuff
      (goto-char ipos)

      ;; Insert the text *before* the actual table
      (insert-before-markers
       (or header
           ;; Format the standard header
           (concat
            "#+CAPTION: "
            (nth 9 lwords) " ["
            (substring
             (format-time-string (cdr org-time-stamp-formats))
             1 -1)
            "]"
            (if block (concat ", for " range-text ".") "")
            "\n")))

      (when nil
       ;; Insert the narrowing line
       (when (and narrow (integerp narrow) (not narrow-cut-p))
         (insert-before-markers
          "|"                            ; table line starter
          (if multifile "|" "")          ; file column, maybe
          (if level-p   "|" "")          ; level column, maybe
          (if timestamp "|" "")          ; timestamp column, maybe
          (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
          (format "<%d>| |\n" narrow)))  ; headline and time columns

       ;; Insert the table header line
       (insert-before-markers
        "|"                              ; table line starter
        (if multifile (concat (nth 1 lwords) "|") "")  ; file column, maybe
        (if level-p   (concat (nth 2 lwords) "|") "")  ; level column, maybe
        (if timestamp (concat (nth 3 lwords) "|") "")  ; timestamp column, maybe
        (if properties (concat (mapconcat 'identity properties "|") "|") "") ;properties columns, maybe
        (concat (nth 4 lwords) "|"
                (nth 5 lwords) "|\n"))                 ; headline and time columns
       )

      (when nil
       ;; Insert the total time in the table
       (insert-before-markers
        "|-\n"                            ; a hline
        "|"                               ; table line starter
        (if multifile (concat "| " (nth 6 lwords) " ") "")
					; file column, maybe
        (if level-p   "|"      "")        ; level column, maybe
        (if timestamp "|"      "")        ; timestamp column, maybe
        (if properties (make-string (length properties) ?|) "")  ; properties columns, maybe
        (concat (format org-clock-total-time-cell-format (nth 7 lwords))  "| ") ; instead of a headline
        (format org-clock-total-time-cell-format
                (org-minutes-to-clocksum-string (or total-time 0))) ; the time
        "|\n")                          ; close line
       )

      ;; Now iterate over the tables and insert the data
      ;; but only if any time has been collected
      (when (and total-time (> total-time 0))

        (while (setq tbl (pop tables))
          ;; now tbl is the table resulting from one file.
          (setq file-time (nth 1 tbl))
          (when (or (and file-time (> file-time 0))
                    (not (plist-get params :fileskip0)))
            ;; (insert-before-markers "|-\n")  ; a hline because a new file starts
            ;; First the file time, if we have multiple files
            (when multifile
              ;; Summarize the time collected from this file
              (let ((filename (file-relative-name (car tbl) default-directory)))
               (insert-before-markers
                (format (concat "File: %s"
                                (make-string (- max-report-line-len (+ (length "File: ") (length filename))) ?\ )
                                " %s"
                                "\n")
                        ;; (file-name-nondirectory (car tbl))
                        filename
                        ;; (if level-p   "| " "") ; level column, maybe
                        ;; (if timestamp "| " "") ; timestamp column, maybe
                        ;; (if properties (make-string (length properties) ?|) "")  ;properties columns, maybe
                        (org-minutes-to-clocksum-string (nth 1 tbl)))))) ; the time

            ;; Get the list of node entries and iterate over it
            (setq entries (nth 2 tbl))
            (while (setq entry (pop entries))
              (setq level (car entry)
                    headline (nth 1 entry)
                    content  (nth 5 entry)
                    notes    (nth 6 entry)
                    hlc (if emph (or (cdr (assoc level hlchars)) "") ""))
              (when narrow-cut-p
                (if (and (string-match (concat "\\`" org-bracket-link-regexp
                                               "\\'")
                                       headline)
                         (match-end 3))
                    (setq headline
                          (format "[[%s][%s]]"
                                  (match-string 1 headline)
                                  (org-shorten-string (match-string 3 headline)
                                                      narrow)))
                    ;; (setq headline (org-shorten-string headline narrow))
                    ))
              (insert-before-markers
               ;; "|"                      ; start the table line
               ;; (if multifile "|" "")    ; free space for file name column?
               ;; (if level-p (format "%d|" (car entry)) "")   ; level, maybe
               ;; (if timestamp (concat (nth 2 entry) "|") "") ; timestamp, maybe
               ;; (if properties
               ;;     (concat
               ;;      (mapconcat
               ;;       (lambda (p) (or (cdr (assoc p (nth 4 entry))) ""))
               ;;       properties "|") "|") "")  ;properties columns, maybe
               ;; (if indent (org-clocktable-indent-string level) "") ; indentation
               (make-string level ?*)
               " "
               hlc headline hlc                                       ; headline
               ;; content "|" notes "|"                            ; new added sharad
               ;; (make-string (min (1- ntcol) (or (- level 1))) ?|)
					; empty fields for higher levels
               (make-string (- max-report-line-len (+ level (length headline))) ?\ )
               hlc (org-minutes-to-clocksum-string (nth 3 entry)) hlc ; time
               ;; "|\n"                                             ; close line
               "\n"
               (if content  (concat content "\n") "")
               (if notes (mapconcat 'identity notes "\n") ""))))))
      ;; When exporting subtrees or regions the region might be
      ;; activated, so let's disable ̀delete-active-region'
      (let ((delete-active-region nil)) (backward-delete-char 1))
      (if (setq formula (plist-get params :formula))
          (cond
            ((eq formula '%)
             ;; compute the column where the % numbers need to go
             (setq pcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)
                           (min maxlevel (or ntcol 100))))
             ;; compute the column where the total time is
             (setq tcol (+ 2
                           (length properties)
                           (if multifile 1 0)
                           (if level-p 1 0)
                           (if timestamp 1 0)))
             (insert
              (format
               "\n#+TBLFM: $%d='(org-clock-time%% @%d$%d $%d..$%d);%%.1f"
               pcol            ; the column where the % numbers should go
               (if (and narrow (not narrow-cut-p)) 3 2) ; row of the total time
               tcol            ; column of the total time
               tcol (1- pcol)  ; range of columns where times can be found
               ))
             (setq recalc t))
            ((stringp formula)
             (insert "\n#+TBLFM: " formula)
             (setq recalc t))
            (t (error "Invalid formula in clocktable")))
          ;; Should we rescue an old formula?
          (when (stringp (setq content (plist-get params :content)))
            (when (string-match "^\\([ \t]*#\\+tblfm:.*\\)" content)
              (setq recalc t)
              (insert "\n" (match-string 1 (plist-get params :content)))
              (beginning-of-line 0))))
      (when nil
        ;; Back to beginning, align the table, recalculate if necessary
        (goto-char ipos)
        (skip-chars-forward "^|")
        (org-table-align)
        (when org-hide-emphasis-markers
          ;; we need to align a second time
          (org-table-align))
        (when sort
          (save-excursion
            (org-table-goto-line 3)
            (org-table-goto-column (car sort))
            (org-table-sort-lines nil (cdr sort))))
        (when recalc
          (if (eq formula '%)
              (save-excursion
                (if (and narrow (not narrow-cut-p)) (beginning-of-line 2))
                (org-table-goto-column pcol nil 'force)
                (insert "%")))
          (org-table-recalculate 'all))
        (when rm-file-column
          ;; The file column is actually not wanted
          (forward-char 1)
          (org-table-delete-column)))
      total-time))

  (defun org-get-clock-note ()
    (when (org-at-clock-log-p)
      (save-excursion
        (forward-line)
        (when (org-at-item-p)
          (let ((ele (org-element-at-point)))
            (let ((begin (org-element-property :contents-begin ele))
                  (end (org-element-property :contents-end ele)))
              (buffer-substring begin end)))))))


;;;###autoload
  (defun org-clock-sum-with-notes (&optional tstart tend headline-filter propname)
    "Sum the times for each subtree.
Puts the resulting times in minutes as a text property on each headline.
TSTART and TEND can mark a time range to be considered.
HEADLINE-FILTER is a zero-arg function that, if specified, is called for
each headline in the time range with point at the headline.  Headlines for
which HEADLINE-FILTER returns nil are excluded from the clock summation.
PROPNAME lets you set a custom text property instead of :org-clock-minutes."
    (org-with-silent-modifications
     (let* ((re (concat "^\\(\\*+\\)[ \t]\\|^[ \t]*"
                        org-clock-string
                        "[ \t]*\\(?:\\(\\[.*?\\]\\)-+\\(\\[.*?\\]\\)\\|=>[ \t]+\\([0-9]+\\):\\([0-9]+\\)\\)"))
            (lmax 30)
            (ltimes (make-vector lmax 0))
            (t1 0)
            (level 0)
            ts te dt
            time
            (clock-notes ()))
       (if (stringp tstart) (setq tstart (org-time-string-to-seconds tstart)))
       (if (stringp tend) (setq tend (org-time-string-to-seconds tend)))
       (if (consp tstart) (setq tstart (org-float-time tstart)))
       (if (consp tend) (setq tend (org-float-time tend)))
       (remove-text-properties (point-min) (point-max)
                               `(,(or propname :org-clock-minutes) t
                                  :org-clock-force-headline-inclusion t))
       (save-excursion
         (goto-char (point-max))
         (while (re-search-backward re nil t)
           (cond
             ((match-end 2)
              ;; Two time stamps
              (setq ts (match-string 2)
                    te (match-string 3)
                    ts (org-float-time
                        (apply 'encode-time (org-parse-time-string ts)))
                    te (org-float-time
                        (apply 'encode-time (org-parse-time-string te)))
                    ts (if tstart (max ts tstart) ts)
                    te (if tend (min te tend) te)
                    dt (- te ts)
                    t1 (if (> dt 0) (+ t1 (floor (/ dt 60))) t1))
              (push (org-get-clock-note) clock-notes))
             ((match-end 4)
              ;; A naked time
              (setq t1 (+ t1 (string-to-number (match-string 5))
                          (* 60 (string-to-number (match-string 4))))))
             (t ;; A headline
              ;; Add the currently clocking item time to the total
              (when (and org-clock-report-include-clocking-task
                         (equal (org-clocking-buffer) (current-buffer))
                         (equal (marker-position org-clock-hd-marker) (point))
                         tstart
                         tend
                         (>= (org-float-time org-clock-start-time) tstart)
                         (<= (org-float-time org-clock-start-time) tend))
                (let ((time (floor (- (org-float-time)
                                      (org-float-time org-clock-start-time)) 60)))
                  (setq t1 (+ t1 time))))
              (let* ((headline-forced
                      (get-text-property (point)
                                         :org-clock-force-headline-inclusion))
                     (headline-included
                      (or (null headline-filter)
                          (save-excursion
                            (save-match-data (funcall headline-filter))))))
                (setq level (- (match-end 1) (match-beginning 1)))
                (when (>= level lmax)
                  (setq ltimes (vconcat ltimes (make-vector lmax 0)) lmax (* 2 lmax)))
                (when (or (> t1 0) (> (aref ltimes level) 0))
                  (when (or headline-included headline-forced)
                    (if headline-included
                        (loop for l from 0 to level do
                             (aset ltimes l (+ (aref ltimes l) t1))))
                    (setq time (aref ltimes level))
                    (goto-char (match-beginning 0))
                    (put-text-property (point) (point-at-eol)
                                       (or propname :org-clock-minutes) time)
                    (put-text-property (point) (point-at-eol)
                                       :org-clock-notes
                                       (remove nil clock-notes))
                    (if headline-filter
                        (save-excursion
                          (save-match-data
                            (while
                                (> (funcall outline-level) 1)
                              (outline-up-heading 1 t)
                              (put-text-property
                               (point) (point-at-eol)
                               :org-clock-force-headline-inclusion t))))))
                  (setq t1 0)
                  (loop for l from level to (1- lmax) do
                       (aset ltimes l 0)))))))
         (setq org-clock-file-total-minutes (aref ltimes 0))))))


  (defun org-heading-content-only-x ()
    (if (org-at-heading-p)
        (save-excursion
          (save-restriction
            (let ((start
                   (progn
                     (goto-char (org-element-property :contents-begin (org-element-at-point)))
                     (org-end-of-meta-data t)
                     ;; (while (org-at-drawer-p)
                     ;;   (goto-char (org-element-property :end (org-element-at-point))))
                     ;; (if (org-at-heading-p) (backward-char))
                     (point))))
              (unless (org-at-heading-p)
                (progn
                  (outline-next-heading)
                  ;; (outline-next-visible-heading 1)
                  (backward-char)
                  (buffer-substring start (point)))))))))

  (defun org-clock-get-table-data (file params)
    "Get the clocktable data for file FILE, with parameters PARAMS.
FILE is only for identification - this function assumes that
the correct buffer is current, and that the wanted restriction is
in place.
The return value will be a list with the file name and the total
file time (in minutes) as 1st and 2nd elements.  The third element
of this list will be a list of headline entries.  Each entry has the
following structure:

  (LEVEL HEADLINE TIMESTAMP TIME)

LEVEL:     The level of the headline, as an integer.  This will be
           the reduced leve, so 1,2,3,... even if only odd levels
           are being used.
HEADLINE:  The text of the headline.  Depending on PARAMS, this may
           already be formatted like a link.
TIMESTAMP: If PARAMS require it, this will be a time stamp found in the
           entry, any of SCHEDULED, DEADLINE, NORMAL, or first inactive,
           in this sequence.
TIME:      The sum of all time spend in this tree, in minutes.  This time
           will of cause be restricted to the time block and tags match
           specified in PARAMS."
    (let* ((maxlevel (or (plist-get params :maxlevel) 3))
           (timestamp (plist-get params :timestamp))
           (ts (plist-get params :tstart))
           (te (plist-get params :tend))
           (ws (plist-get params :wstart))
           (ms (plist-get params :mstart))
           (block (plist-get params :block))
           (link (plist-get params :link))
           (tags (plist-get params :tags))
           (properties (plist-get params :properties))
           (inherit-property-p (plist-get params :inherit-props))
           todo-only
           (matcher (if tags (cdr (org-make-tags-matcher tags))))
           cc range-text st p time org-clock-notes org-heading-content-only level hdl props tsp tbl)

      (setq org-clock-file-total-minutes nil)
      (when block
        (setq cc (org-clock-special-range block nil t ws ms)
              ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
      (when (integerp ts) (setq ts (calendar-gregorian-from-absolute ts)))
      (when (integerp te) (setq te (calendar-gregorian-from-absolute te)))
      (when (and ts (listp ts))
        (setq ts (format "%4d-%02d-%02d" (nth 2 ts) (car ts) (nth 1 ts))))
      (when (and te (listp te))
        (setq te (format "%4d-%02d-%02d" (nth 2 te) (car te) (nth 1 te))))
      ;; Now the times are strings we can parse.
      (if ts (setq ts (org-matcher-time ts)))
      (if te (setq te (org-matcher-time te)))
      (save-excursion
        (org-clock-sum-with-notes ts te
                       (unless (null matcher)
                         (lambda ()
                           (let* ((tags-list (org-get-tags-at))
                                  (org-scanner-tags tags-list)
                                  (org-trust-scanner-tags t))
                             (eval matcher)))))
        (goto-char (point-min))
        (setq st t)
        (while (or (and (bobp) (prog1 st (setq st nil))
                        (get-text-property (point) :org-clock-minutes)
                        (setq p (point-min)))
                   (setq p (next-single-property-change
                            (point) :org-clock-minutes)))
          (goto-char p)
          (when (setq time (get-text-property p :org-clock-minutes))
            (setq
             org-clock-notes (get-text-property p :org-clock-notes)
             org-heading-content-only (org-heading-content-only-x))
            (save-excursion
              (beginning-of-line 1)
              (when (and (looking-at (org-re "\\(\\*+\\)[ \t]+\\(.*?\\)\\([ \t]+:[[:alnum:]_@#%:]+:\\)?[ \t]*$"))
                         (setq level (org-reduced-level
                                      (- (match-end 1) (match-beginning 1))))
                         (<= level maxlevel))
                (setq hdl (if (not link)
                              (match-string 2)
                              (org-make-link-string
                               (format "file:%s::%s"
                                       (buffer-file-name)
                                       (save-match-data
                                         (match-string 2)))
                               (org-make-org-heading-search-string
                                (replace-regexp-in-string
                                 org-bracket-link-regexp
                                 (lambda (m) (or (match-string 3 m)
                                                 (match-string 1 m)))
                                 (match-string 2)))))
                      tsp (when timestamp
                            (setq props (org-entry-properties (point)))
                            (or (cdr (assoc "SCHEDULED" props))
                                (cdr (assoc "DEADLINE" props))
                                (cdr (assoc "TIMESTAMP" props))
                                (cdr (assoc "TIMESTAMP_IA" props))))
                      props (when properties
                              (remove nil
                                      (mapcar
                                       (lambda (p)
                                         (when (org-entry-get (point) p inherit-property-p)
                                           (cons p (org-entry-get (point) p inherit-property-p))))
                                       properties))))
                (when (> time 0) (push (list level hdl tsp time props org-heading-content-only org-clock-notes) tbl))))))
        (setq tbl (nreverse tbl))
        (list file org-clock-file-total-minutes tbl))))


;;;###autoload
  (defun org-dblock-write:clocktable-alt (params)
    "Write the standard clocktable."
    (setq params (org-combine-plists org-clocktable-defaults params))
    (catch 'exit
      (let* ((scope (plist-get params :scope))
             (block (plist-get params :block))
             (ts (plist-get params :tstart))
             (te (plist-get params :tend))
             (link (plist-get params :link))
             (maxlevel (or (plist-get params :maxlevel) 3))
             (ws (plist-get params :wstart))
             (ms (plist-get params :mstart))
             (step (plist-get params :step))
             (timestamp (plist-get params :timestamp))
             (formatter (or (plist-get params :formatter)
                            org-clock-clocktable-alt-formatter
                            'org-clocktable-alt-write-default))
             cc range-text ipos pos one-file-with-archives
             scope-is-list tbls level)
        ;; Check if we need to do steps
        (when block
          ;; Get the range text for the header
          (setq cc (org-clock-special-range block nil t ws ms)
                ts (car cc) te (nth 1 cc) range-text (nth 2 cc)))
        (when step
          ;; Write many tables, in steps
          (unless (or block (and ts te))
            (error "Clocktable `:step' can only be used with `:block' or `:tstart,:end'"))
          (org-clocktable-steps params)
          (throw 'exit nil))

        (setq ipos (point)) ; remember the insertion position

        ;; Get the right scope
        (setq pos (point))
        (cond
          ((and scope (listp scope) (symbolp (car scope)))
           (setq scope (eval scope)))
          ((eq scope 'agenda)
           (setq scope (org-agenda-files t)))
          ((eq scope 'agenda-with-archives)
           (setq scope (org-agenda-files t))
           (setq scope (org-add-archive-files scope)))
          ((eq scope 'file-with-archives)
           (setq scope (and buffer-file-name
                            (org-add-archive-files (list buffer-file-name)))
                 one-file-with-archives t)))
        (setq scope-is-list (and scope (listp scope)))
        (if scope-is-list
            ;; we collect from several files
            (let* ((files scope)
                   file)
              (org-agenda-prepare-buffers files)
              (while (setq file (pop files))
                (with-current-buffer (find-buffer-visiting file)
                  (save-excursion
                    (save-restriction
                      (push (org-clock-get-table-data file params) tbls))))))
            ;; Just from the current file
            (save-restriction
              ;; get the right range into the restriction
              (org-agenda-prepare-buffers (list (or (buffer-file-name)
                                                    (current-buffer))))
              (cond
                ((not scope))  ; use the restriction as it is now
                ((eq scope 'file) (widen))
                ((eq scope 'subtree) (org-narrow-to-subtree))
                ((eq scope 'tree)
                 (while (org-up-heading-safe))
                 (org-narrow-to-subtree))
                ((and (symbolp scope) (string-match "^tree\\([0-9]+\\)$"
                                                    (symbol-name scope)))
                 (setq level (string-to-number (match-string 1 (symbol-name scope))))
                 (catch 'exit
                   (while (org-up-heading-safe)
                     (looking-at org-outline-regexp)
                     (if (<= (org-reduced-level (funcall outline-level)) level)
                         (throw 'exit nil))))
                 (org-narrow-to-subtree)))
              ;; do the table, with no file name.
              (push (org-clock-get-table-data nil params) tbls)))

        ;; OK, at this point we tbls as a list of tables, one per file
        (setq tbls (nreverse tbls))

        (setq params (plist-put params :multifile scope-is-list))
        (setq params (plist-put params :one-file-with-archives
                                one-file-with-archives))

        (funcall formatter ipos tbls params)))))


(deh-section "time sheet"
  (defcustom org-time-sheet-date-formatter
    (lambda (day month year) (format "%4d-%02d-%02d" year month day))
    "Function to format date in time sheets.
It takes three numbers as arguments: day month year."
    :type 'function
    :group 'org-clock)

  (defcustom org-time-sheet-time-formatter
    (lambda (start end hour minutes headings)
      (list (format-time-string "%F %R" (apply 'encode-time minutes-start))
            (format-time-string "%F %R" (apply 'encode-time minutes-end))
            (format "%2d00--%2d00" hour (1+ hour)) (or (nth 1 headings) "") (or (nth 2 headings) "") minutes))
    "Callback function returning one table line in a time sheet (as list).
The arguments of the function are:
START:    start time with format as in `decode-time'
END:     end time with format as in `decode-time'
MINUTES:  number of minutes between start time and end time
HEADINGS: the heading titles of the current entry and all its parents as a list starting with the top-parent."
    :type 'function
    :group 'org-clock)

  (eval-when-compile
    (require 'cl-lib))
  (require 'org-element)
  (require 'ob-core)

  (defun org-element-parent (element &optional type)
    "Get parent of ELEMENT or nil if there is none.
If TYPE is non-nil get next parent of that type."
    (let* ((props (cadr element))
           (parent (plist-get props :parent)))
      (if type
          (when parent
            (if (eq (car parent) type)
                parent
                (org-element-parent parent type)))
          parent)))

  (defun org-element-timestamp-less-p (ts1 ts2 &optional end)
    "Non-nil if timestamp TS1 is less than timestamp TS2.
TS1 and TS2 is timestamp data as returned by `org-element-timestamp-parser'.
If end is non-nil the end-time of TS1 and TS2 is compared else the start time."
    (cl-assert (eq (car ts1) 'timestamp) "TS1 is not a timestamp")
    (cl-assert (eq (car ts2) 'timestamp) "TS2 is not a timestamp")
    (let ((p1 (cadr ts1))
          (p2 (cadr ts2))
          (tests '("year" "month" "day" "hour" "minute"))
          ret)
      (while (and (let* ((what (intern-soft (concat ":" (car tests) (if end "-end" "-start"))))
                         (t1 (plist-get p1 what))
                         (t2 (plist-get p2 what)))
                    (cond
                      ((< t1 t2)
                       (setq ret t)
                       nil)
                      ((= t1 t2) t)))
                  (setq tests (cdr tests))))
      ret))

  (defun time-day-month-year (time)
    "Return the list (day month year) from TIME.
TIME may be the time as returned by `current-time' or by `decode-time'."
    (if (<= (length time) 4)
        (setq time (decode-time time)))
    (mapcar (lambda (el) (nth el time)) '(3 4 5)))

  (defun org-element-timestamp-to-time (timestamp &optional start/end encode)
    "Convert start or end of TIMESTAMP returned by `org-element-timestamp-parser'
to time format as defined in the documentation of `decode-time'.
START/END is either the symbol 'start or 'end or nil which is equivalent to 'start.
If ENCODE is non-nil the return value is encoded as described in the documentation for `current-time'."
    (cl-assert (eq (car timestamp) 'timestamp) "Argument is not a timestamp")
    (unless start/end (setq start/end 'start))
    (let* ((p (cadr timestamp))
           (ret (append
                 '(0)
                 (mapcar (lambda (what) (plist-get p (intern-soft (concat ":" what "-" (symbol-name start/end))))) '("minute" "hour" "day" "month" "year"))
                 (list 0 nil (car (current-time-zone))))))
      (if encode
          (apply #'encode-time ret)
          ret)))

  (defmacro decoded-time-complete-timezone (t1 t2)
    "If only one of the time specifications T1 and T2 has time-zone information
append that to the other one."
    `(let ((n1 (length ,t1))
           (n2 (length ,t2)))
       (cond
         ((> n1 n2)
          (setq ,t2 (copy-sequence ,t2))
          (setf (nthcdr n2 ,t2) (nthcdr n2 ,t1)))
         ((< n1 n2)
          (setq ,t1 (copy-sequence ,t1))
          (setf (nthcdr n1 ,t1) (nthcdr n1 ,t2))))))

  (defun decoded-time-less-p (t1 t2)
    "Like `time-less-p' but for decoded time values as `decode-time' returns."
    (decoded-time-complete-timezone t1 t2)
    (time-less-p (apply 'encode-time t1) (apply 'encode-time t2)))

  (defun decoded-time-advance (time dt)
    "Return TIME advanced by DT but for decoded time values as `decode-time' returns.
The time zone information of time is used for the result."
    (decode-time (apply 'encode-time (append (cl-mapcar #'+ (butlast time (- (length time) 6)) (butlast dt (- (length dt) 6))) (nthcdr 6 time)))))

  (defun org-time-sheet (&optional tStart tEnd &optional dont-sum)
    "Create time sheet for time span from tStart to tEnd from current org buffer.
When called non-interactively each of the parameters tStart and tEnd may be nil
or must be decoded time (see `decode-time').
Do not sum up minutest of a project within an hour if dont-sum is non-nil.
Interactively do not sum if called with prefix arg."
    (interactive (list
                  (decode-time (org-read-date t t nil "Start time:" '(0 0)))
                  (decode-time (org-read-date t t nil "End time:"))
                  current-prefix-arg))
    (org-time-sheet-shedule (org-time-sheet-collect tStart tEnd) (called-interactively-p 'any) dont-sum))

  (defun org-time-sheet-collect (tStart tEnd)
    "Returns ordered time sheet collection of current buffer
for clocked items with start time within the range from tStart to tEnd."
    (if (> (length tStart) 4)
        (setq tStart (apply 'encode-time tStart)))
    (if (> (length tEnd) 4)
        (setq tEnd (apply 'encode-time tEnd)))
    (let ((tree (org-element-parse-buffer)))
      (cl-stable-sort
       (org-element-map tree 'clock
         (lambda (clock)
           ;; get the relevant data of the clocks
           (let* ((timestamp (plist-get (cadr clock) :value))
                  (parent clock)
                  (headers (nreverse (cl-loop while (setq parent (org-element-parent parent 'headline)) collect (car (plist-get (cadr parent) :title))))))
             (cl-assert timestamp nil "Clock line without timestamp")
             (when (and (or (null tStart) (null (time-less-p (org-element-timestamp-to-time timestamp 'start t) tStart)))
                        (or (null tEnd) (time-less-p (org-element-timestamp-to-time timestamp 'end t) tEnd)))
               (list (org-element-timestamp-to-time timestamp 'start)
                     (org-element-timestamp-to-time timestamp 'end)
                     headers))
             )))
       #'time-less-p
       :key (lambda (clock) (apply 'encode-time (car clock))))))

  (defun org-time-sheet-shedule (clocks &optional interactive dont-sum)
    "Creates time sheet shedule from ordered time sheet clock collection (see `org-time-sheet-collect')."
    ;; sheduling
    (when clocks
      (setq clocks (cons nil clocks))
      (let* ((start (copy-sequence (caadr clocks)))
             (day-month-year (time-day-month-year start))
             (shedule (list (list (apply org-time-sheet-date-formatter day-month-year)))))
        (setf (nth 1 start) 0) ;; clear minutes
        (while (cdr clocks)
          (let ((end (decoded-time-advance start '(0 0 1 0 0 0)))
                project-alist
                (iter clocks))
            (while (decoded-time-less-p (cl-caadr iter) end) ;; collect clocks starting before the end of current hour
              (let* ((start-time (cl-caadr iter))
                     (end-time (cl-cadadr iter))
                     (minutes-start (if (decoded-time-less-p start-time start) start start-time))
                     (minutes-end (if (decoded-time-less-p end end-time) end end-time))
                     (minutes (/ (nth 1 (time-subtract (apply 'encode-time minutes-end) (apply 'encode-time minutes-start))) 60))
                     (headlines (nth 2 (cadr iter)))
                     (project (assoc headlines project-alist)))
                (if (and project (null dont-sum))
                    (setcdr project (list (+ (cadr project) minutes) minutes-start minutes-end))
                    (setq project-alist (cons (list headlines minutes minutes-start minutes-end) project-alist)))
                (if (decoded-time-less-p end end-time)
                    (setq iter (cdr iter))
                    ;; delete clock that also finishes in this hour:
                    (setcdr iter (cddr iter))) ;; delete clock entry
                ))
            (setq project-alist (nreverse project-alist))
            ;; Compose shedule for hour:
            (while project-alist
              (let ((headlines (caar project-alist))
                    (minutes (nth 1 (car project-alist)))
                    (minutes-start (nth 2 (car project-alist)))
                    (minutes-end (nth 3 (car project-alist))))
                (setq shedule (cons (funcall org-time-sheet-time-formatter minutes-start minutes-end (nth 2 start) minutes headlines) shedule)))
              (setq project-alist (cdr project-alist)))
            ;; calculate new time:
            (when (cdr clocks)
              (let ((next-hour-start-time (decoded-time-advance start '(0 0 1 0 0 0)))
                    (next-hour-end-time (decoded-time-advance start '(0 0 2 0 0 0))))
                (setq start (copy-sequence (caadr clocks)))
                (setf (nth 1 start) 0) ;; minutes
                (when (decoded-time-less-p start next-hour-end-time)
                  (setq start next-hour-start-time))
                (let ((new-day-month-year (time-day-month-year start)))
                  (unless (equal day-month-year new-day-month-year)
                    (setq shedule (cons (list (apply org-time-sheet-date-formatter new-day-month-year)) shedule)
                          day-month-year new-day-month-year)))))))
        (setq shedule (nreverse shedule))
        (when interactive
          (insert (with-temp-buffer
                    (insert "#+begin_src emacs-lisp\n#+end_src\n")
                    (let ((pt (point)))
                      (org-babel-insert-result shedule)
                      (delete-region (point-min) pt))
                    (buffer-string))))
        shedule))))





(provide 'orgclocktask-config)
;;; orgclocktask-config.el ends here
