;;; xlsx-mode -- Spreadsheet Mode -- Tabular interface to Calc
;; Copyright (C) 2014 -- Use at 'yer own risk  -- NO WARRANTY!
;; Author: sam fiechter sam.fiechter(at)gmail
;; Version: 0.000000000000001
;; Created: 2014-03-24
;; Keywords: calc, spreadsheet


;;;Code
(require 'avl-tree)
;; ;;;;;;;;;;;;; variables ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; pseudo constants
(defvar xlsx-range-parts-re "\\([A-Za-z]+\\)\\([0-9]+\\)\\:\\([A-Za-z]+\\)\\([0-9]+\\)")
(defvar xlsx-one-cell-re "$?[A-Za-z]+$?[0-9]+")
(defvar xlsx-range-re "$?[A-Za-z]+$?[0-9]+\\:$?[A-Za-z]+$?[0-9]+")
(defvar xlsx-cell-or-range-re (concat "[^A-Za-z0-9]?\\(" xlsx-range-re  "\\|" xlsx-one-cell-re  "\\)[^A-Za-z0-9\(]?"))
(defvar xlsx-empty-name "*Sams Spreadsheet Mode*")

;;       CELL Format -- [ "A1" 0.5 "= 1/2" "%0.2g" "= 3 /2" (list of cells to calc when changes)]

;;   0 = index / cell Name
(defvar xlsx-c-addr 0)
;;   1 = value (formatted)
(defvar xlsx-c-fmtd 1)
;;   2 = value (number)
(defvar xlsx-c-val 2)
;;   3 = format (TBD)
(defvar xlsx-c-fmt 3)
;;   4 = formula
(defvar xlsx-c-fmla 4)
;;   5 = depends on -- list of indexes
(defvar xlsx-c-deps 5)

(defvar xlsx-lcmask (lognot (logxor (string-to-char "A") (string-to-char "a"))))

;; status vars
(defvar xlsx-cur-col 0)
(defvar xlsx-max-col 3)
(defvar xlsx-col-widths (make-vector xlsx-max-col 7))
(defvar xlsx-mark-cell nil)
(defvar xlsx-cur-row 1)
(defvar xlsx-max-row 3)
(defvar xlsx-row-padding 4)
(defvar xlsx-sheets (list )) ;; list of xlsx-data
(defvar xlsx-data (avl-tree-create 'xlsx-avl-cmp))
(defvar xlsx-default-number-fmt ",0.00")
(defvar xlsx-cursor nil)

;;other
(defvar xlsx-input-history (list ))
(defvar xlsx-format-history (list ))
;; ;;;;;;;;;;;;; keymaps ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar xlsx-map  (make-sparse-keymap 'xlsx-map))

(define-key xlsx-map [left]       'xlsx-move-left)
(define-key xlsx-map [right]      'xlsx-move-right)
(define-key xlsx-map [tab]        'xlsx-move-right)
(define-key xlsx-map [up]         'xlsx-move-up)
(define-key xlsx-map [down]       'xlsx-move-down)
(define-key xlsx-map [return]     'xlsx-edit-cell)
(define-key xlsx-map [backspace]  'xlsx-clear-key)
(define-key xlsx-map [remap set-mark-command] 'xlsx-set-mark)
(define-key xlsx-map [remap self-insert-command] 'xlsx-edit-cell)
(define-key xlsx-map (kbd "C-x f") 'xlsx-edit-format)

;;(define-key xlsx-map [C-R]        'xlsx-search-buffer)

(define-key xlsx-map (kbd "+")  'xlsx-increase-cur-col )
(define-key xlsx-map (kbd "-")  'xlsx-decrease-cur-col )



;;  ____        _           ___       _     _
;; |  _ \  __ _| |_ __ _   / / \   __| | __| |_ __
;; | | | |/ _` | __/ _` | / / _ \ / _` |/ _` | '__|
;; | |_| | (_| | || (_| |/ / ___ \ (_| | (_| | |
;; |____/ \__,_|\__\__,_/_/_/   \_\__,_|\__,_|_|    functions
;;

(defun xlsx-new-cell (addr) "Blank cell"
       (vector addr "" "" xlsx-default-number-fmt "" (list) )
       )

(defun xlsx-transform-fmla (from to fun)
  "transform a function from from to to moving all addresses relative to the addresses
EX:  From: A1 To: B1 Fun: = A2 / B1
     Returns: = B2 / C1

"
  (let* ((f (xlsx-addr-to-index from))
         (t (xlsx-addr-to-index to))
         (s (concat " fun "))
         (refs (xlsx-formula-cell-refs s)))
    (dolist (ref refs)
      (setq cr (xlsx-index-to-addr (+ (- (xlsx-addr-to-index (elt ref 0)) f) t)))
      (setq s (concat (substring s 0 (elt ref  1)) cr (substring s (elt ref 2))))
      )
    (substring s 1 -1)
    ))

(defun xlsx-clear-key () "delete current cell" (interactive)
       (let  (  (current-cell (concat (xlsx-col-letter xlsx-cur-col) (int-to-string xlsx-cur-row))))
         (if  (avl-tree-delete xlsx-data current-cell)
             (xlsx-draw-cell xlsx-cur-col xlsx-cur-row (xlsx-pad-right "" xlsx-cur-col))
           nil) ))

(defun xlsx-avl-cmp (a b)
  "This is the function used by avl tree to compare ss addresses"
  (let ((A (if (sequencep a) (elt a xlsx-c-addr) a)) (B (if (sequencep b) (elt b xlsx-c-addr) b)))  ; a or b can be vectors or addresses
    (< (xlsx-addr-to-index A) (xlsx-addr-to-index B)) ))


(defun xlsx-addr-to-index (a)  "Convert from ss addr (e.g. A1) to index  -- expects [A-Z]+[0-9]+"
       (let ((chra (- (string-to-char "A") 1))

             )
         (if (sequencep a)
             (progn
               (let ((out 0) (i 0))
                 (while (and  (< i (length a)) (< chra (elt a i)))
                   (setq out (+ (* 26 out)  (- (logand xlsx-lcmask (elt a i)) chra))) ;; -33 is mask to change case
                   (setq i (+ 1 i)))
                 (setq out (+ (* out xlsx-max-row) (string-to-int (substring a i))))
                 out))
           a )))

(defun xlsx-index-to-addr (idx)
  "Convert form ss index to addr (eg A1) -- expects integer"
  (if (integer-or-marker-p idx)
      (let* ((row (% idx xlsx-max-row))
             (col (- (/ (- idx row) xlsx-max-row) 1)))
        (concat (xlsx-col-letter col) (int-to-string row))
        ) "A1") )

(defun xlsx-col-number (column)
  "returns the index of a column id -- expects letters"
  (let ((chra (- (string-to-char "A") 1))
        (out 0) (i 0)
        (a column))
    (dotimes (j (length a))
      (aset a j (logand xlsx-lcmask (elt a j))) )
    (while (and  (< i (length a)) (< chra (elt a i)))
      (setq out (+ (* 26 out)  (- (elt a i) chra))) ;; -33 is mask to change case
      (setq i (+ 1 i)) )
    out
    ))

(defun xlsx-col-letter (a)
  "returns the letter of the column id arg -- expects int"
  (let ((out "") (n 1) (chra (string-to-char "A")))
    (while (<= 0 a)
      (setq out (concat (char-to-string (+ chra (% a 26))) out))
      (setq a  (- (/ a 26) 1)) )
    out ))

(defun xlsx-edit-format ( )
  "edit the format of the selected cell" (interactive)
  (let*  (  (current-cell (concat (xlsx-col-letter xlsx-cur-col) (int-to-string xlsx-cur-row)))
            (m (avl-tree-member xlsx-data (xlsx-addr-to-index current-cell)))
            (of (if m (elt m xlsx-c-fmt) xlsx-default-number-fmt))
            (prompt (concat "Cell " current-cell ": "))
            (nt (read-string prompt of  xlsx-yformat-history )))
    (if m
        (progn
          (aset m xlsx-c-fmla nt)
          (aset m xlsx-c-fmtd (xlsx-format-number nt (elt m xlsx-c-val)))
          (xlsx-draw-cell xlsx-cur-col xlsx-cur-row (elt m xlsx-c-fmtd)))
      (progn
        (setq m (xlsx-new-cell current-cell))
        (aset m xlsx-c-fmla nt)))))

(defun xlsx-set-mark () "set the mark to the current cell" (interactive)
       (if xlsx-mark-cell
           (setq xlsx-mark-cell nil)
         (setq xlsx-mark-cell (list xlsx-cur-col xlsx-cur-row)))
       (xlsx-move-cur-cell 0 0) )

(defun xlsx-edit-cell ( )
  "edit the selected cell"
  (interactive)

  (let*  (  (current-cell (concat (xlsx-col-letter xlsx-cur-col) (int-to-string xlsx-cur-row)))
            (prompt (concat "Cell " current-cell ": "))
            (m (avl-tree-member xlsx-data (xlsx-addr-to-index current-cell)))
            (ot (if m (if (string= "" (elt m xlsx-c-fmla)) (elt m xlsx-c-val) (elt m xlsx-c-fmla)) "" ))
            (nt 1) )
    (setq nt (if (equal 'return last-input-event)
                 (read-string prompt ot  xlsx-input-history )
               (read-string prompt (char-to-string last-input-event)  xlsx-input-history )))

    (xlsx-update-cell current-cell nt)
    (xlsx-move-down)
    ))


(defun xlsx-update-cell (current-cell nt)
  "Update the value/formula  of current cell to nt"
  (let  ( (m (avl-tree-member xlsx-data (xlsx-addr-to-index current-cell)))
          )

    ;;delete this cell from its old deps
    (if m (let ((refs (xlsx-formula-cell-refs  (aref m xlsx-c-fmla))))
            (dolist (ref refs)
              (xlsx-del-dep (elt ref 0) current-cell))
            ) nil )
    ;; if this is a formula, eval and do deps
    (if (and (< 0 (length nt)) (= (string-to-char "=") (elt nt 0))) ; formulas start with  =
        (progn
          (if m
              (aset m xlsx-c-fmla nt)
            (progn
              (setq m (xlsx-new-cell current-cell))
              (aset m xlsx-c-fmla nt)
              (avl-tree-enter xlsx-data m)
              ))

          (let ((s (concat nt " ")) (deps (list)))
            (setq s (replace-regexp-in-string xlsx-cell-or-range-re (lambda (str)
                                                                      (save-match-data
                                                                        (let ((addr (match-string 1 str)))
                                                                          (push addr deps)
                                                                          (xlsx-cell-val addr)))) s nil nil 1))
            (dolist (dep deps)
              (xlsx-add-dep dep current-cell))

            (setq nt (calc-eval (substring s 1 -1 ) ))  ;; throw away = and  ' '
            ))
      (if m (aset m xlsx-c-fmla "") nil))
    ;; nt is now not a formula
    (if m
        (progn
          (aset m xlsx-c-val nt)
          (aset m xlsx-c-fmtd (xlsx-format-number (elt m xlsx-c-fmt) nt)))
      (progn  ;;else
        (setq m (xlsx-new-cell current-cell))
        (aset m xlsx-c-val nt)
        (aset m xlsx-c-fmtd (xlsx-format-number xlsx-default-number-fmt nt))
        (avl-tree-enter xlsx-data m)
        ))
    ;;do eval-chain
    (dolist (a (aref m xlsx-c-deps))
      (if (equal a current-cell)
          nil ;; don't loop infinite
        (xlsx-eval-chain a current-cell)))

    ;;draw it
    (xlsx-draw-cell xlsx-cur-col xlsx-cur-row (xlsx-pad-right (elt m xlsx-c-fmtd) xlsx-cur-col))
    ))


(defun xlsx-xml-query (node child-node)
  "search an xml doc to find nodes with tags that match child-node"
  (let ((match (list (if (string= (car node) child-node) node nil)))
        (children (cddr node)))
    (if (listp children)
        (dolist (child children)
          (if (listp child)
              (setq match (append match (xlsx-xml-query child child-node))) nil)
          ) nil )
    (delq nil match)))

(defun xlsx-load (filename)
  "Load a file into the Spreadsheet."
  (interactive "FFilename:")
  (let ((patterns (list (cons "\\.\\(xlsx\\|XLSX\\)$" 'xlsx-load-xlsx)
                        (cons "\\.\\(csv\\|CSV\\)$" 'xlsx-load-csv)
                        )))
    (dolist (e patterns)
      (if (string-match-p (car e) filename)
          (eval (list (cdr e) filename))
        nil) )))

(defun xlsx-to-int (w)
  (truncate (if (numberp w) w (string-to-number w))))

(defun xlsx-load-xlsx (filename )
  "Try and read an XLSX file into xlsx-mode"
  (let ((xml nil)
        (sheets (list)) ;; filenames for sheets
        (styles (list)) ;; fn for styles
        (strings (list)) ;; fn for strings
        (sheettype "application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml")
        (styletype "application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml")
        (stringstype "application/vnd.openxmlformats-officedocument.spreadsheetml.sharedStrings+xml")
        (shstrs (list) ))

    (with-temp-buffer  ;; read in the workbook file to get name/num sheets
      (erase-buffer)
      (shell-command (concat "unzip -p " filename " \"\\[Content_Types\\].xml\"") (current-buffer))
      (setq xml (xml-parse-region (buffer-end -1) (buffer-end 1)))
      (dolist (sh (xlsx-xml-query (car xml) "Override"))
        (let ((type (cdr (assoc 'ContentType (elt sh 1))))
              (name (cdr (assoc 'PartName (elt sh 1)))))
          (if (string= type sheettype) (push name sheets))
          (if (string= type styletype) (push name styles))
          (if (string= type stringstype) (push name strings))
          )))
    (setq sheets  (nreverse sheets))
    (setq styles (nreverse styles))
    (setq strings (nreverse strings))
    (dolist (sfn sheets)  ;; sfn is the sheet file name
      (dolist (stringfn strings) ;; stringfn is the string file name
        (with-temp-buffer
          (erase-buffer)
          (message "Decompressing Strings %s..." (concat "unzip -p " filename " " (substring stringfn 1)));; strings
          (shell-command (concat "unzip -p " filename " " (substring stringfn 1)) (current-buffer))
          (setq xml (xml-parse-region (buffer-end -1) (buffer-end 1)))
          (let ((ts (xlsx-xml-query (car xml) "t")) )
            (dolist (cell ts)
              (push (elt cell 2) shstrs)
              ))))
      (setq shstrs (vconcat  (nreverse shstrs)))

      (with-temp-buffer

        (message "Decompressing Sheet %s..." (concat "unzip -p " filename " " (substring sfn 1)) ) ;; sheet layout
        (erase-buffer)
        (shell-command (concat "unzip -p " filename " " (substring sfn 1)) (current-buffer))
        (setq xml (xml-parse-region (buffer-end -1) (buffer-end 1)))
        (let ((sheet (avl-tree-create 'xlsx-avl-cmp))
              (cols (xlsx-xml-query (car xml) "col")))
          (setq xlsx-data sheet)
          (dolist (col cols)
            (let ((max (string-to-int (cdr (assoc 'max (elt col 1)))))
                  (min (string-to-int (cdr (assoc 'min (elt col 1)))))
                  (width (cdr (assoc 'width (elt col 1))))
                  (len (length xlsx-col-widths)) )
              (if (< len  max)
                  (setq xlsx-col-widths (vconcat xlsx-col-widths (make-vector (- max len) (xlsx-to-int width)))) nil )
              (dotimes (i (- max min))
                (aset xlsx-col-widths (+ min i) (xlsx-to-int width)))
              ))
          (dolist (cell (xlsx-xml-query (car xml) "c"))
            (let* ((range (prin1-to-string (cdr (assoc 'r (elt cell 1))) t))
                   (value (prin1-to-string (car (cddr (assoc 'v cell))) t))
                   (fmla (prin1-to-string  (car (cddr (assoc 'f cell))) t)))
              (if (string= value "nil") (setq value ""))
              (if (string= "s" (cdr (assoc 't (elt cell 1)))) (setq value (elt shstrs (string-to-int value))))
              (if (string= fmla "nil") (setq fmla ""))
              (if (string-match "\\([A-Za-z]+\\)\\([0-9]+\\)" range)
                  (let ((row (string-to-int (match-string 2 range)))
                        (col (xlsx-col-number (match-string 1 range))))
                    (setq col (if col col 0))
                    (if (<= xlsx-max-row row) (setq xlsx-max-row (+ 5 row)))
                    (if (<= xlsx-max-col col)
                        (progn
                          (setq xlsx-col-widths (vconcat xlsx-col-widths (make-vector  (- col xlsx-max-col ) 7)))
                          (setq xlsx-max-col (length xlsx-col-widths))
                          ) nil )
                    ) nil)
              (if (string= "" fmla)
                  (xlsx-update-cell range value)
                (xlsx-update-cell range (concat "=" fmla)))
              )))
        (push  xlsx-data xlsx-sheets) ))
    (let ((xd (car xlsx-sheets)))
      (setq xlsx-data (if xd xd (avl-tree-create 'xlsx-avl-cmp)))
      (xlsx-draw-all) )
    ))

(defun xlsx-load-csv (filename)
  "Read a CSV file into xlsx-mode"
  (interactive "fFilename:")
  (let ((mybuff (current-buffer))
        (cc xlsx-cur-col)
        (cr xlsx-cur-row))

    (with-temp-buffer
      (insert-file-contents filename)
      (beginning-of-buffer)
      (let ((x 0) (cell "") (cl xlsx-cur-col)
            (rw xlsx-cur-row) (j 0)
            (re ",?\\(\"\\(\\(\"\"\\|[|^\"]\\)+\\)\"\\|\\([^,]+\\)\\),?")
            (newline ""))
        (while (not (eobp))
          (setq newline (thing-at-point 'line))
          (setq j 0)
          (while (and (< j (length newline)) (string-match re newline j))
            (setq x (if (match-string 2 newline) 2 1))
            (setq cell (match-string x newline))
            (setq j (+ 1 (match-end x)))
            (with-current-buffer mybuff
              (xlsx-update-cell (concat (xlsx-col-letter cl) (int-to-string rw)) cell)
              )
            (setq cl (+ cl 1))
            (if (<= xlsx-max-col cl)
                (progn
                  (setq xlsx-max-col (+ cl 1))
                  (setq xlsx-col-widths (vconcat xlsx-col-widths (list (elt xlsx-col-widths (- cl 1)))))
                  ) nil ))
          (setq rw (+ rw 1))
          (if (<=  xlsx-max-row  rw)
              (setq xlsx-max-row (+ rw 1)) nil)
          (setq cl xlsx-cur-col)
          (forward-line 1)
          )))
    (setq xlsx-cur-col cc)
    (setq xlsx-cur-rw cr)
    (xlsx-draw-all)
    ))


;;  ____                     _
;; |  _ \ _ __ __ ___      _(_)_ __   __ _
;; | | | | '__/ _` \ \ /\ / / | '_ \ / _` |
;; | |_| | | | (_| |\ V  V /| | | | | (_| |
;; |____/|_|  \__,_| \_/\_/ |_|_| |_|\__, |
;;                                   |___/
;; functions dealing with the cursor and cell drawing /padding



(defun xlsx-format-number (fmt val)
  "output text format of numerical value according to format string
   000000.00  -- pad to six digits (or however many zeros to left of .) round at two digits, or pad out to two
   #,###.0 -- insert a comma (anywhere to the left of the . is fine -- you only need one and it'll comma ever three
   0.00## -- Round to four digits, and pad out to at least two."
  (if (stringp val) nil (setq val (format "%s" val)))
  (if (equal 0 (string-match "^ *\\+?-?[0-9,\\.]+ *$" val))
      (progn
        (let ((ip 0)   ;;int padding
              (dp 0)   ;;decmel padding
              (dr 0)   ;;decmil round
              (dot-index (string-match "\\." fmt))
              (power 0)
              (value (if (numberp val) val (string-to-number val)))
              (dec ""))
          (dotimes (i (or dot-index (length fmt)))
            (and (= (elt fmt i) (string-to-char "0")) (setq ip (1+ ip)))
            )
          (if dot-index
              (progn (dotimes (i (- (length fmt) dot-index))
                       (and (= (elt fmt (+ i dot-index)) (string-to-char "0")) (setq dp (1+ dp)))
                       (and (= (elt fmt (+ i dot-index)) (string-to-char "#")) (setq dr (1+ dr)))
                       )
                     (setq power (+ dp dr))
                     (setq dec  (fround (* (- (+ 1 value) (ftruncate value)) (expt 10 power))))  ;; 0.0 = 1.00
                     (setq power (- (length (format "%d" dec)) dp 2))  ; -2 = "1."
                     (if (> power 0)
                         (setq dec (* dec (expt 10 power)))
                       nil
                       )
                     (setq dec (concat "." (substring (format "%d" dec) 1)))
                     ) nil )
          (setq dec (concat (format (concat "%0." (number-to-string ip) "d") (ftruncate value)) (if dot-index dec nil)))
          (if (string-match "," fmt)
              (let ((re "\\([0-9]\\)\\([0-9]\\{3\\}\\)\\([,\\.]\\)\\|\\([0-9]\\)\\([0-9][0-9][0-9]\\)$")
                    (rep (lambda (a) (save-match-data (concat (match-string 1 a) (match-string 4 a) "," (match-string 5 a) (match-string 2 a) (match-string 3 a))))))
                (while (string-match re dec)
                  (setq dec (replace-regexp-in-string re rep dec)))
                ) nil )
          dec))
    val))

(defun xlsx-highlight (txt)
  "highlight text"
  (let ((myface '((:foreground "White") (:background "Blue"))))
    (propertize txt 'font-lock-face myface)))

(defun xlsx-pad-center (s i)
  "pad a string out to center it - expects stirng, col no (int)"
  (setq s (replace-regexp-in-string "\n" " " s))
  (let ( (ll (length s) ))
    (if (>= ll (- (elt xlsx-col-widths i) 2))
        (make-string (elt xlsx-col-widths i) (string-to-char "#"))
      (let* ( (pl (/ (- (elt xlsx-col-widths i) ll)  2))  ; half the pad length
              (pad (make-string (- (elt xlsx-col-widths i) pl 1 ) (string-to-char " "))) )
        (concat (make-string pl (string-to-char " ")) s pad)) )
    ))

(defun xlsx-pad-left (s i)
  "pad to the left  - expects stirng, col no (int)"
  (setq s (replace-regexp-in-string "\n" " " s))
  (let ( (ll (length s)) )
    (if (>= ll (- (elt xlsx-col-widths i) 2))
        (make-string (elt xlsx-col-widths i) (string-to-char "#"))
      (concat s (make-string (- (elt xlsx-col-widths i)  ll) (string-to-char " ")))
      )))

(defun xlsx-pad-right (s i)
  "pad to the right  - expects stirng, col no (int)"
  (setq s (replace-regexp-in-string "\n" " " s))
  (let ( (ll (length s)) )
    (if (>= ll (- (elt xlsx-col-widths i) 2))
        (make-string (elt xlsx-col-widths i) (string-to-char "#"))
      (concat (make-string (- (elt xlsx-col-widths i)  ll) (string-to-char " ")) s)
      )))


(defun xlsx-draw-all ()
  "Populate the current ss buffer." (interactive)
  ;;  (pop-to-buffer xlsx-empty-name nil)

  (let ((i 0) (j 0) (k 0) (header (make-string xlsx-row-padding (string-to-char " "))))
    (beginning-of-buffer)
    (erase-buffer)
    (setq cursor-type nil)  ;; no cursor
    (setq truncate-lines 1)  ;; no wrap-aroudn
    (dotimes (i xlsx-max-col)  ;; make header
      (setq header (concat header (xlsx-pad-right (xlsx-col-letter i) i))))
    (dotimes (j xlsx-max-row) ;; draw buffer
      (if (= 0 j)
          (insert  (xlsx-highlight header))
        (progn
          (insert (xlsx-highlight (format (concat "%" (int-to-string xlsx-row-padding) "d") j) ))
          (dotimes (i xlsx-max-col)
            (let ((m (avl-tree-member xlsx-data (+ j (* xlsx-max-row (+ i 1))))))
              (if m
                  (insert (xlsx-pad-right (elt m xlsx-c-fmtd) i))
                (insert (make-string (elt xlsx-col-widths i) (string-to-char " "))))))))

      (insert "\n"))
    (set-buffer-modified-p nil)
    (xlsx-move-cur-cell 0 0) ;;draw cursor
    ))


(defun xlsx-draw-cell (x y text)
  "redraw one cell on the ss  - expects  col no. (int), row no, (int), cell value (padded string)"
  (let ((col xlsx-row-padding) (i 0))
    ;;    (pop-to-buffer xlsx-empty-name nil)
    (setq cursor-type nil)  ;; no cursor
    (setq truncate-lines 1)  ;; no wrap-around

    (dotimes (i x) (setq col (+ col (elt xlsx-col-widths i))) )
    (goto-line (+ y 1))
    (move-to-column col)

    (insert text)
    (delete-forward-char (length text))
                                        ;    (recenter)
    ))

(defun xlsx-move-left ()
  (interactive)
  (if (> xlsx-cur-col  0) (xlsx-move-cur-cell -1 0) nil)
  )

(defun xlsx-move-right ()
  (interactive)
  (if (< xlsx-cur-col (- xlsx-max-col 1))
      (xlsx-move-cur-cell 1 0)
    (progn
      (setq xlsx-max-col (+ 1 xlsx-max-col))
      (setq xlsx-col-widths   (vconcat xlsx-col-widths (list (elt xlsx-col-widths xlsx-cur-col))))
      (setq xlsx-cur-col (+ 1 xlsx-cur-col))
      (xlsx-draw-all)
      )))

(defun xlsx-move-up ()
  (interactive)
  (if (> xlsx-cur-row 1)
      (xlsx-move-cur-cell 0 -1)
    nil ))

(defun xlsx-move-down ()
  (interactive)
  (if (< xlsx-cur-row (- xlsx-max-row 1))
      (xlsx-move-cur-cell 0 1 )
    (progn
      (setq xlsx-max-row (+ 1 xlsx-max-row))
      (setq xlsx-cur-row (+ 1 xlsx-cur-row))
      (xlsx-draw-all)
      )))

(defun xlsx-move-cur-cell (x y) (interactive)
       "Move the cursor or selection overaly"
       (goto-char 0)
       (let* ((new-row (+ xlsx-cur-row y))
              (new-col (+ xlsx-cur-col x))
              (n (avl-tree-member xlsx-data (+ new-row (* xlsx-max-row (+ 1 new-col)))))
              (row-pos (line-beginning-position (+ 1 new-row)))
	      (cur-mesg "")
              (col-pos (+ 4 (if (> new-col 0) (apply '+ (mapcar (lambda (x) (elt xlsx-col-widths x)) (number-sequence 0 (- new-col 1)))) 0))) )
         (if (listp xlsx-cursor) (progn
                                   (dolist (ovl xlsx-cursor) (delete-overlay ovl))
                                   (setq xlsx-cursor nil) ))
         (if xlsx-mark-cell
             (progn  ;; mark set...
               (if (overlayp xlsx-cursor) (progn
                                            (delete-overlay xlsx-cursor)
                                            (setq xlsx-cursor nil) ))
               (let* ((mc (elt xlsx-mark-cell 0))
                      (mr (elt xlsx-mark-cell 1))
                      (minc (if (> new-col mc) mc new-col))
                      (maxc (if (> new-col mc) new-col mc))
                      (minr (if (> new-row mr) mr new-row))
                      (maxr (if (> new-row mr) new-row mr))
                      (col-min  (+ 4 (if (> minc 0) (apply '+ (mapcar (lambda (x) (elt xlsx-col-widths x)) (number-sequence 0 (- minc 1) ))) 0)))
                      (col-max  (+ 4 (if (>= maxc 0) (apply '+ (mapcar (lambda (x) (elt xlsx-col-widths x)) (number-sequence 0 maxc ))) 0))) )
                 (dotimes (row (+ 1 (- maxr minr)))
                   (let ((ovl nil) (row-pos (line-beginning-position (+ 1 row minr))))
                     (setq ovl (make-overlay (+ row-pos col-min) (+ row-pos col-max)))
                     (overlay-put ovl 'face '((:foreground "White") (:background "Blue")))
                     (push ovl xlsx-cursor)
                     ))
                 (setq xlsx-cur-row new-row)
                 (setq xlsx-cur-col new-col)
                 (minibuffer-message (concat "Range " (xlsx-col-letter mc) (int-to-string mr)
                                             ":" (xlsx-col-letter xlsx-cur-col) (int-to-string xlsx-cur-row)))
                 ))
           ;; Mark Not set...
           (let ((n (avl-tree-member xlsx-data (+ new-row (* xlsx-max-row (+ 1 new-col))))))
             (if (overlayp xlsx-cursor)
                 (move-overlay xlsx-cursor (+ row-pos col-pos) (+ row-pos col-pos (elt xlsx-col-widths new-col)))
               (progn
                 (setq xlsx-cursor (make-overlay (+ row-pos col-pos) (+ row-pos col-pos (elt xlsx-col-widths  new-col))))
                 ))

             (overlay-put xlsx-cursor 'face '((:foreground "White") (:background "Blue")))
             (setq xlsx-cur-row new-row)
             (setq xlsx-cur-col new-col)
            (setq cur-mesg (concat "Cell " (xlsx-col-letter xlsx-cur-col) (int-to-string xlsx-cur-row)
                                         " : " (if n (if (string= "" (elt n xlsx-c-fmla)) (elt n xlsx-c-val ) (elt n xlsx-c-fmla)) "" )))
             ))
         (goto-char (+ row-pos col-pos))
         (recenter nil)
	 (minibuffer-message cur-mesg)
         ))


(defun xlsx-increase-cur-col ()
  "Increase the width of the current column" (interactive)
  (aset xlsx-col-widths xlsx-cur-col (+ 1 (elt xlsx-col-widths xlsx-cur-col)))
  (xlsx-draw-all))

(defun xlsx-decrease-cur-col ()
  "Decrease the width of the current column" (interactive)
  (aset xlsx-col-widths xlsx-cur-col (- (elt xlsx-col-widths xlsx-cur-col) 1))
  (xlsx-draw-all))

;;   ____     _ _   _____            _             _   _
;;  / ___|___| | | | ____|_   ____ _| |_   _  __ _| |_(_) ___  _ __
;; | |   / _ \ | | |  _| \ \ / / _` | | | | |/ _` | __| |/ _ \| '_ \
;; | |__|  __/ | | | |___ \ V / (_| | | |_| | (_| | |_| | (_) | | | |
;;  \____\___|_|_| |_____| \_/ \__,_|_|\__,_|\__,_|\__|_|\___/|_| |_|
;; fuctions dealing with eval

(defun xlsx-cell-val (address)
  "get the value of a cell"
  (let ((addr (replace-regexp-in-string "\\$" "" address)))
    (if (string-match xlsx-range-parts-re addr)
        (progn
          (let* ((s (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 2 addr))))
                 (cl (- (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 4 addr))) s))
                 (rl (- (xlsx-addr-to-index (concat  (match-string 3 addr) (match-string 2 addr))) s))
                 (rv "[") (m 0)
                 )
            (if (< cl 0)
                (progn
                  (setq cl (* -1 cl))
                  (setq s (- s cl)) ) nil)
            (if (< rl 0)
                (progn
                  (setq rl (* -1 rl))
                  (setq s (- s rl))) nil )
            (setq rl (/ rl xlsx-max-row))
            (dotimes (r (+ 1 rl))
              (setq rv (concat rv "["))
              (dotimes (c (+ 1 cl))
                (setq m (avl-tree-member xlsx-data (+ s c (* xlsx-max-row r))))
                (setq rv (concat rv (if m (elt m xlsx-c-val) "0") (if (= c cl) "]" ",") ))
                )
              (setq rv (concat rv (if (= r rl) "]" ",")))
              ) rv))
      (progn
        (let ((m (avl-tree-member xlsx-data  (xlsx-addr-to-index addr))) )
          (if m (elt m xlsx-c-val) "0")
          )))))


(defun xlsx-eval-chain (addr chain)
  "Update cell and all deps"
  (let ((m (avl-tree-member xlsx-data (xlsx-addr-to-index addr))) )
    (if m
        (progn
          (xlsx-eval-fun addr)
          (dolist (a (elt m xlsx-c-deps))
            (if (member a chain) nil
              (xlsx-eval-chain a (append chain (list addr)))
              ))))))


(defun xlsx-formula-cell-refs (formula)
  "Takes a string and finds all the cell references in a string Ex:
   (xlsx-fomual-cell-refs (\"= A1+B1\"))
   returns a list ((\"A1\" 2 3) (\"B1\" 5 6))"
  (let* ((s (concat formula " "))
         (j 0) (retval (list )))
    (while (string-match xlsx-cell-or-range-re s j)
      (setq retval (append retval (list (list (match-string 1 s) (match-beginning 1) (match-end 1)))))
      (setq j (+ 1(match-end 1)))
      )
    retval))

(defun xlsx-eval-fun (addr)
  "sets cell value based on its function; draws"
  (let*  ( (m (avl-tree-member xlsx-data (xlsx-addr-to-index addr)))
           (cv "")
           (s (if m (elt m xlsx-c-fmla) ""))
           (refs (xlsx-formula-cell-refs s)) )
    (if refs
        (progn
          (dolist (ref refs)
            (setq cv (xlsx-cell-val (elt ref 0)))
            (setq s (concat (substring s 0 (elt ref  1)) cv (substring s (elt ref 2)))))
          (setq cv (calc-eval (substring s 1)))
          (aset m xlsx-c-val cv)
          (aset m xlsx-c-fmtd (xlsx-format-number (elt m xlsx-c-fmt) cv))
          (let ((c 0) (i 0) (chra (- (string-to-char "A") 1)))
            (while (and  (< i (length addr)) (< chra (elt addr i)))
              (setq c (+ (* 26 c)  (- (logand -33 (elt addr i)) chra))) ;; -33 is mask to change case
              (setq i (+ 1 i)))
            (xlsx-draw-cell (- c 1) (string-to-int (substring addr i))
                            (xlsx-pad-right (elt m xlsx-c-fmtd) (- c 1))) )
          cv ) (aref m xlsx-c-val)
          )))


(defun xlsx-add-dep (ca cc)
  "Add to dep list.
   CA -- addr to to add
   CC -- cell who depends"
  (let ((addr (replace-regexp-in-string "\\$" "" ca)))
    (if (string-match xlsx-range-parts-re addr)
        (progn
          (let* ((s (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 2 addr))))
                 (cl (- (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 4 addr))) s))
                 (rl (- (xlsx-addr-to-index (concat  (match-string 3 addr) (match-string 2 addr))) s))
                 )
            (if (< cl 0)
                (progn
                  (setq cl (* -1 cl))
                  (setq s (- s cl)) ) nil)
            (if (< rl 0)
                (progn
                  (setq rl (* -1 rl))
                  (setq s (- s rl))) nil )
            (setq rl (/ rl xlsx-max-row))
            (dotimes (r (+ 1 rl))
              (dotimes (c (+ 1 cl))
                (xlsx-add-dep  (xlsx-index-to-addr (+ s c (* r xlsx-max-row))) cc)
                )
              )))
      (let  ( (m (avl-tree-member xlsx-data (xlsx-addr-to-index addr))))
        (if m
            (aset m xlsx-c-deps (append (elt m xlsx-c-deps) (list cc)))
          (progn
            (setq m (xlsx-new-cell addr))
            (aset m xlsx-c-deps (list cc))
            (avl-tree-enter xlsx-data m)
            ))))))


(defun xlsx-del-dep (ca cc)
  "Remove from dep list."
  (let ((addr (replace-regexp-in-string "\\$" "" ca)))
    (if (string-match xlsx-range-parts-re addr)
        (progn
          (let* ((s (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 2 addr))))
                 (cl (- (xlsx-addr-to-index (concat  (match-string 1 addr) (match-string 4 addr))) s))
                 (rl (- (xlsx-addr-to-index (concat  (match-string 3 addr) (match-string 2 addr))) s))
                 )
            (if (< cl 0)
                (progn
                  (setq cl (* -1 cl))
                  (setq s (- s cl)) ) nil)
            (if (< rl 0)
                (progn
                  (setq rl (* -1 rl))
                  (setq s (- s rl))) nil )
            (setq rl (/ rl xlsx-max-row))
            (dotimes (r (+ 1 rl))
              (dotimes (c (+ 1 cl))
                (xlsx-del-dep (xlsx-index-to-addr (+ s c (* r xlsx-max-row))) cc)
                )
              )))
      (let  ( (m (avl-tree-member xlsx-data (xlsx-addr-to-index addr))))
        (if
            (delete cc (aref m xlsx-c-deps))
            nil) ))))



;;;###autoload
(define-derived-mode xlsx-mode text-mode xlsx-empty-name
  "ss game mode
  Keybindings:
  \\{xlsx-map} "
  (use-local-map xlsx-map)
  ;;  (setq truncate-lines 1)

  ;; (unless (featurep 'emacs)
  ;;   (setq mode-popup-menu
  ;;         '("ss Commands"
  ;;           ["Start new game"        xlsx-start-game]
  ;;           ["End game"                xlsx-end-game
  ;;            (xlsx-active-p)]
  ;;           ))



  (setq xlsx-cur-col 0)
  (setq xlsx-max-col 30)
  (setq xlsx-col-widths (make-vector xlsx-max-col 7))
  (setq xlsx-cur-row 1)
  (setq xlsx-max-row 30)
  (setq xlsx-row-padding 4)
  (setq xlsx-data (avl-tree-create 'xlsx-avl-cmp))
  (if (file-exists-p (buffer-file-name)) (xlsx-load (buffer-file-name)) nil)

  (xlsx-draw-all)

  )

;;  ____        __ __  __       _   _
;; |  _ \  ___ / _|  \/  | __ _| |_| |__  ___
;; | | | |/ _ \ |_| |\/| |/ _` | __| '_ \/ __|
;; | |_| |  __/  _| |  | | (_| | |_| | | \__ \
;; |____/ \___|_| |_|  |_|\__,_|\__|_| |_|___/

(defmath SUM (x)
  "add the items in the range"
  (interactive 1 "sum")
  :" vsum(x)"

  )

(defmath ROUND (x n)
  "Rounds the number x to n"
  (interactive 2 "x n")
  :" (calc-floor(x * 10^n) / 10^n)"

  )


(provide 'xlsx-mode)
(push (cons "\\.\\(xls\\|xlsx\\|XLSX\\)$" 'xlsx-mode) auto-mode-alist)
(push (cons "\\.\\(csv\\|CSV\\)$" 'xlsx-mode) auto-mode-alist)

;;; xlsx-mode.el enxds here
