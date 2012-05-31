;;; jhg-footnote.el --- Functions for Footnotes
;;
;; ~harley/share/emacs/pkg/footnote/footnote.el ---
;;
;; $Id: footnote.el,v 1.10 2005/10/04 03:58:52 harley Exp $
;;

;; Author:    Harley Gorrell <harley@mahalito.net>
;; URL:       http://www.mahalito.net/~harley/elisp/footnote.el
;; License:   GPL v2

;;; Commentary:
;; * My functions for footnotes.
;; * Inserts urls with quickurl

;;; History:
;;  2003-03-16: Updated URL and contact info

;;; Code:

(defvar footnote-num nil
  "*The number of the last footnote in the buffer.")
(make-variable-buffer-local 'footnote-nextnum)

(defvar footnote-marker nil)
(make-variable-buffer-local 'footnote-marker)

(defun footnote-nextnum ()
  "Return the number of the next footnote to be inserted."
  (when (not footnote-num)
    (setq footnote-num (footnote-findmax)))
  (setq footnote-num (1+ footnote-num)))

(defun footnote-findmax ()
  "Find and return the largest footnote number in the buffer."
  (let ((max 0))
    (save-excursion
      (goto-char (point-min))
      (while (search-forward-regexp "^\\[\\([0-9]+\\)\\]" (point-max) t)
	(setq max (max max (string-to-number (match-string 1))))))
    max))

(defun footnote-num-set (num)
  "Set the next footnote to NUM."
  (interactive "NFootnote#")
  (setq footnote-num num))

(defun footnote-insert (cnt footnote)
  "Insert the CNT FOOTNOTE."
  (insert (format "[%d] " cnt))
  (save-excursion
    (if footnote-marker
      (goto-char footnote-marker)
      (goto-char (point-max))
      (insert "\n\n"))
    (end-of-line)
    (insert "\n" (format "[%d] " cnt) footnote)
    (footnote-marker (point))
    cnt))

(defun footnote-marker (pnt)
  "Set the footnote PNT to current point."
  (interactive "d")
  (unless footnote-marker
    (setq footnote-marker (make-marker)))
  (set-marker footnote-marker pnt))

;;;###autoload
(defun footnote (arg)
  "Prompt for and insert a footnote into the current buffer.
ARG sets the footnote number."
  (interactive "P")
  (if arg (footnote-num-set (1- arg))) ;; we will inc next
  (let ((num (footnote-nextnum)))
    (condition-case err
        (footnote-insert num (read-from-minibuffer (format "[%d] " num)))
      ('quit ;; roll back the change on quit (if it is still the same)
       (setq footnote-num (1- footnote-num))))
    nil))

(defun footnote-quickurl (url &optional silent)
  "Insert the quickurl URL as a footnote (SILENT is ignored)."
  (footnote-insert (footnote-nextnum) (cdr url)))

;; insert urls as footnotes
(fset 'quickurl-insert 'footnote-quickurl)

(defun footnote-renumber ()
  "Renumber the footnotes in the buffer."
  (interactive)
  (let ((f-list '())
        f-number-old f-number-rec f-number-rec)
    (setq footnote-num 0) ;; reset the counter
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "\\[\\([0-9]+\\)\\] " (point-max) t)
        ;; (message "%s" (match-string 1))
        ;; Seen this footnote number already?
        (setq f-number-old (match-string-no-properties 1))
        (setq f-number-rec (assoc f-number-old f-list))
        (if f-number-rec
          ;; yes - replace with new number
          (replace-match (format "[%s] " (cdr f-number-rec)))
          ;; no - make a new footnote number and remember it
          (progn
            (setq f-number-new (format "%d" (footnote-nextnum)))
            (replace-match (format "[%s] " f-number-new))
            (setq f-list (cons (cons f-number-old f-number-new) f-list))) )))))

(defun footnote-region-find ()
  "Find the region around a block of footnotes."
  (interactive)
  (let (foot-s foot-e)
    (save-excursion
      (goto-char (point-max))
      ;; find the start of the footnote region
      (when (re-search-backward "^\\[" (point-min) t)
        (while (looking-at "^\\[")
          (forward-line -1))
        (setq foot-s (point)))
      ;; find the end
      (when foot-s
        (next-line 1)
        (while (looking-at "^\\[")
          (forward-line 1))
        (setq foot-e (point)))
      ;; sort em (need a better sort)
      (if (and foot-s foot-e)
        (list foot-s foot-e)
        nil))))

(defun footnote-region-sort (foot-s foot-e)
  "Sort the region of footnotes from FOOT-S to FOOT-E."
  (interactive "r")
  (sort-lines nil foot-s foot-e))

(defun footnote-sort ()
  "Find and sort a region of footnotes."
  (interactive)
  (let ((foot-region (footnote-region-find)))
    (when foot-region
      (footnote-region-sort (car foot-region) (cadr foot-region)))))

(defun footnote-renumber-and-sort ()
  "Renumber and sort the footnotes."
  (interactive)
  (footnote-renumber)
  (footnote-sort))

(provide 'footnote)

;;; footnote.el ends here
