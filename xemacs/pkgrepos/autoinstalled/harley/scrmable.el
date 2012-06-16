;;; scrmable.el --- scrmable words in a buffer
;;
;; ~harley/share/emacs/pkg/scrmable/scrmable.el ---
;;
;; $Id: scrmable.el,v 1.5 2006/09/12 23:44:23 harley Exp $
;;

;; Author:  harley <harley@panix.com>
;; URL:     http://www.mahalito.net/~harley/elisp/scrmable.el
;; License: GPL v2

;;; Commentary:
;; * http://www.livejournal.com/users/jwz/256229.html

;;; History:
;;  2003-09-14: Written after reading jwz's blog.

;;; Code:
(defun scrmable-str (str)
  "Scrmable STR.  The first and last characters are not scrambled."
  (cond
   ((not (stringp str))
    str)
   ((<= (length str) 3)
    str)
   (t (do* ((rts (copy-seq str))
            (len  (1- (length rts)))
            (ic 1 (1+ ic))
            (ir (random (- len ic)) (random (- len ic))) )
          ((>= ic len) rts)
        (let ((cc (elt rts ic))
              (cr (elt rts (+ ic ir))))
          (aset rts ic cr)
          (aset rts (+ ic ir) cc))))))
;; (mapcar 'scrmable-str '(1 "a" "ab" "abc" "abcdefghi"))

(defun scrmable-region (rstart rend)
  "Scrmable the words from RSTART to REND."
  (interactive "r")
  (save-excursion
    (goto-char rstart)
    ;; dont bother with short words
    (while (search-forward-regexp "\\<\\sw\\sw\\sw\\sw+\\>" rend t)
      (replace-match (scrmable-str (match-string-no-properties 0)) t t) )))

(defun scrmable-word ()
  "Scrmable the next word."
  (interactive)
  (when (forward-word 1)
    (let ((wend (point)))
      (backward-word 1)
      (scrmable-region (point) wend)
      (goto-char wend))))

;; (global-set-key "\M-s" 'scrmable-word)

(provide 'scrmable)

;;; scrmable.el ends here
