;;; config.el --- config                             -*- lexical-binding: t; -*-

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

;;

;;; Code:



;; enscript  -f Courier7  -E $f -p${dst}/${f}.ps
;; ps2pdf ${dst}/${f}.ps ${dst}/${f}.pdf

(defun find-alt-pdf-file (file)
  (interactive "ffile: ")
  (let ((filename (or file (buffer-file-name (current-buffer)))))
    (if filename
        (let* ((tmpdir (or (getenv "TMP") "/tmp/"))
               (ofile (expand-file-name (file-name-nondirectory filename)
                                        tmpdir))
               ofilemode
               (psfile (expand-file-name (concat (file-name-nondirectory filename) ".ps") tmpdir))
               (pdffile (expand-file-name (concat (file-name-nondirectory filename) ".pdf") tmpdir)))
          (when (and (file-exists-p ofile)
                     (not (file-writable-p ofile)))
            (setq ofilemode (file-modes ofile))
            (set-file-modes ofile 666))
          (copy-file filename ofile 1)
          (if ofilemode
              (set-file-modes ofile ofilemode))
          (when (file-exists-p ofile)
            (shell-command-local-no-output (concat "enscript --color -f Courier7  -E " ofile " -p" psfile))
            (message (concat "enscript --color -f Courier7  -E " ofile " -p" psfile))
            (if (file-exists-p psfile)
                (progn
                  (shell-command-local-no-output (concat "ps2pdf " psfile " " pdffile))
                  (message (concat "ps2pdf " psfile " " pdffile))
                  (if (file-exists-p pdffile)
                      (find-file pdffile)
                      (message "Not able to create %s file." pdffile)))
                (message "Not able to create %s file." psfile)))
          (message "File %s done not exists" ofile)))))


(progn ;; "editing case"
  ;; http://stackoverflow.com/questions/9288181/converting-from-camelcase-to-in-emacs

  (defun toggle-camelcase-underscores ()
    "Toggle between camelcase and underscore notation for the symbol at point."
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (currently-using-underscores-p (progn (goto-char start)
                                                   (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (progn
              (upcase-initials-region start end)
              (replace-string "_" "" nil start end)
              (downcase-region start (1+ start)))
            (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
            (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

  (defun toggle-camelcase-underscores ()
    "Toggle between camelcase and underscore notation for the symbol at point."
    (interactive)
    (save-excursion
      (let* ((bounds (bounds-of-thing-at-point 'symbol))
             (start (car bounds))
             (end (cdr bounds))
             (currently-using-underscores-p (progn (goto-char start)
                                                   (re-search-forward "_" end t))))
        (if currently-using-underscores-p
            (progn
              (upcase-initials-region start end)
              (replace-string "_" "" nil start end)
              (downcase-region start (1+ start)))
            (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
            (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))
  ;; (local-set-key "\M-\C-C"  'un-camelcase-word-at-point)
  )

;;; config.el ends here
