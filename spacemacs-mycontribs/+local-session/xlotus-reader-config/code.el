;;; reader-config.el --- Reader Config

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



(deh-require-maybe reader-mode
  (add-element-to-lists
   #'(lambda ()
       (reader-mode 1))
   reader-requester))

(deh-require-maybe page-ext
  )

(deh-section "pb"
  (defun page-break-xx-display-table (window)
    "Create a display-table that displays page-breaks prettily."
    (let ((table (or (copy-sequence (window-display-table window))
                     (make-display-table))))
      (aset table ?\^L
            "abc")
      table))

  (defun set-xx-table (window)
    (set-window-display-table window
                              (page-break-xx-display-table window)))

  (defun page-break-xx-mode-hook-function  ()
    (interactive)
    "Function called for updating display table"
    (mapcar 'set-xx-table
            (window-list nil 'no-minibuffer))))

(deh-section "page break"
  ;; http://www.emacswiki.org/emacs/PageBreaks
  (defvar page-break-face 'bold)
  (defvar page-break-string-char ?-)

  (defun page-break-display-table (window)
    "Create a display-table that displays page-breaks prettily."
    (let ((table (or (copy-sequence (window-display-table window))
                     (make-display-table))))
      (aset table ?\^L
            (let ((face-offset (lsh (face-id page-break-face) 19)))
              (vconcat (mapcar (lambda (c) (+ face-offset c))
                               (make-string (1- (window-width window))
                                            page-break-string-char)))))
      table))

  ;; http://ergoemacs.org/emacs/modernization_formfeed.html
  ;; "§ ────────── ────────── ────────── ────────── ──────────"

  (defun page-break-mode-hook-function  ()
    "Function called for updating display table"
    (mapcar (lambda (window)
              (set-window-display-table window
                                        (page-break-display-table window)))
            (window-list nil 'no-minibuffer)))

  (define-minor-mode page-break-mode
      "Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters."
    :global t
    :lighter " Pgbrk"
    (if page-break-mode
        (add-hook 'window-configuration-change-hook
                  'page-break-mode-hook-function )
        (remove-hook 'window-configuration-change-hook
                     'page-break-mode-hook-function)))

  (defun turn-on-page-break-mode ()
    (page-break-mode 1))

  (defun turn-off-page-break-mode ()
    (page-break-mode -1))

  ;; (turn-on-page-break-mode)
  ;; (turn-off-page-break-mode)
  )

(provide 'reader-config)
;;; reader-config.el ends here
