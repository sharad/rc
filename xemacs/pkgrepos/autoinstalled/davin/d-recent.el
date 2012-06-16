;;; d-recent.el --- log of recently edited files

;; Copyright (C) 2006-2011 Davin Pearson

;; Author/Maintainer: Davin Pearson http://www.davinpearson.com
;; Keywords: recently edited files
;; Version: 1.0

;;; Limitation of Warranty

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; When you quit Emacs a list of files that were edited is written to
;; disk in the file and folder ~/.emacs.d/recent-YYYYMMDD-HHMMSS.

;;; Install Instructions:

;; See the following URL for the latest info and a tarball:

;; http://davin.50webs.com/research/2010/mopa2e2.html#d-recent.el

;; Extract the file in the above-mentioned tarball and put it
;; somewhere in load-path and load it by putting the following
;; command in your .emacs file:
;;
;; (require 'd-recent.el)

;;; Known Bugs:

;; None so far!

(defvar d-recent--pad-width 10)
(defvar d-recent--keymap (make-keymap))
(defvar d-recent--buf-name "*recent*")

(global-set-key [kp-enter] 'd-recent-switch-to-buffer)

(defvar d-recent--time-emacs-started-stamp (d-time--decode-time-readable (current-time)))
(defvar d-recent--persistent-file-name (concat "~/.emacs.d/recent-"
                                               d-recent--time-emacs-started-stamp))

(defun d-recent--get-nf-string (cons-cell)
  (assert (consp cons-cell))
  (let ((number (format (concat "%" (format "%d" 10) "d") (car cons-cell)))
        (file   (cdr cons-cell)))
    (concat number " " file)))

(defun d-recent--insert-lines ()
  (progn
    (if (get-buffer d-recent--buf-name)
        (kill-buffer d-recent--buf-name))
    (generate-new-buffer d-recent--buf-name)
    (set-buffer d-recent--buf-name)
    (erase-buffer)
    (goto-char (point-min))
    (let ((ptr d-recent--list))
      (while ptr
        ;;(debug)
        (if (file-exists-p (cdar ptr))
            (insert (d-recent--get-nf-string (car ptr)) "\n"))
        (setq ptr (cdr ptr)))
      ))
  )

(defun d-recent--fontify-lines ()
  (progn
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let ((s (d-current-line-as-string))
            (min (point-at-bol))
            (max (point-at-eol)))
        (put-text-property min
                           (1+ max)
                           'face
                           (car (d-groups-get-face (substring s (1+ d-recent--pad-width)))))
        (forward-line 1))
      )
    )
  )

(defun d-recent--pred (x y)
  (> (car x) (car y)))

(defun d-recent--generate-buffer ()
  ;;(d-beeps "foo")
  (setq d-recent--list (sort d-recent--list 'd-recent--pred))

  (save-excursion

    (d-recent--insert-lines)

    (use-local-map d-recent--keymap)
    (local-set-key "\C-m" 'd-recent--find-file)

    (d-recent--fontify-lines)

    )

  (progn
    (set-buffer d-recent--buf-name)
    (goto-char (point-min)))
  )

(defun d-recent-switch-to-buffer ()
  (interactive)
  ;;(when (or (not (get-buffer d-recent--buf-name)) (= 0 (buffer-size (get-buffer d-recent--buf-name))))
  ;;(d-foo)
  (if (not (get-buffer d-recent--buf-name))
      (d-recent--generate-buffer))
  
  (switch-to-buffer d-recent--buf-name))

(defun d-recent--find-file ()
  (interactive)
  (let ((s (substring (d-current-line-as-string) (1+ d-recent--pad-width))))
    ;;(message "s=%s" s)
    (assert (file-exists-p s))
    (d-find-file s)
    ;;(kill-buffer (get-buffer d-recent--buf-name))
    )
  )

;;;
;;; USED BY: d-find.el
;;; 
(defun d-recent-find-file-hook (filename)

  (setq filename (safe-compress-file-name filename))

  (if (file-directory-p filename)
      (setq filename (concat filename "/")))
    
  (if (string= d-recent--persistent-file-name filename)
      nil
    (if (not (file-directory-p filename))
        (let ((found (rassoc filename d-recent--list)))
          (if found
              (setcar found (1+ (car found)))
            (setq d-recent--list (cons (cons 1 filename) d-recent--list))))
      
      ;;(d-recent--generate-buffer)
      
      )
    )
  )

(defun d-recent--load-log-file ()
  (progn
    (setq d-recent--list nil)
    ;;
    ;; NOTE: find-file rather than d-find-file is used here to prevent an infinite recursion
    ;;
    (find-file d-recent--persistent-file-name)
    (goto-char (point-min))
    (while (< (point) (point-max))
      (let* ((line   (d-current-line-as-string))
             (number (read (substring line 0 d-recent--pad-width)))
             (file   (substring line (1+ d-recent--pad-width))))
        (if (file-exists-p file)
            (setq d-recent--list (cons (cons number file) d-recent--list))))
      (forward-line 1))
    (kill-buffer nil)
    )
  )

;;(d-recent--load-log-file)

;;(add-hook 'kill-emacs-hook 'd-recent--save-log-file)
;;(add-hook 'after-save-hook 'd-recent--save-log-file)
;;(add-hook 'write-file-hooks 'd-recent--save-log-file)

(defun d-recent--save-log-file ()
  (progn
    (message "*** begin save-log-file")
    ;;
    ;; NOTE: doesn't use find-file to prevent an infinite recursion
    ;;
    ;;(if (file-exists-p d-recent--persistent-file-name) (delete-file d-recent--persistent-file-name))
    (find-file d-recent--persistent-file-name)
    (erase-buffer)
    (let ((ptr d-recent--list))
      (while ptr
        (when (file-exists-p (cdar ptr))        
          (setq line (d-recent--get-nf-string (car ptr)))
          (insert line "\n"))
        ;;(message "line=%s" line)
        ;;(shell-command (concat "echo >>" d-recent--persistent-file-name " \"" line "\""))
        (setq ptr (cdr ptr))))
    (save-buffer)
    (kill-buffer nil)
    (message "*** end save-log-file")
    )
  )

;;;
;;; (setq ptr (buffer-list))
;;; (setq ptr (cdr ptr))
;;;
(defadvice save-some-buffers (before d-recent activate)
  (let* ((list  (buffer-list))
         (ptr   list)
         (found nil))
    (setq d-recent--list (sort d-recent--list 'd-recent--pred))
    (while (and ptr (not found))
      (when (and (buffer-modified-p (car ptr)) (buffer-file-name (car ptr)))
        ;;(debug)
        (setq found t))
      (setq ptr (cdr ptr)))
    ;;(debug)
    (if found
        (d-recent--save-log-file))))

(setq d-recent--list nil)

(provide 'd-recent)


