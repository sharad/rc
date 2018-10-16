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



;; (provide 'config)
;;; config.el ends here


;; from http://emacswiki.org/emacs/BookMarks#toc4
;; Access File Bookmarks from `C-x C-f'

;; If you hit ‘C-x C-f’ and then realize that you want a file that is bookmarked, you can get to the bookmark this way:
(defun bookmark-to-abbrevs ()
  "Create abbrevs based on `bookmark-alist'."
  (dolist (bookmark bookmark-alist)
    (let* ((name (car bookmark))
           (file (bookmark-get-filename name)))
      (define-abbrev global-abbrev-table name file))))
;; Put Last-Selected Bookmark on Top

;; Using this method you’ll find frequently used bookmarks easily (cho-seiri-hou in Japanese). – rubikitch

(defadvice bookmark-jump (after bookmark-jump activate)
  (let ((latest (bookmark-get-bookmark bookmark)))
    (setq bookmark-alist (delq latest bookmark-alist))
    (add-to-list 'bookmark-alist latest)))


;; Syncing Bookmarks with zsh

;; zsh has a feature called ‘cd-able-vars’ which is similar to
;; bookmarks but limited to directories on the local machine. Here’s
;; some code to convert emacs’ bookmarks into code for zsh. First add
;; this to your .zshrc:

;;     setopt cd_able_vars
;;     [[ -r ~/.zsh.bmk ]] && source ~/.zsh.bmk

;; Add this to your .emacs:

(defadvice bookmark-write-file
    (after local-directory-bookmarks-to-zsh-advice activate)
  (local-directory-bookmarks-to-zsh))

(defun local-directory-bookmarks-to-zsh ()
  (interactive)
  (when (and (require 'tramp nil t)
             (require 'bookmark nil t))
    (set-buffer (find-file-noselect "~/.zsh.bmk" t t))
    (delete-region (point-min) (point-max))
    (insert "# -*- mode:sh -*-\n")
    (let (collect-names)
      (mapc (lambda (item)
              (let ((name (replace-regexp-in-string "-" "_" (car item)))
                    (file (cdr (assoc 'filename
                                      (if (cddr item) item (cadr item))))))
                (when (and (not (tramp-tramp-file-p file))
                           (file-directory-p file))
                  (setq collect-names (cons (concat "~" name) collect-names))
                  (insert (format "%s=\"%s\"\n" name (expand-file-name file) name)))))
            bookmark-alist)
      (insert ": " (mapconcat 'identity collect-names " ") "\n"))
    (let ((backup-inhibited t)) (save-buffer))
    (kill-buffer (current-buffer))))
