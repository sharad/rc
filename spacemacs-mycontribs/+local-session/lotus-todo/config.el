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



(progn "todo"
  ; http://stackoverflow.com/a/2242801/341107
  ;;This command will do something like you want.

  (defun annotate-todo ()
    "put fringe marker on TODO: lines in the curent buffer"
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "TODO:" nil t)
        (let ((overlay (make-overlay (- (point) 5) (point))))
          (overlay-put overlay 'before-string (propertize "A"
                                                          'display '(left-fringe right-triangle)))))))

  ;; You can customize the bitmap as desired.

  ;; To get this to apply to all files, you could add it to the 'find-file-hooks

  (add-hook 'find-file-hooks 'annotate-todo))

;;{{
;; http://emacs-fu.blogspot.in/2008/12/highlighting-todo-fixme-and-friends.html
 (lambda ()
   (font-lock-add-keywords nil '(("\\<\\(FIXME\\|TODO\\|BUG\\):" 1 font-lock-warning-face t))))
;;}}


;;{{http://emacswiki.org/emacs/AlessandroPiras#toc9

(defun get-same-mode-buffers ()
  "Get all the buffers having the same Major mode as the current
buffer"
  (interactive)
  (remove-if-not (lambda (buf)
		   (eql (with-current-buffer buf major-mode)
			major-mode))
		 (buffer-list)))

(defun fixme-occur ()
  "Finds all the occurrences of the fixme keywords in all buffers
having the same major mode as the current buffer"
  (interactive)
  (multi-occur (get-same-mode-buffers)
	       fixme-keyword-re-string))
;; (fixme-mode 1)
;;}}


(progn ;; "todo in dir"
  ;; http://www.datenterrorist.de/index.php?itemid=1437
  (defun grep-todos-in-dir (dir &optional not-recursive)
    "Grep recursively for TODO comments in the given directory"
    (interactive "Ddirectory:")
    (let ((recur "-r"))
      (if not-recursive
          (setq recur ""))
      (grep (concat "grep -nH -I " recur " -E \"[\\#\\/\\-\\;\\*]\s*TODO|FIXME|XXX:?\" " dir " 2>/dev/null")))
    (enlarge-window 7))

  ;; (global-set-key-if-unbind [f4] 'grep-todos-in-dir)
  )


;;; config.el ends here
