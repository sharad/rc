;;; copy-config.el --- copy config

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
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



;; from: http://emacswiki.org/emacs/CopyWithoutSelection

;; Introduction

;; The following functions will copy the word, the line, the
;; paragraph and the string and the Parenthesis at point, and will
;; paste them to the mark. If it is used in shell-mode, stuff will
;; be pasted at end of shell prompt.

;; Give a prefix 1 will prevent paste.

;; And besides, there are also something to do with some basic
;; shortcuts.  Selecting words or sexps without moving the cursor

;; You can select word or sexp after point and put it in the
;; kill-ring by typing:

;;          C-M-SPC M-w

;; This does not move the cursor. If you want to select the next two
;; words after point, just type C-M-SPC C-M-SPC M-w. Killing next
;; word or sexp can be done with C-M-SPC C-w, or better with C-M-k.
;; the base functions

     (defun get-point (symbol &optional arg)
      "get the point"
      (funcall symbol arg)
      (point)
     )

     (defun copy-thing (begin-of-thing end-of-thing &optional arg)
       "copy thing between beg & end into kill ring"
        (save-excursion
          (let ((beg (get-point begin-of-thing 1))
         	 (end (get-point end-of-thing arg)))
            (copy-region-as-kill beg end)))
     )

     (defun paste-to-mark(&optional arg)
       "Paste things to mark, or to the prompt in shell-mode"
       (let ((pasteMe
     	 (lambda()
     	   (if (string= "shell-mode" major-mode)
     	     (progn (comint-next-prompt 25535) (yank))
     	   (progn (goto-char (mark)) (yank) )))))
     	(if arg
     	    (if (= arg 1)
     		nil
     	      (funcall pasteMe))
     	  (funcall pasteMe))
     	))


;; Copy Word

     (defun copy-word (&optional arg)
      "Copy words at point into kill-ring"
       (interactive "P")
       (copy-thing 'backward-word 'forward-word arg)
       ;;(paste-to-mark arg)
     )

;; Key binding

     (global-set-key (kbd "C-c w")         (quote copy-word))

;; Copy Line

     (defun copy-line (&optional arg)
      "Save current line into Kill-Ring without mark the line "
       (interactive "P")
       (copy-thing 'beginning-of-line 'end-of-line arg)
       (paste-to-mark arg)
     )

;; Key binding

;; (global-set-key (kbd "C-c l")         (quote copy-line))

;; Tip: WholeLineOrRegion copies the current line when the region is
;; not active. Thus, C-a M-w C-y C-y … duplicates the current
;; line (similar to vi’s ‘yyp’), while C-w deletes it (‘dd’). When
;; the region is active, M-w and C-w act as usual. :) Copy Paragraph

     (defun copy-paragraph (&optional arg)
      "Copy paragraphes at point"
       (interactive "P")
       (copy-thing 'backward-paragraph 'forward-paragraph arg)
       (paste-to-mark arg)
     )

;; key binding

;; (global-set-key (kbd "C-c p")         (quote copy-paragraph))

;; Copy String

     (defun beginning-of-string(&optional arg)
       "  "
       (re-search-backward "[ \t]" (line-beginning-position) 3 1)
     	     (if (looking-at "[\t ]")  (goto-char (+ (point) 1)) )
     )
     (defun end-of-string(&optional arg)
       " "
       (re-search-forward "[ \t]" (line-end-position) 3 arg)
     	     (if (looking-back "[\t ]") (goto-char (- (point) 1)) )
     )

     (defun thing-copy-string-to-mark(&optional arg)
       " Try to copy a string and paste it to the mark
     When used in shell-mode, it will paste string on shell prompt by default "
       (interactive "P")
       (copy-thing 'beginning-of-string 'end-of-string arg)
       (paste-to-mark arg)
     )


;; Key binding

      (global-set-key (kbd "C-c s")         (quote thing-copy-string-to-mark))

;; Copy Parenthesis

;; Similar with new copy string to mark

     (defun beginning-of-parenthesis(&optional arg)
       "  "
       (re-search-backward "[[<(?\"]" (line-beginning-position) 3 1)
     	     (if (looking-at "[[<(?\"]")  (goto-char (+ (point) 1)) )
     )
     (defun end-of-parenthesis(&optional arg)
       " "
       (re-search-forward "[]>)?\"]" (line-end-position) 3 arg)
     	     (if (looking-back "[]>)?\"]") (goto-char (- (point) 1)) )
     )

     (defun thing-copy-parenthesis-to-mark(&optional arg)
       " Try to copy a parenthesis and paste it to the mark
     When used in shell-mode, it will paste parenthesis on shell prompt by default "
       (interactive "P")
       (copy-thing 'beginning-of-parenthesis 'end-of-parenthesis arg)
       (paste-to-mark arg)
     )
;; (global-set-key (kbd "C-c a")         (quote thing-copy-parenthesis-to-mark))



(provide 'copy-config)
;;; copy-config.el ends here
