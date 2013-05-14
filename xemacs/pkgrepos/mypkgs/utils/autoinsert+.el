;;; autoinsert+.el --- automatic mode-dependent insertion of text into new files

;; Copyright (C) 1985-1987, 1994-1995, 1998, 2000-2012
;;   Free Software Foundation, Inc.

;; Author: Charlie Martin <crm@cs.duke.edu>
;; Adapted-By: Daniel Pfeiffer <occitan@esperanto.org>
;; Keywords: convenience
;; Maintainer: FSF

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;  The following defines an association list for text to be
;;  automatically inserted when a new file is created, and a function
;;  which automatically inserts these files; the idea is to insert
;;  default text much as the mode is automatically set using
;;  auto-mode-alist.
;;
;;  To use:
;;     (add-hook 'find-file-hook 'auto-insert+)
;;     setq auto-insert+-directory to an appropriate slash-terminated value
;;
;;  You can also customize the variable `auto-insert+-mode' to load the
;;  package.  Alternatively, add the following to your .emacs file:
;;  (auto-insert+-mode 1)
;;
;;  Author:  Charlie Martin
;;           Department of Computer Science and
;;           National Biomedical Simulation Resource
;;           Box 3709
;;           Duke University Medical Center
;;           Durham, NC 27710
;;	      (crm@cs.duke.edu,mcnc!duke!crm)

;;; Code:

(defgroup auto-insert+ nil
  "Automatic mode-dependent insertion of text into new files."
  :prefix "auto-insert+-"
  :group 'files
  :group 'convenience
  :link '(custom-manual "(autotype) Autoinsert+ing"))


(defcustom auto-insert+ 'not-modified
  "Controls automatic insertion into newly found empty files.
Possible values:
	nil	do nothing
	t	insert if possible
	other	insert if possible, but mark as unmodified.
Insertion is possible when something appropriate is found in
`auto-insert+-alist'.  When the insertion is marked as unmodified, you can
save it with  \\[write-file] RET.
This variable is used when the function `auto-insert+' is called, e.g.
when you do (add-hook 'find-file-hook 'auto-insert+).
With \\[auto-insert+], this is always treated as if it were t."
  :type '(choice (const :tag "Insert if possible" t)
                 (const :tag "Do nothing" nil)
                 (other :tag "insert if possible, mark as unmodified."
                        not-modified))
  :group 'auto-insert+)

(defcustom auto-insert+-query 'function
  "Non-nil means ask user before auto-insert+ing.
When this is `function', only ask when called non-interactively."
  :type '(choice (const :tag "Don't ask" nil)
                 (const :tag "Ask if called non-interactively" function)
                 (other :tag "Ask" t))
  :group 'auto-insert+)

(defcustom auto-insert+-prompt "Perform %s auto-insert+ion? "
  "Prompt to use when querying whether to auto-insert+.
If this contains a %s, that will be replaced by the matching rule."
  :type 'string
  :group 'auto-insert+)


(defcustom auto-insert-alist
  '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
     (upcase (concat (file-name-nondirectory
		      (file-name-sans-extension buffer-file-name))
		     "_"
		     (file-name-extension buffer-file-name)))
     "#ifndef " str \n
     "#define " str "\n\n"
     _ "\n\n#endif")

    (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
     nil
     "#include \""
     (let ((stem (file-name-sans-extension buffer-file-name)))
       (cond ((file-exists-p (concat stem ".h"))
	      (file-name-nondirectory (concat stem ".h")))
	     ((file-exists-p (concat stem ".hh"))
	      (file-name-nondirectory (concat stem ".hh")))))
     & ?\" | -10)

    (("[Mm]akefile\\'" . "Makefile") . "makefile.inc")

    (html-mode . (lambda () (sgml-tag "html")))

    (plain-tex-mode . "tex-insert.tex")
    (bibtex-mode . "tex-insert.tex")
    (latex-mode
     ;; should try to offer completing read for these
     "options, RET: "
     "\\documentclass[" str & ?\] | -1
     ?{ (read-string "class: ") "}\n"
     ("package, %s: "
      "\\usepackage[" (read-string "options, RET: ") & ?\] | -1 ?{ str "}\n")
     _ "\n\\begin{document}\n" _
     "\n\\end{document}")

    (("/bin/.*[^/]\\'" . "Shell-Script mode magic number") .
     (lambda ()
       (if (eq major-mode (default-value 'major-mode))
	   (sh-mode))))

    (ada-mode . ada-header)

    (("\\.[1-9]\\'" . "Man page skeleton")
     "Short description: "
     ".\\\" Copyright (C), " (substring (current-time-string) -4) "  "
     (getenv "ORGANIZATION") | (progn user-full-name)
     "
.\\\" You may distribute this file under the terms of the GNU Free
.\\\" Documentation License.
.TH " (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
     " " (file-name-extension (buffer-file-name))
     " " (format-time-string "%Y-%m-%d ")
     "\n.SH NAME\n"
     (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
     " \\- " str
     "\n.SH SYNOPSIS
.B " (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
     "\n"
     _
     "
.SH DESCRIPTION
.SH OPTIONS
.SH FILES
.SH \"SEE ALSO\"
.SH BUGS
.SH AUTHOR
" (user-full-name)
     '(if (search-backward "&" (line-beginning-position) t)
	  (replace-match (capitalize (user-login-name)) t t))
     '(end-of-line 1) " <" (progn user-mail-address) ">\n")

    (("\\.el\\'" . "Emacs Lisp header")
     "Short description: "
     ";;; " (file-name-nondirectory (buffer-file-name)) " --- " str "

;; Copyright (C) " (substring (current-time-string) -4) "  "
 (getenv "ORGANIZATION") | (progn user-full-name) "

;; Author: " (user-full-name)
'(if (search-backward "&" (line-beginning-position) t)
     (replace-match (capitalize (user-login-name)) t t))
'(end-of-line 1) " <" (progn user-mail-address) ">
;; Keywords: "
 '(require 'finder)
 ;;'(setq v1 (apply 'vector (mapcar 'car finder-known-keywords)))
 '(setq v1 (mapcar (lambda (x) (list (symbol-name (car x))))
		   finder-known-keywords)
	v2 (mapconcat (lambda (x) (format "%12s:  %s" (car x) (cdr x)))
	   finder-known-keywords
	   "\n"))
 ((let ((minibuffer-help-form v2))
    (completing-read "Keyword, C-h: " v1 nil t))
    str ", ") & -2 "

\;; This program is free software; you can redistribute it and/or modify
\;; it under the terms of the GNU General Public License as published by
\;; the Free Software Foundation, either version 3 of the License, or
\;; (at your option) any later version.

\;; This program is distributed in the hope that it will be useful,
\;; but WITHOUT ANY WARRANTY; without even the implied warranty of
\;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
\;; GNU General Public License for more details.

\;; You should have received a copy of the GNU General Public License
\;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

\;;; Commentary:

\;; " _ "

\;;; Code:



\(provide '"
       (file-name-sans-extension (file-name-nondirectory (buffer-file-name)))
       ")
\;;; " (file-name-nondirectory (buffer-file-name)) " ends here\n")
    (("\\.texi\\(nfo\\)?\\'" . "Texinfo file skeleton")
     "Title: "
     "\\input texinfo   @c -*-texinfo-*-
@c %**start of header
@setfilename "
     (file-name-sans-extension
      (file-name-nondirectory (buffer-file-name))) ".info\n"
      "@settitle " str "
@c %**end of header
@copying\n"
      (setq short-description (read-string "Short description: "))
      ".\n\n"
      "Copyright @copyright{} " (substring (current-time-string) -4) "  "
      (getenv "ORGANIZATION") | (progn user-full-name) "

@quotation
Permission is granted to copy, distribute and/or modify this document
under the terms of the GNU Free Documentation License, Version 1.3
or any later version published by the Free Software Foundation;
with no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.
A copy of the license is included in the section entitled ``GNU
Free Documentation License''.

A copy of the license is also available from the Free Software
Foundation Web site at @url{http://www.gnu.org/licenses/fdl.html}.

@end quotation

The document was typeset with
@uref{http://www.texinfo.org/, GNU Texinfo}.

@end copying

@titlepage
@title " str "
@subtitle " short-description "
@author " (getenv "ORGANIZATION") | (progn user-full-name)
     " <" (progn user-mail-address) ">
@page
@vskip 0pt plus 1filll
@insertcopying
@end titlepage

@c Output the table of the contents at the beginning.
@contents

@ifnottex
@node Top
@top " str "

@insertcopying
@end ifnottex

@c Generate the nodes for this menu with `C-c C-u C-m'.
@menu
@end menu

@c Update all node entries with `C-c C-u C-n'.
@c Insert new nodes with `C-c C-c n'.
@node Chapter One
@chapter Chapter One

" _ "

@node Copying This Manual
@appendix Copying This Manual

@menu
* GNU Free Documentation License::  License for copying this manual.
@end menu

@c Get fdl.texi from http://www.gnu.org/licenses/fdl.html
@include fdl.texi

@node Index
@unnumbered Index

@printindex cp

@bye

@c " (file-name-nondirectory (buffer-file-name)) " ends here\n"))
  "A list specifying text to insert by default into a new file.
Elements look like (CONDITION . ACTION) or ((CONDITION . DESCRIPTION) . ACTION).
CONDITION may be a regexp that must match the new file's name, or it may be
a symbol that must match the major mode for this element to apply.
Only the first matching element is effective.
Optional DESCRIPTION is a string for filling `auto-insert+-prompt'.
ACTION may be a skeleton to insert (see `skeleton-insert'), an absolute
file-name or one relative to `auto-insert+-directory' or a function to call.
ACTION may also be a vector containing several successive single actions as
described above, e.g. [\"header.insert\" date-and-author-update]."
  :type 'sexp
  :group 'auto-insert+)


;; Establish a default value for auto-insert+-directory
(defcustom auto-insert+-directory "~/insert/"
  "Directory from which auto-insert+ed files are taken.
The value must be an absolute directory name;
thus, on a GNU or Unix system, it must end in a slash."
  :type 'directory
  :group 'auto-insert+)


(defvar auto-insert+-alist nil)
(defvar auto-noinsert+-alist nil)

;;(setq auto-insert+-alist nil auto-noinsert+-alist nil)

(defun auto-insert+choose-action (alist)
       (let ((name (completing-read "Which? " alist)))
         (cdr (assoc name alist))))

(defun auto-insert+run-action (action)
  (and action
       (or (not (stringp action))
           (file-readable-p (expand-file-name
                             action auto-insert+-directory)))
       ;; (or (not auto-insert+-query)
       ;;     (if (eq auto-insert+-query 'function)
       ;;         (eq this-command 'auto-insert+))
       ;;     (y-or-n-p (format auto-insert+-prompt desc)))
       (mapc
        (lambda (action)
          (if (stringp action)
              (if (file-readable-p
                   (setq action (expand-file-name
                                 action auto-insert+-directory)))
                  (insert-file-contents action))
              (save-window-excursion
                ;; make buffer visible before skeleton or function
                ;; which might ask the user for something
                (switch-to-buffer (current-buffer))
                (if action
                    (if (and (consp action)
                             (not (eq (car action) 'lambda)))
                        (skeleton-insert action)
                        (funcall action))))))
        (if (vectorp action)
            action
            (vector action)))))



;; TODO: check major mode with reverse auto-mode-alist file pattern.
;;;###autoload
(defun auto-insert+ ()
  "Insert default contents into new files if variable `auto-insert+' is non-nil.
Matches the visited file name against the elements of `auto-insert+-alist'."
  (interactive)
  ;; (message "in auto-insert+")
  (and (not buffer-read-only)
       (or (eq this-command 'auto-insert+)
	   (and auto-insert+
		(bobp) (eobp)))

       ;; no action test
       (let ((alist auto-noinsert+-alist)
             noaction-alist cond)
         (while alist
           (setq cond (caar alist))
	   (if (if (symbolp cond)
		   (eq cond major-mode)
                   (and buffer-file-name
                        (string-match cond buffer-file-name)))
	       (setq noaction-alist (append noaction-alist (caar alist))
                     alist nil))
           (setq alist (cdr alist)))
         ;; (message "noaction-alist %s" noaction-alist)
         (not noaction-alist))

       (let ((alist auto-insert+-alist)
	     case-fold-search cond desc action-alist)
	 (goto-char 1)
	 ;; find all matching alist entry
	 (while alist

	   (let* ((element (car alist))
                  (cond
                    (if (atom (car element))
                        (car element)
                        (caar element)))
                  (newdesc
                   (if (atom (car element))
                       (car element)
                       (cdar element))))

             (if

              (some (lambda (c)
                         (if (symbolp c)
                             (eq cond major-mode)
                             (and buffer-file-name
                                  (string-match cond buffer-file-name))))
                       (if (consp cond) cond (list cond)))

                 (setq action-alist (append action-alist (cdr element))
                       desc (concat desc (if newdesc (format " or %s" newdesc))))))

           (setq alist (cdr alist)))

	 ;; Now, if we found something, do it
         (if (and action-alist
                  (or (not auto-insert+-query)
                      (if (eq auto-insert+-query 'function)
                          (eq this-command 'auto-insert+))
                      (y-or-n-p (format auto-insert+-prompt desc))))
             (auto-insert+run-action (auto-insert+choose-action action-alist)))

	 (and (buffer-modified-p)
	      (not (eq this-command 'auto-insert+))
	      (set-buffer-modified-p (eq auto-insert+ t)))))
  ;; Return nil so that it could be used in
  ;; `find-file-not-found-hooks', though that's probably inadvisable.
  nil)


;;;###autoload
(defun define-auto-insert+ (condition name action &optional after)
  "Associate CONDITION with (additional) ACTION in `auto-insert+-alist'.
Optional AFTER means to insert action after all existing actions for CONDITION,
or if CONDITION had no actions, after all other CONDITIONs."
  (unless (stringp name)
    (error "name arg not string"))
  (unless action
    (error "action arg can not be nil"))
  (unless (assoc condition auto-insert+-alist)
    (if after
        (nconc auto-insert+-alist
               (list (cons condition (list (cons name nil)))))
        (push (cons condition (list (cons name nil)))
              auto-insert+-alist)))
  (let ((elt (assoc condition auto-insert+-alist)))
    (if elt
        (let* ((action-alist (cdr elt))
               (name-action (assoc name action-alist)))
          (if name-action
              (setcdr name-action
                      (if (vectorp action)
                          action
                          (if (vectorp (cdr name-action))
                              (vconcat (if after (cdr name-action))
                                       (if (vectorp action) action (vector action))
                                       (if after () (cdr name-action)))
                              (if (cdr name-action)
                                  (if after
                                      (vector (cdr name-action) action)
                                      (vector action (cdr name-action)))
                                  (vector action)))))
              (if after
                  (nconc action-alist (list (cons name action)))
                  (push (cons name action) action-alist))))
        (message "you should not see this message."))))


;;;###autoload
(defun set-auto-insert+noaction (condition)
  "Set pattern where auto-insert+ should not be performed."
  (unless (member condition auto-noinsert+-alist)
      (push condition auto-noinsert+-alist)))

(require 'autoinsert)
(defun add-from-autoinsert-alist (name alist &optional after)
  (dolist (elt alist)
    (define-auto-insert+ (car elt) name (cdr elt) after)))

(add-from-autoinsert-alist "old" auto-insert-alist)


(defvar tmpledir-alist
  '(("yastemp" "~/emacs.d/template.d/autoinsert" )
    ("autoinsert" "~/emacs.d/template.d/autoinsert")
    ("template" "~/emacs.d/template.d/template")))

(defun template-buffer ()
  (tmpldir )
  ())


;; auto-insert+-alist

;;;###autoload
(define-minor-mode auto-insert+-mode
  "Toggle auto-insert+ mode, a global minor mode.
With a prefix argument ARG, enable auto-insert+ mode if ARG is
positive, and disable it otherwise.  If called from Lisp, enable
the mode if ARG is omitted or nil.

When auto-insert+ mode is enabled, when new files are created you can
insert a template for the file depending on the mode of the buffer."
  :global t :group 'auto-insert+
  :init-value nil
  (if (and arg
           (if (> (prefix-numeric-value arg) 0)
               auto-insert+-mode (not auto-insert+-mode)))

      (add-hook 'find-file-hook 'auto-insert+)
      (remove-hook 'find-file-hook 'auto-insert+)))

(provide 'autoinsert+)

;;; autoinsert.el ends here
