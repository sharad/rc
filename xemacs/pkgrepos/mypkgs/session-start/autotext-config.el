;; automatic text, at least adapt author!

(eval-after-load "autoinsert"
  ;; from http://www.emacswiki.org/emacs/AutoInsertMode
  '(progn
    (define-auto-insert
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     '("Short description: "
       "/*" \n
       (file-name-nondirectory (buffer-file-name))
       " -- " str \n
       " */" > \n \n
       "#include <iostream>" \n \n
       "using namespace std;" \n \n
       "main()" \n
       "{" \n
       > _ \n
       "}" > \n))


    (define-auto-insert '("\\.c\\'" . "C skeleton")
     '(
       "Short description: "
       "/**\n * "
       (file-name-nondirectory (buffer-file-name))
       " -- " str \n
       " *" \n
       " * Written on " (format-time-string "%A, %e %B %Y.") \n
       " */" > \n \n
       "#include <stdio.h>" \n
       "#include \""
       (file-name-sans-extension
        (file-name-nondirectory (buffer-file-name)))
       ".h\"" \n \n
       "int main()" \n
       "{" > \n
       > _ \n
       "}" > \n))


    (define-auto-insert '(perl-mode . "Perl skeleton")
     '("Description: "
       "#!/usr/bin/env perl" \n
       \n
       "use strict;" \n
       "use warnings;" \n \n
       _ \n \n
       "__END__" "\n\n"
       "=head1 NAME" "\n\n"
       str "\n\n"
       "=head1 SYNOPSIS" "\n\n\n"
       "=head1 DESCRIPTION" "\n\n\n"
       "=head1 COPYRIGHT" "\n\n"
       "Copyright (c) " (substring (current-time-string) -4) " "
       (getenv "ORGANIZATION") | (progn user-full-name) "\n\n"
       "This library is free software; you can redistribute it and/or" "\n"
       "modify it under the same terms as Perl itself." "\n\n"
       "=cut" "\n"))))



;; (deh-section "autoinsert"
(deh-require-maybe autoinsert

  ;; (dolist (f '(
  ;;              "/usr/share/emacs/23.1.50/lisp/autoinsert.elc"
  ;;              "/usr/share/emacs/23.2/lisp/autoinsert.elc"
  ;;              "/usr/share/emacs23/site-lisp/happycoders-emacs/autoinsert.elc"
  ;;              ; "/usr/share/xemacs21/xemacs-packages/lisp/text-modes/autoinsert.elc"
  ;;              "/usr/share/emacs-snapshot/site-lisp/happycoders-emacs/autoinsert.elc"
  ;;              ))
  ;;   (load f))

  (defun insert-muse-file ()
    "Which files to insert to embed."
    (insert-file-contents ""))

  (add-hook 'find-file-hooks 'auto-insert)
  (setq
   ;; auto-insert t
   auto-insert-directory "~/emacs.d/template.d/"
   auto-insert-query t)
  (setq auto-insert-alist
        (append '(((muse-mode .  "Muse Mode") . insert-muse-file))
                auto-insert-alist))
  (setq auto-insert-alist
        (append '(((".planner-registry.el\\'" . "Planner File") . nil)
                  ((".ido.last\\'" . "Planner File") . nil))
                auto-insert-alist))

  (deh-require-maybe auto-insert-choose)

  )




(deh-require-maybe template
  (template-initialize)

  (add-to-list
   ;; disable nagging update .ido.last
   ;; http://permalink.gmane.org/gmane.emacs.bugs/31777
   'template-update-buffer-alist
   '(".ido.last" nil nil nil nil))

  (setq template-auto-update-disable-regexp "\\.ido\\.last"))

(deh-require-maybe template-simple
  )

;;{{ from: http://www.emacswiki.org/emacs/AutoInsertMode
;; I use Yasnippet for initial skeletons:

;; (deh-require-todo yasnippet
(deh-require-maybe yasnippet

;;  (messageto "*Complains*" "Install yasnippet for good experience")

  (defun my/autoinsert-yas-expand ()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))

  (messageto "*Complains*" "Do not overwrite default value of auto-insert-alist try "
                           "to integrate default value also.")

  (deh-require-maybe auto-insert-choose)


  (setq auto-insert-c-header-alist)


  (custom-set-variables
   '(auto-insert 'other)
   '(auto-insert-directory "~/autoinsert/")
   '(auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
                         ;; (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])

                         (("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
                          (lambda nil (auto-insert-choose-and-call auto-insert-c-header-alist)))


                         (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                         (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
                         (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
                         (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
                         (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
                         (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand])

                         (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand



                                                         ])



                         ))))
;;}}






((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
  (upcase
   (concat
    (file-name-nondirectory
     (file-name-sans-extension buffer-file-name))
    "_"
    (file-name-extension buffer-file-name)))
  "#ifndef " str n "#define " str "\n\n" _ "\n\n#endif")
 (("\\.\\([Cc]\\|cc\\|cpp\\)\\'" . "C / C++ program")
  nil "#include \""
  (let
      ((stem
        (file-name-sans-extension buffer-file-name)))
    (cond
      ((file-exists-p
        (concat stem ".h"))
       (file-name-nondirectory
        (concat stem ".h")))
      ((file-exists-p
        (concat stem ".hh"))
       (file-name-nondirectory
        (concat stem ".hh")))))
  & 34 | -10)
 (("[Mm]akefile\\'" . "Makefile")
  . "makefile.inc")
 (html-mode lambda nil
            (sgml-tag "html"))
 (plain-tex-mode . "tex-insert.tex")
 (bibtex-mode . "tex-insert.tex")
 (latex-mode "options, RET: " "\\documentclass[" str & 93 | -1 123
             (read-string "class: ")
             "}\n"
             ("package, %s: " "\\usepackage["
                              (read-string "options, RET: ")
                              & 93 | -1 123 str "}\n")
             _ "\n\\begin{document}\n" _ "\n\\end{document}")
 (("/bin/.*[^/]\\'" . "Shell-Script mode magic number")
  lambda nil
  (if
   (eq major-mode
       (default-value 'major-mode))
   (sh-mode)))
 (ada-mode . ada-header)
 (("\\.[1-9]\\'" . "Man page skeleton")
  "Short description: " ".\\\" Copyright (C), "
  (substring
   (current-time-string)
   -4)
  "  "
  (getenv "ORGANIZATION")
  |
  (progn user-full-name)
  "\n.\\\" You may distribute this file under the terms of the GNU Free\n.\\\" Documentation License.\n.TH "
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name)))
  " "
  (file-name-extension
   (buffer-file-name))
  " "
  (format-time-string "%Y-%m-%d ")
  "\n.SH NAME\n"
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name)))
  " \\- " str "\n.SH SYNOPSIS\n.B "
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name)))
  "\n" _ "\n.SH DESCRIPTION\n.SH OPTIONS\n.SH FILES\n.SH \"SEE ALSO\"\n.SH BUGS\n.SH AUTHOR\n"
  (user-full-name)
  '(if
    (search-backward "&"
     (line-beginning-position)
     t)
    (replace-match
     (capitalize
      (user-login-name))
     t t))
  '(end-of-line 1)
  " <"
  (progn user-mail-address)
  ">\n")
 (("\\.el\\'" . "Emacs Lisp header")
  "Short description: " ";;; "
  (file-name-nondirectory
   (buffer-file-name))
  " --- " str "\n\n;; Copyright (C) "
  (substring
   (current-time-string)
   -4)
  "  "
  (getenv "ORGANIZATION")
  |
  (progn user-full-name)
  "\n\n;; Author: "
  (user-full-name)
  '(if
    (search-backward "&"
     (line-beginning-position)
     t)
    (replace-match
     (capitalize
      (user-login-name))
     t t))
  '(end-of-line 1)
  " <"
  (progn user-mail-address)
  ">\n;; Keywords: "
  '(require 'finder)
  '(setq v1
    (mapcar
     (lambda
         (x)
       (list
        (symbol-name
         (car x))))
     finder-known-keywords)
    v2
    (mapconcat
     (lambda
         (x)
       (format "%12s:  %s"
               (car x)
               (cdr x)))
     finder-known-keywords "\n"))
  ((let
       ((minibuffer-help-form v2))
     (completing-read "Keyword, C-h: " v1 nil t))
   str ", ")
  & -2 "\n\n;; This program is free software; you can redistribute it and/or modify\n;; it under the terms of the GNU General Public License as published by\n;; the Free Software Foundation, either version 3 of the License, or\n;; (at your option) any later version.\n\n;; This program is distributed in the hope that it will be useful,\n;; but WITHOUT ANY WARRANTY; without even the implied warranty of\n;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the\n;; GNU General Public License for more details.\n\n;; You should have received a copy of the GNU General Public License\n;; along with this program.  If not, see <http://www.gnu.org/licenses/>.\n\n;;; Commentary:\n\n;; " _ "\n\n;;; Code:\n\n\n\n(provide '"
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name)))
  ")\n;;; "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here\n")
 (("\\.texi\\(nfo\\)?\\'" . "Texinfo file skeleton")
  "Title: " "\\input texinfo   @c -*-texinfo-*-\n@c %**start of header\n@setfilename "
  (file-name-sans-extension
   (file-name-nondirectory
    (buffer-file-name)))
  ".info\n" "@settitle " str "\n@c %**end of header\n@copying\n"
  (setq short-description
        (read-string "Short description: "))
  ".\n\n" "Copyright @copyright{} "
  (substring
   (current-time-string)
   -4)
  "  "
  (getenv "ORGANIZATION")
  |
  (progn user-full-name)
  "\n\n@quotation\nPermission is granted to copy, distribute and/or modify this document\nunder the terms of the GNU Free Documentation License, Version 1.3\nor any later version published by the Free Software Foundation;\nwith no Invariant Sections, no Front-Cover Texts, and no Back-Cover Texts.\nA copy of the license is included in the section entitled ``GNU\nFree Documentation License''.\n\nA copy of the license is also available from the Free Software\nFoundation Web site at @url{http://www.gnu.org/licenses/fdl.html}.\n\n@end quotation\n\nThe document was typeset with\n@uref{http://www.texinfo.org/, GNU Texinfo}.\n\n@end copying\n\n@titlepage\n@title " str "\n@subtitle " short-description "\n@author "
  (getenv "ORGANIZATION")
  |
  (progn user-full-name)
  " <"
  (progn user-mail-address)
  ">\n@page\n@vskip 0pt plus 1filll\n@insertcopying\n@end titlepage\n\n@c Output the table of the contents at the beginning.\n@contents\n\n@ifnottex\n@node Top\n@top " str "\n\n@insertcopying\n@end ifnottex\n\n@c Generate the nodes for this menu with `C-c C-u C-m'.\n@menu\n@end menu\n\n@c Update all node entries with `C-c C-u C-n'.\n@c Insert new nodes with `C-c C-c n'.\n@node Chapter One\n@chapter Chapter One\n\n" _ "\n\n@node Copying This Manual\n@appendix Copying This Manual\n\n@menu\n* GNU Free Documentation License::  License for copying this manual.\n@end menu\n\n@c Get fdl.texi from http://www.gnu.org/licenses/fdl.html\n@include fdl.texi\n\n@node Index\n@unnumbered Index\n\n@printindex cp\n\n@bye\n\n@c "
  (file-name-nondirectory
   (buffer-file-name))
  " ends here\n"))






(defun start-latex () "Adds all that stuff to start a new LaTeX document" (interactive)
  (goto-char (point-min))
  (insert
   (concat
    "\\documentclass[a4paper,french]{article}\n"
    "\\title{}\n"
    "\\author{Jean-Baptiste Rouquier}\n"
    "\\date{}\n\n"
    "\\usepackage[french]{babel}    %\n"
    "\\usepackage{indentfirst}      % comment\n"
    "\\usepackage[latin1]{inputenc} % comment\n"
    "\\usepackage[T1]{fontenc}      % comment\n"
    "\\usepackage[pdftex]{graphicx}}\n"
    "\\begin{document}\n"
    "\\maketitle\n\n\n\n"))
  (goto-char (point-max))
  (insert "\n\\end{document}\n")
  (goto-char (point-min))
  (next-line 2)
  (backward-char 2)
  (latex-mode))

(defun add-gpl () "Adds the GPL statements at the beginning of the file" (interactive)
  (let ((comment-style 'box)
        (gpl
         (concat
"This program is free software; you can redistribute it and/or
modify it under the terms of the GNU General Public License as
published by the Free Software Foundation\; either version 2 of the
License, or (at your option) any later version.

This program is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY\; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.
"
                 (if (boundp 'user-full-name)
                     (concat "\nWritten and (c) by " user-full-name "\n")
                     "")
;;                  (if (boundp 'user-mail-address) (concat
;;                                                   "Contact <"
;;                                                   user-mail-address
;;                                                   "> for comment & bug reports\n")
;;                    "")
                 )))

    (goto-char (point-min))
    (insert gpl)
    (comment-region (point-min) (+ (point-min) (length gpl)))))

(provide 'autotext-config)
