;; automatic text, at least adapt author!
(defun start-latex () "Adds all that stuff to start a new LaTeX document" (interactive)
  (goto-char (point-min))
  (insert
"\\documentclass[a4paper,french]{article}
\\title{}
\\author{Jean-Baptiste Rouquier}
\\date{}

\\usepackage[french]{babel}    %
\\usepackage{indentfirst}      % comment
\\usepackage[latin1]{inputenc} % comment
\\usepackage[T1]{fontenc}      % comment
\\usepackage[pdftex]{graphicx}}
\\begin{document}
\\maketitle


")
  (goto-char (point-max))
  (insert "
\\end{document}
")
  (goto-char (point-min))
  (next-line 2)
  (backward-char 2)
  (latex-mode)
)

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

  (custom-set-variables
   '(auto-insert 'other)
   '(auto-insert-directory "~/autoinsert/")
   '(auto-insert-alist '((("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header") . ["template.h" c++-mode my/autoinsert-yas-expand])
                         (("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source") . ["template.cc" my/autoinsert-yas-expand])
                         (("\\.sh\\'" . "Shell script") . ["template.sh" my/autoinsert-yas-expand])
                         (("\\.el\\'" . "Emacs Lisp") . ["template.el" my/autoinsert-yas-expand])
                         (("\\.pl\\'" . "Perl script") . ["template.pl" my/autoinsert-yas-expand])
                         (("\\.pm\\'" . "Perl module") . ["template.pm" my/autoinsert-yas-expand])
                         (("\\.py\\'" . "Python script") . ["template.py" my/autoinsert-yas-expand])
                         (("[mM]akefile\\'" . "Makefile") . ["Makefile" my/autoinsert-yas-expand])
                         (("\\.tex\\'" . "TeX/LaTeX") . ["template.tex" my/autoinsert-yas-expand])))))
;;}}


(provide 'autotext-config)
