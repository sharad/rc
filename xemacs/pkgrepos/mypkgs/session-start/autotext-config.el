;; automatic text, at least adapt author!






(with-report-error "autotext"


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

  (require 'utils-config)



  (when nil
    (yas/expand-snippet "(defun $1 ()
  \"DOCSTRING\"
  (interactive)
  (let (var1)
    (setq var1 some)
    $0
  ))

(require $0 )" ))

  (defun my/autoinsert+-yas-expand ()
    "Replace text in yasnippet template."
    (yas/expand-snippet (buffer-string) (point-min) (point-max)))

  (messageto "*Complains*" "Do not overwrite default value of auto-insert-alist try "
                           "to integrate default value also.")

  (deh-require-maybe autoinsert+

    ;; (add-hook 'find-file-hooks 'auto-insert+)

    (auto-insert+-mode 1)

    (setq
     ;; auto-insert t
     auto-insert+-directory "~/.xemacs/template.d/autoinsert"
     template-directory-list '("~/.xemacs/pkgrepos/world/misc/pde/templates" "~/.templates/" "~/.xemacs/template.d/templates")
     auto-insert-query t
     auto-insert+ 'other)

    (defun insert-muse-file ()
      "Which files to insert to embed."
      (insert-file-contents ""))

    ;; auto-insert+-alist

    (setq auto-insert-alist
          (append '(((muse-mode .  "Muse Mode") . insert-muse-file))
                  auto-insert-alist))

    ;; (set-auto-insert+noaction '(".planner-registry.el\\'" . "Planner File"))
    (set-auto-insert+noaction '(".planner-registry.el\\'" . "Planner File"))

    (set-auto-insert+noaction '(".ido.last\\'" . "Ido save file name"))
    (set-auto-insert+noaction '(".gtags-dir-local.el\\'" . "Ido save file name"))
    (set-auto-insert+noaction '("diary/private\\'" . "Diary private file name"))

    (define-auto-insert+ '(muse-mode . "Muse Mode")
        "empty"
      'insert-muse-file)

    (define-auto-insert+ '("\\.\\([Hh]\\|hh\\|hpp\\)\\'" . "C / C++ header")
        "yastemp"
      ["template.h" c++-mode my/autoinsert+-yas-expand])

    (define-auto-insert+ '("\\.\\([C]\\|cc\\|cpp\\)\\'" . "C++ source")
        "yastemp"
      ["template.cc" my/autoinsert+-yas-expand])


    (define-auto-insert+ '("\\.sh\\'" . "Shell script")
        "yastemp"
      ["template.sh" my/autoinsert+-yas-expand])

    (define-auto-insert+ '("\\.pl\\'" . "Perl script")
        "yastemp"
      ["template.pl" my/autoinsert+-yas-expand])
    (define-auto-insert+ ' ("\\.pm\\'" . "Perl module")
        "yastemp"
      ["template.pm" my/autoinsert+-yas-expand])

    (define-auto-insert+ '("\\.py\\'" . "Python script")
        "yastemp"
      ["template.py" my/autoinsert+-yas-expand])

    (define-auto-insert+ '("[mM]akefile\\'" . "Makefile")
        "yastemp"
      ["Makefile" my/autoinsert+-yas-expand])

    (define-auto-insert+ '("\\.tex\\'" . "TeX/LaTeX")
        "yastemp"
      ["template.tex" my/autoinsert+-yas-expand])

    (define-auto-insert+ '("\\.el\\'" . "Emacs Lisp")
        "yastemp"
      ["template.el" my/autoinsert+-yas-expand])

    ;; Make auto-insert+-alist element plist of :desc :cond :priority :actions
    ;; something like ((cond . (:desc :priority :actions)))
    ;; (define-auto-insert+ '("\\.*\\'" . "All file")
    ;;     "template"
    ;;   [template-not-found-function])

    ;; from http://www.emacswiki.org/emacs/AutoInsertMode
    (define-auto-insert+
     '("\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'" . "C++ skeleton")
     "test"
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


    (define-auto-insert+ '("\\.c\\'" . "C skeleton")
     "test"
      '(
        "Short description: "
        "/**\n * "
        (file-name-nondirectory (buffer-file-name))
        " -- " str \n
        "*" \n
        "* Written on " (format-time-string "%A, %e %B %Y.") \n
        "*/" > \n \n
        "#include <stdio.h>" \n
        "#include \""
        (file-name-sans-extension
         (file-name-nondirectory (buffer-file-name)))
        ".h\"" \n \n
        "int main()" \n
        "{" > \n
        > _ \n
        "}" > \n))


    (define-auto-insert+ '(perl-mode . "Perl skeleton")
     "test"
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
       "=cut" "\n")))




  )
;;}}











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
)




;; (deh-require-maybe  text-language
;;   (add-element-to-lists 'text-language-mode text-langs)
;;   (add-element-to-lists 'text-language-guess-mode text-langs))



(provide 'autotext-config)
