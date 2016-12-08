;; automatic text, at least adapt author!


;;{{ from: http://www.emacswiki.org/emacs/AutoInsertMode
;; I use Yasnippet for initial skeletons:

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
  (forward-line 2)
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


;; (deh-require-maybe  text-language
;;   (add-element-to-lists 'text-language-mode text-langs)
;;   (add-element-to-lists 'text-language-guess-mode text-langs))

;;;###autoload
(defun configuration|common|autotext-config|template|config ()
  (template-initialize)
  ;; to ignore
  (setq template-auto-update-disable-regexp "ido\\.last"))

;;;###autoload
(defun configuration|common|autotext-config|template|init ()
    (use-package template
      :defer t
      :config
      (configuration|common|autotext-config|template|config)))

;;;###autoload
(defun configuration|common|autotext-config|autoinsert+|config ()
  ;; (add-hook 'find-file-hooks 'auto-insert+)

  ;; (auto-insert+-mode 1)

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

  (define-auto-insert+ 'muse-mode "Muse Mode" "empty" :func 'insert-muse-file)


  ;; Make auto-insert+-alist element plist of :desc :cond :priority :actions
  ;; something like ((cond . (:desc :priority :actions)))
  ;; (define-auto-insert+ '("\\.*\\'" "All file")
  ;;     "template"
  ;;   [template-not-found-function])

  ;; from http://www.emacswiki.org/emacs/AutoInsertMode
  (define-auto-insert+
      "\\.\\(CC?\\|cc\\|cxx\\|cpp\\|c++\\)\\'"
      "C++ skeleton"
    "test"
    :skeleton
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


  (define-auto-insert+ "\\.c\\'" "C skeleton"
    "test"
    :skeleton
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


  (define-auto-insert+ 'perl-mode "Perl skeleton"
    "test"
    :skeleton
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
      "=cut" "\n"))
  )

;;;###autoload
(defun configuration|common|autotext-config|autoinsert+|init ()
    (use-package autoinsert+
      :defer t
      :config
      (configuration|common|autotext-config|autoinsert+|config))

    (auto-insert+-mode 1))

;;;###autoload
(defun configuration|common|autotext-config|auto-yasnippet|config ()
  ;;   (global-set-key (kbd "H-w") 'create-auto-yasnippet)
  ;;   (global-set-key (kbd "H-y") 'expand-auto-yasnippet)
  )

;;;###autoload
(defun configuration|common|autotext-config|auto-yasnippet|init ()
    (use-package auto-yasnippet
      :defer t
      :config
      (configuration|common|autotext-config|auto-yasnippet|config)))

;;;###autoload
(defun configuration|common|autotext-config|packages ()
  '(template
    template-simple
    autoinsert+
    yasnippet
    auto-yasnippet))

(provide 'autotext-config)
