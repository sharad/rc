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
    (comment-region (point-min) (+ (point-min) (length gpl))))
)



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
                auto-insert-alist)))




(deh-require-maybe template
  (template-initialize)

  (add-to-list
   ;; disable nagging update .ido.last
   'template-update-buffer-alist
   '(".ido.last" nil nil nil nil))

  (setq template-auto-update-disable-regexp "\\.ido\\.last"))



(provide 'autotext-config)

