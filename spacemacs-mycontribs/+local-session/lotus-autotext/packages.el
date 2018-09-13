;;; packages.el --- lotus-autotext layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-autotext-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-autotext/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-autotext/pre-init-PACKAGE' and/or
;;   `lotus-autotext/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-autotext-packages
  '(
    autoinsert+
    (template    :location local)
    (auto-yasnippet :location local)
    (template-simple :location local)
    yasnippet)
  "The list of Lisp packages required by the lotus-autotext layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-autotext/init-autoinsert+ ()
  (use-package autoinsert+
      :defer t
      ;; :init
      ;; (progn
      ;;   (auto-insert+-mode 1))
      :config
      (progn
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
            "=cut" "\n")))))

(defun lotus-annotation/init-PKG ()
  (use-package PKG
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-annotation/init-PKG ()
  (use-package PKG
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-annotation/init-PKG ()
  (use-package PKG
      :defer t
      :config
      (progn
        (progn
          ))))

;; (defun lotus-autotext/init-PACKAGE ()
;;   (use-package PACKAGE
;;       :defer t
;;       :config
;;       (progn
;;         )))

;; (defun lotus-autotext/init-PACKAGE ()
;;   (use-package PACKAGE
;;       :defer t
;;       :config
;;       (progn
;;         )))

;;; packages.el ends here
