;;; packages.el --- lotus-perl layer packages file for Spacemacs.
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
;; added to `lotus-perl-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-perl/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-perl/pre-init-PACKAGE' and/or
;;   `lotus-perl/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-perlS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-perl-packages
  '(
    ;; (PACKAGE :location local)
    cperl-mode
    (compile-dwim :location local)
    (pde :location local)
    (pde-load :location local)
    comint)
  "The list of Lisp packages required by the lotus-perl layer.

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

(defun lotus-perl/post-init-cperl-mode () ;core-mod
  ;; ref: http://search.cpan.org/src/YEWENBIN/PDE-v0.2.13/lisp/doc/QuickStartEn.html
  ;; Configuration in .emacs2

  ;; Editing
  ;; cperl-mode provides an excellent environment for perl
  ;; programming. With a good style, the indentation is fine. I recommend
  ;; the setting in PBP.

  ;; If you want reindent the code, using M-x indent-region when region is
  ;; selected. I recommend using pde-indent-dwim, it saves you time to
  ;; markup region.

  ;; Comment or uncomment code using M-x comment-dwim. When region
  ;; selected, if there are line uncommented, then comment the region,
  ;; otherwise uncomment the region. If no region selected and have
  ;; non-whitspace characters in current line, it will add comment at the
  ;; end of the line. If in empty line or only whitespace in current line,
  ;; it will add new comment. If there is comment already, jump to the
  ;; position where comment starts. With C-u prefix, it can delete comment
  ;; in current line. So with this command, all jobs about comment are
  ;; done.

  ;; Most editor provide feature about code-snippet. In emacs, you can use
  ;; abbrev. First you should turn on abbrev-mode. cperl-mode provides some
  ;; useful abbrevs, for example after keywords if, else, elsif, while,
  ;; for, foreach, unless and until. Also for pod items =head11, =over,
  ;; =head2, =pod.

  ;; Emacs code completion is not as gaudy as other editors, but it
  ;; provides more ways and high customization for completion. The most
  ;; useful commands for completion are hippie-expand and
  ;; dabbrev-expand. They can do almost all kinds of completion. In my
  ;; experience, I use hippie-expand to complete whole line and
  ;; dabbrev-expand to complete words. Emacs doesn't provide semantic
  ;; analysis for perl, so there is no exists way for code intelligent
  ;; completion. Although hippie-expand provide completion for file name,
  ;; its priority is too low to reach. So you may need addtional command
  ;; for file name completion when editing. I recommend use
  ;; comint-dynamic-complete.
  ;; Command
  ;;     * C-M-\ indent-region
  ;;     * C-M-= pde-indent-dwim
  ;;     * C-; comment-dwim
  ;;     * M-/ dabbrev-expand
  ;;     * M-; hippie-expand
  ;;     * C-c f comint-dynamic-complete
  ;;     * M-' just-one-space
  ;;     * M-\ delete-horizontal-space
  ;;     * C-M-a beginning-of-defun
  ;;     * C-M-e end-of-defun
  ;;     * C-M-f forward-sexp
  ;;     * C-M-b backward-sexp
  ;;     * C-M-u backward-up-list
  ;;     * M-{ backward-paragraph
  ;;     * M-} forward-paragraph
  ;;     * M-h mark-paragraph
  ;;     * C-M-h mark-defun
  ;;     * C-x h mark-whole-buffer

  ;; END: Editing
  (use-package cperl-mode
               :defer t
               :config
               (progn
                 (defalias 'perl-mode 'cperl-mode)
                 (defun pde-perl-mode-hook ()
                   (abbrev-mode t)
                   (add-to-list 'cperl-style-alist
                                '("PDE"
                                  (cperl-auto-newline                         . t)
                                  (cperl-brace-offset                         . 0)
                                  (cperl-close-paren-offset                   . -4)
                                  (cperl-continued-brace-offset               . 0)
                                  (cperl-continued-statement-offset           . 4)
                                  (cperl-extra-newline-before-brace           . nil)
                                  (cperl-extra-newline-before-brace-multiline . nil)
                                  (cperl-indent-level                         . 4)
                                  (cperl-indent-parens-as-block               . t)
                                  (cperl-label-offset                         . -4)
                                  (cperl-merge-trailing-else                  . t)
                                  (cperl-tab-always-indent                    . t)))
                   (cperl-set-style "PDE")
                   ;; chmod when saving
                   (when (and
                          buffer-file-name
                          (not (string-match "\\.\\(pm\\|pod\\)$" (buffer-file-name))))
                     (add-hook 'after-save-hook 'executable-chmod nil t))
                   (set (make-local-variable 'compile-dwim-check-tools) nil)

                   ;; A wonderful minibuffer completion mode
                   (if (functionp 'partial-completion-mode)
                       (partial-completion-mode 1))
                   (setq cperl-lazy-help-time 2)
                   (cperl-lazy-install)

                   ;; Interactive shell

                   ;; A striking feature of dynamic language is it provides eval
                   ;; function. The eval function make it possible to build a huge project
                   ;; like building blocks. `perl -de 1' is good candicate for interactive
                   ;; shell. But PDE provides another interactive shell for easy
                   ;; customization and extending, and echo result of eval.

                   ;; (autoload 'run-perl "inf-perl" "Start perl interactive shell" t)
                   ;; Commands
                   ;;     * run-perl
                   ;;     * inf-perl-send
                   ;;     * inf-perl-send-line
                   ;;     * inf-perl-send-region
                   ;;     * inf-perl-load-file

                   ;; END: Interactive shell
                   ;; Debugger

                   ;; gud of emacs supports perldb, but it is simple and crude compare with
                   ;; gdb-ui. PDE provides a similar interface like gdb-ui. But it is known
                   ;; not stable. You may have a try.

                   ;; Perl Debugger

                   ;; perl5db provides a convenient command line interface, and maybe it
                   ;; more quick to use command than using emacs command. Remember perl5db
                   ;; commands is worth if you want to debug perl script. Except n, s, c, b,
                   ;; B, w, W, L, p, x these most used commands, these are also convenient:

                   ;;     * f Switch to a loaded file
                   ;;     * l Jump to line or function
                   ;;     * . Back to current line
                   ;;     * r Return from current subroutine
                   ;;     * V Search for variable
                   ;;     * S Search for function
                   ;;     * y List lexcial variables

                   ;; (autoload 'perldb-ui "perldb-ui" "perl debugger" t)

                   ;; Commands
                   ;;     * perldb-ui
                   ;;     * perldb-many-windows
                   ;;     * perldb-restore-windows


                   ;; Syntax Checking and Running
                   ;; cperl-mode doesn't provide command for syntax checking and running. It
                   ;; did put them on menu. Activating them requires mode-compile. But I
                   ;; don't like mode-compile, it makes simple problem complicated. I like
                   ;; smark-compile+. But customizing and extending smart-compile+ is not
                   ;; convenient, so I rewrite the library to compile-dwim. You can use
                   ;; compile-dwim-compile for syntax checking and compile-dwim-run to run
                   ;; the script.

                   ;; If you have experience of GUI programming, a problem may raise when
                   ;; running several programs at the same time. compile asks you to kill
                   ;; previous process before the new one starts. If you think this is not
                   ;; convenient, you can set compilation-buffer-name-function to
                   ;; pde-compilation-buffer-name.

                   ;; Emacs provides flymake to do on-the-fly syntax checking. in my humble
                   ;; opinion, it is not very useful. But you can turn it on if you want
                   ;; emacs seems more like IDE.

                   ;; For perl novice, it is common forgeting chmod before running perl
                   ;; script. The library executable provide a solution for automatic chmod
                   ;; when saving file.
                   (use-package pde
                                :defer t
                                :config
                                (progn
                                  (add-hook 'cperl-mode-hook
                                            '(lambda ()
                                              (make-local-variable 'compilation-buffer-name-function)
                                              (setq
                                               compilation-buffer-name-function
                                               'pde-compilation-buffer-name)))))

                   ;; Commands
                   ;;     * C-c s compile-dwim-compile
                   ;;     * C-c r compile-dwim-run
                   ;;     * compile
                   ;;     * flymake-mode
                   ;;     * executable-set-magic


                   ;; END: Syntax Checking and Running
                   )



                 (add-hook 'cperl-mode-hook
                           '(lambda ()
                             (pde-perl-mode-hook)
                             (setq indent-tabs-mode nil))
                           )

                 (autoload 'executable-chmod "executable"
                           "Make sure the file is executable.")

                 (use-package comint
                              :defer t)
                 (use-package inf-perl
                              :defer t)
                 (use-package perldb-ui
                              :defer t)
                 (use-package compile-dwim
                              :defer t)
                 (use-package imenu
                              :defer t
                              :config
                              (progn
                                (defalias 'imenu--completion-buffer 'pde-ido-imenu-completion)))
                 (autoload 'comint-dynamic-complete "comint" "Complete for file name" t)
                 (autoload 'run-perl "inf-perl" "Start perl interactive shell" t)
                 (autoload 'perldb-ui "perldb-ui" "perl debugger" t)
                 (autoload 'compile-dwim-run "compile-dwim" "Build and run" t)
                 (autoload 'compile-dwim-compile "compile-dwim" "Compile or check syntax" t)
                 (autoload 'imenu-tree "imenu-tree" "Show imenu tree" t))))

(defun lotus-perl/init-compile-dwim ()
  (use-package compile-dwim
      :defer t
      :config
      (progn
        )))

(defun lotus-perl/init-pde ()
  (use-package pde
      :defer t
      :config
      (progn
        )))

(defun lotus-perl/init-pde-load ()
  (use-package pde-load
      :defer t
      :config
      (progn
        )))

(defun lotus-perl/post-init-comint ()
  (use-package comint
      :defer t
      :config
      (progn
        (setq comint-completion-addsuffix '("/" . "")))))




;;; packages.el ends here
