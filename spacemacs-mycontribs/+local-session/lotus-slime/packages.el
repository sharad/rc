;;; packages.el --- lotus-slime layer packages file for Spacemacs.
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
;; added to `lotus-slime-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-slime/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-slime/pre-init-PACKAGE' and/or
;;   `lotus-slime/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-slimeS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-slime-packages
  '(
    ;; (PACKAGE :location local)
    slime
    )
  "The list of Lisp packages required by the lotus-slime layer.

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

(defun lotus-slime/post-init-slime ()
  (use-package slime
      :defer t
      :config
      (progn
        (setq inferior-lisp-program "sbcl"
              ;; I am setting up port other than 4005 so stumpwm will not be interfered with
              ;; other slime invokation.
              slime-port 4005
              slime-backend (get-slime-config 'slime-backend)
              slime-path (get-slime-config 'slime-path)
              swank-loader-full-path
              (if (file-name-absolute-p slime-backend)
                  slime-backend
                (expand-file-name slime-backend slime-path)))

        (setq w3m-command "/usr/bin/w3m" ; Ubuntu or Debian version
              cltl2-url "file:///usr/share/doc/cltl/clm/node1.html"
              hyperspec-prog (expand-file-name "hyperspec.el" slime-path)
              ;; hyperspec-path "/usr/share/doc/HyperSpec/")
              hyperspec-path "/usr/share/doc/hyperspec/")

        ;; settings for Common Lisp development:
        (setq lisp-indent-function 'common-lisp-indent-function
              ;slime-complete-symbol-function 'slime-fuzzy-complete-symbol
              slime-startup-animation nil
              common-lisp-hyperspec-root (concat "file://" hyperspec-path)
              common-lisp-hyperspec-symbol-table (concat hyperspec-path "Data/Map_Sym.txt")
              w3m-default-homepage common-lisp-hyperspec-root
              browse-url-browser-function 'w3m
              w3m-symbol 'w3m-default-symbol
              w3m-key-binding 'info
              w3m-coding-system 'utf-8
              w3m-default-coding-system 'utf-8
              w3m-file-coding-system 'utf-8
              w3m-file-name-coding-system 'utf-8
              w3m-terminal-coding-system 'utf-8

              ;; error in process filter: if: slime-eval-in-emacs disabled for
              ;; security.Set slime-enable-evaluate-in-emacs true to enable it.
              slime-enable-evaluate-in-emacs t)

        ;; Replace "sbcl" with the path to your implementation
        ;; (setq inferior-lisp-program "sbcl")

        ;; ("quicklisp-slime-helper")
        ;; CL-USER>
        )


      ;; (require 'slime)
      ;; next two lines only apply to Autumn 2007/post-2.0 versions of SLIME from CVS:
      ;; (slime-setup)


      ;; After starting SLIME, the commands of both packages should be available.

      ;; The REPL and slime-fancy modules deserve special mention. Many
      ;; users consider the REPL (see REPL) essential while slime-fancy (see
      ;; slime-fancy) loads the REPL and almost all of the popular
      ;; contribs. So, if you aren't sure what to choose start with:

      ;; (dolist (p '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf))
      ;;   (xrequire p))

      ;; (slime-setup '(inferior-slime slime-fancy slime-asdf))
      (progn
          ;; it is done in /etc/emacs/site-start.d/50slime.el
          ;; do not knows doing it second time would create some problem.
          ;; (slime-setup '(slime-repl inferior-slime slime-fancy slime-scratch slime-editing-commands slime-asdf))
          (slime-setup '(slime-fancy))
          ;; http://common-lisp.net/project/slime/doc/html/slime_002dautodoc_002dmode.html
          (slime-setup '(slime-autodoc)) ;; doc needed like eldoc
          ;; (slime-setup '(slime-fancy slime-scratch slime-editing-commands slime-repl slime-asdf))
          ;; (slime-setup '(slime-fancy slime-asdf slime-banner))
          ;; (slime-setup '(slime-asdf slime-banner))
          ;; (xrequire 'slime-repl)
          (setq slime-complete-symbol*-fancy t)
          (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol))

      (progn
        (defun slime-connect-ignore-version (&optional host port)
          (interactive)
          (let ((host (or host "localhost"))
                (port (or port 4005))
                (sharad-slime-ignore-version t))
            (setq sharad-slime-ignore-version t)
            (slime-connect host port))))

      (progn
        ;;{{
        ;; C-c C-d h slime-hyperspec-lookup SYMBOL-NAME
        ;; M-C-x evaluates current form via SLIME
        ;; M-C-q re-indents form following cursor
        ;; C-c C-d h on symbol or function retrieves HyperSpec entry
        ;; M-x describe-mode for more
        ;; http://cl-cookbook.sourceforge.net/emacs-ide.html

        (when nil
          (global-set-key [(shift f1)]
                          '(lambda ()
                             (interactive)
                             (let ((browse-url-browser-function
                                    'browse-url-w3)
                                   (common-lisp-hyperspec-root
                                    (concat "file://" hyperspec-path))
                                   (common-lisp-hyperspec-symbol-table
                                    (concat common-lisp-hyperspec-root
                                            "Data/Map_Sym.txt"))
                                   (hyperspec-prog (expand-file-name "hyperspec.el" slime-path)))
                               (load-library hyperspec-prog)
                               (common-lisp-hyperspec
                                (thing-at-point 'symbol))))))


        ;;}}
        )

      ;; Contrib packages aren't loaded by default. You have to modify
      ;; your setup a bit so that Emacs knows where to find them and which
      ;; of them to load. Generally, you should call slime-setup with the
      ;; list of package-names that you want to use. For example, a setup
      ;; to load the slime-scratch and slime-editing-commands packages
      ;; looks like:
      (when nil ;;; I do not need it. -thanks.
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        ;; quicklisp-slime-helper of qlisp installed
        ;; slime-helper.el installed in "/all/res/share/common-lisp/quicklisp/slime-helper.el"
        ;;
        ;; To use, add this to your ~/.emacs:
        ;;
        ;;   (load (expand-file-name "/all/res/share/common-lisp/quicklisp/slime-helper.el"))
        ;;   ;; Replace "sbcl" with the path to your implementation
        ;;   (setq inferior-lisp-program "sbcl")
        ;;
        ;;
        ;; ("quicklisp-slime-helper")

        ;; CL-USER>  (ql:quickload "quicklisp-slime-helper")
        ;; To load "quicklisp-slime-helper":
        ;;   Load 1 ASDF system:
        ;;     quicklisp-slime-helper
        ;; ; Loading "quicklisp-slime-helper"
        ;; ..................................................
        ;; [package quicklisp-slime-helper]
        ;; slime-helper.el installed in "/all/res/share/common-lisp/quicklisp/slime-helper.el"

        ;; To use, add this to your ~/.emacs:

        ;; uncomment here
        (let ((helper (expand-file-name (concat quicklisp-path "/slime-helper.el"))))
          (if (file-exists-p helper)
              (load helper))))

      (defun slime-proxy-setup ()
        (interactive)
        (slime-setup '(slime-proxy slime-parenscript)))))

;;; packages.el ends here
