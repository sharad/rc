;;; packages.el --- lotus-cedet layer packages file for Spacemacs.
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
;; added to `lotus-cedet-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-cedet/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-cedet/pre-init-PACKAGE' and/or
;;   `lotus-cedet/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-cedetS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-cedet-packages
  '(
    ecb
    ede
    semantic
    ;; semantic/bovine/gcc
    ;; semantic/db-javap
    imenu
    )
  "The list of Lisp packages required by the lotus-cedet layer.

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

(defun lotus-cedet/init-ecb ()
  (use-package ecb
      :defer t
      :config
      (progn
        (progn
          ;; Because ECB is a global minor-mode it can also be (de)activated/toggled by M-x ecb-minor-mode.

          ;; (add-element-to-lists 'ecb-minor-mode pgm-langs)
          ;; (add-element-to-lists 'ecb-toggle-ecb-windows pgm-langs)
          (setq
           ecb-auto-activate nil
           ecb-tip-of-the-day nil)

          (setq ecb-toggle-layout-sequence
                '("sharad-leftright-analyse-etc"
                  "sharad-leftright-analyse-etc-reverse"
                  ; "left9"
                  ; "left14"
                  ; "sharad-leftright-analyse"
                  )
                ecb-other-window-behavior 'smart ;'all
                )

        (with-eval-after-load "speedbar"
          (setq  ecb-use-speedbar-instead-native-tree-buffer nil))

        (with-eval-after-load "winring"
          (ecb-winman-winring-enable-support)
          (winring-initialize))))))

(defun lotus-cedet/init-ede ()
  (use-package ede
      :defer t
      :config
      (progn
        (progn
          (setq ede-project-placeholder-cache-file (auto-config-file "ede/ede-projects.el"))
          (global-ede-mode 1)                      ; Enable the Project management system
          ;; (global-srecode-minor-mode 1)            ; Enable template insertion menu
          )

        (progn ;; "https://gist.github.com/alexott/3968635"
          ;;(when nil
          ;; cedet-1.1-startup.el --- Working configuration for CEDET 1.1 & below

          ;; Copyright (C) Alex Ott
          ;;
          ;; Author: Alex Ott <alexott@gmail.com>
          ;; Keywords: CEDET 1.1,
          ;; Requirements: CEDET 1.1 or below
          (let ((cedet "~/tmp/cedet-1.1/common/cedet.el"))
            (when (file-exists-p cedet)
              (load-file cedet)))

          (deh-require-maybe (progn
                               semantic-decorate-include
                               semantic-gcc
                               semantic-ia
                               semantic/ia
                               eassist
                               semantic-lex-spp))
          ;; (require 'semantic-decorate-include)
          ;; (require 'semantic-gcc)
          ;; (require 'semantic/ia)
          ;; (require 'eassist)
          ;; (require 'semantic-lex-spp)

          (if (fboundp 'semantic-load-enable-excessive-code-helpers)
              (semantic-load-enable-excessive-code-helpers))

          (deh-require-maybe semantic-util-modes
            (custom-set-variables
             '(semantic-idle-scheduler-idle-time 3)
             '(semantic-self-insert-show-completion-function
               (lambda nil (semantic-ia-complete-symbol-menu (point))))
             '(global-semantic-tag-folding-mode t nil (semantic-util-modes))))

          (setq senator-minor-mode-name "SN")
          (setq semantic-imenu-auto-rebuild-directory-indexes nil)
          (when (fboundp 'global-srecode-minor-mode)
            (global-srecode-minor-mode 1))
          (global-semantic-mru-bookmark-mode 1)
          (when (fboundp 'global-semantic-tag-folding-mode)
            (global-semantic-tag-folding-mode 1))


          (setq-mode-local c-mode semanticdb-find-default-throttle
                           '(project unloaded system recursive))
          (setq-mode-local c++-mode semanticdb-find-default-throttle
                           '(project unloaded system recursive))
          (setq-mode-local erlang-mode semanticdb-find-default-throttle
                           '(project unloaded system recursive))

          ;; customisation of modes
          (defun alexott/cedet-hook ()
            (local-set-key [(control return)] 'semantic-ia-complete-symbol-menu)
            (local-set-key "\C-c?" 'semantic-ia-complete-symbol)
            ;;
            (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
            (local-set-key "\C-c=" 'semantic-decoration-include-visit)

            (local-set-key "\C-cj" 'semantic-ia-fast-jump)
            (local-set-key "\C-cq" 'semantic-ia-show-doc)
            (local-set-key "\C-cs" 'semantic-ia-show-summary)
            (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
            (local-set-key (kbd "C-c <left>") 'semantic-tag-folding-fold-block)
            (local-set-key (kbd "C-c <right>") 'semantic-tag-folding-show-block)
            )
          (add-hook 'c-mode-common-hook 'alexott/cedet-hook)
          (add-hook 'lisp-mode-hook 'alexott/cedet-hook)
          (add-hook 'scheme-mode-hook 'alexott/cedet-hook)
          (add-hook 'emacs-lisp-mode-hook 'alexott/cedet-hook)
          (add-hook 'erlang-mode-hook 'alexott/cedet-hook)

          (defun alexott/c-mode-cedet-hook ()
            (local-set-key "\C-ct" 'eassist-switch-h-cpp)
            (local-set-key "\C-xt" 'eassist-switch-h-cpp)
            (local-set-key "\C-ce" 'eassist-list-methods)
            (local-set-key "\C-c\C-r" 'semantic-symref)
            )
          (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook)

          ;; hooks, specific for semantic
          (defun alexott/semantic-hook ()
            (imenu-add-to-menubar "TAGS")
            )
          (add-hook 'semantic-init-hooks 'alexott/semantic-hook)

          ;; gnu global support

          (when (and (fboundp 'cedet-gnu-global-version-check)
                     (cedet-gnu-global-version-check t))
            (require 'semanticdb-global)
            (semanticdb-enable-gnu-global-databases 'c-mode)
            (semanticdb-enable-gnu-global-databases 'c++-mode))

          ;; ctags
          (when (and (fboundp 'cedet-ectag-version-check)
                     (cedet-ectag-version-check t))
            (require 'semanticdb-ectag)
            (semantic-load-enable-primary-exuberent-ctags-support))

  ;;; ede customization
          (global-ede-mode t)
          (ede-enable-generic-projects)

  ;;; cedet-1.1-startup.el ends here
          ))))

(defun lotus-cedet/post-init-semantic ()
  (use-package semantic
      :defer t
      :config
      (progn
        (progn
          ;; Enable prototype help and smart completion
          (if (functionp 'semantic-load-enable-code-helpers)
              (semantic-load-enable-code-helpers)))
        )))

(defun lotus-cedet/init-semantic/bovine/gcc ()
  ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec6
  (use-package semantic/bovine/gcc
      :defer t
      :config
      (progn
        (progn

          (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
          (setq-mode-local
           c-mode semanticdb-find-default-throttle
           '(project unloaded system recursive))


          ;; if you want to enable support for gnu global
          (when (and
                 (fboundp 'cedet-gnu-global-version-check)
                 (cedet-gnu-global-version-check t))
            (semanticdb-enable-gnu-global-databases 'c-mode)
            (semanticdb-enable-gnu-global-databases 'c++-mode))

          ;; enable ctags for some languages:
          ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
          (when (and
                 (fboundp 'cedet-ectag-version-check)
                 (cedet-ectag-version-check))
            (semantic-load-enable-primary-exuberent-ctags-support)))

        (progn
          (setq qt4-base-dir "/usr/include/qt4")
          (semantic-add-system-include qt4-base-dir 'c++-mode)
          (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
          (when (boundp 'semantic-lex-c-preprocessor-symbol-file)
            (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
            (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
            (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h")))))))

(defun lotus-cedet/init-semantic/db-javap ()
  (use-package semantic/db-javap
      :defer t
      :config
      (progn
        (progn
          (ede-java-root-project "TestProject"
                                 :file "~/work/TestProject/build.xml"
                                 :srcroot '("src" "test")
                                 :localclasspath '("/relative/path.jar")
                                 :classpath '("/absolute/path.jar"))))))

(defun lotus-cedet/post-init-imenu ()
  (use-package imenu
      :defer t
      :config
      (progn
        (with-eval-after-load "semantic"
          (defun my-semantic-hook ()
            ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
            (imenu-add-to-menubar "TAGS"))
          (add-hook 'semantic-init-hooks 'my-semantic-hook)))))

;;; packages.el ends here
