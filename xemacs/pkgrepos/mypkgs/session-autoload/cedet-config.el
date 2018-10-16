;;
;; cedet.el
;; Login : <sh4r4d _at_ _G-mail_>
;; Login : <sh4r4d _at_ _G-mail_>
;; Started on  Mon Jul 26 15:53:21 2010 Sharad Pratap
;; $Id$
;;
;; Copyright (C) @YEAR@ Sharad Pratap
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA 02111-1307 USA
;;

(defvar configuration|common|cedet-config|package-list nil)

;;;###autoload
(defun configuration|common|cedet-config|ede|config ()
  (setq ede-project-placeholder-cache-file (auto-config-file "ede/ede-projects.el"))
  (global-ede-mode 1)                      ; Enable the Project management system
  ;; (global-srecode-minor-mode 1)            ; Enable template insertion menu
  ;; (ede-cpp-root-project "Test"
  ;;                       :name "Test Project"
  ;;                       :file "~/work/project/CMakeLists.txt"
  ;;                       :include-path '("/"
  ;;                                       "/Common"
  ;;                                       "/Interfaces"
  ;;                                       "/Libs"
  ;;                                       )
  ;;                       :system-include-path '("~/exp/include")
  ;;                       :spp-table '(("isUnix" . "")
  ;;                                    ("BOOST_TEST_DYN_LINK" . "")))
  ;;; ede customization
  (use-package global
      :defer t
      :config
      (global-ede-mode t))
  (use-package generic
      :defer t
      :config
      (ede-enable-generic-projects)))

;;;###autoload
(defun configuration|common|cedet-config|ede|init ()
    (use-package ede
      :defer t
      :config
      (configuration|common|cedet-config|ede|config)))
(push 'ede configuration|common|cedet-config|package-list)


;;;###autoload
(defun configuration|common|cedet-config|semantic|config ()
  ;; Enable prototype help and smart completion
  ;; https://gist.github.com/alexott/3968635
  (if (functionp 'semantic-load-enable-code-helpers)
      (semantic-load-enable-code-helpers))

  (use-package imenu
      :defer t
      :config
      (defun my-semantic-hook ()
        ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
        (imenu-add-to-menubar "TAGS"))
      ;; hooks, specific for semantic
      (add-hook 'semantic-init-hooks 'my-semantic-hook)
      (setq semantic-imenu-auto-rebuild-directory-indexes nil))


  ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec6

  (use-package semantic/bovine/gcc
      :defer t
      )

  (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)

  (use-package db-find
      :defer t
      :config
      (setq-mode-local c-mode semanticdb-find-default-throttle
                       '(project unloaded system recursive)))

  (use-package db-global
      :defer t
      :config
      ;; if you want to enable support for gnu global
      (when (and
             (fboundp 'cedet-gnu-global-version-check)
             (cedet-gnu-global-version-check t))
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode))
      )

  (use-package cedet
      :defer t
      :config
      ;; enable ctags for some languages:
      ;;  Unix Shell, Perl, Pascal, Tcl, Fortran, Asm
      (when (and
             (fboundp 'cedet-ectag-version-check)
             (cedet-ectag-version-check))
        (semantic-load-enable-primary-exuberent-ctags-support))
      )

  (progn
    (setq qt4-base-dir "/usr/include/qt4")
    (semantic-add-system-include qt4-base-dir 'c++-mode)
    (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
    (when (boundp 'semantic-lex-c-preprocessor-symbol-file)
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
      (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))))


  (progn
    (use-package semantic/db-javap
        :defer t
        :config
        (ede-java-root-project "TestProject"
                               :file "~/work/TestProject/build.xml"
                               :srcroot '("src" "test")
                               :localclasspath '("/relative/path.jar")
                               :classpath '("/absolute/path.jar")))

    (defun my-cedet-hook ()
      (local-set-key [(control return)] 'semantic-ia-complete-symbol)
      (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
      (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
      (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle))
    (add-hook 'c-mode-common-hook 'my-cedet-hook)


    (defun my-c-mode-cedet-hook ()
      (local-set-key "." 'semantic-complete-self-insert)
      (local-set-key ">" 'semantic-complete-self-insert))

    (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook))

  (progn

    ;; (deh-require-maybe (progn
    ;;                      semantic-decorate-include
    ;;                      semantic-gcc
    ;;                      semantic-ia
    ;;                      semantic/ia
    ;;                      eassist
    ;;                      semantic-lex-spp))
    ;; ;; (require 'semantic-decorate-include)
    ;; ;; (require 'semantic-gcc)
    ;; ;; (require 'semantic/ia)
    ;; ;; (require 'eassist)
    ;; ;; (require 'semantic-lex-spp)

    (if (fboundp 'semantic-load-enable-excessive-code-helpers)
        (semantic-load-enable-excessive-code-helpers))

    (deh-require-maybe semantic-util-modes
      (custom-set-variables
       '(semantic-idle-scheduler-idle-time 3)
       '(semantic-self-insert-show-completion-function
         (lambda nil (semantic-ia-complete-symbol-menu (point))))
       '(global-semantic-tag-folding-mode t nil (semantic-util-modes))))

    (setq senator-minor-mode-name "SN")

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
                     '(project unloaded system recursive)))

  ;; gnu global support
  (use-package global
      :defer t
      :config
      (when (and (fboundp 'cedet-gnu-global-version-check)
                 (cedet-gnu-global-version-check t))
        ;; (require 'semanticdb-global)
        (semanticdb-enable-gnu-global-databases 'c-mode)
        (semanticdb-enable-gnu-global-databases 'c++-mode)))

  (use-package semanticdb-ectag
      :defer t
      :config
      ;; ctags
      (when (and (fboundp 'cedet-ectag-version-check)
                 (cedet-ectag-version-check t))
        (require 'semanticdb-ectag)
        (semantic-load-enable-primary-exuberent-ctags-support))))


;;;###autoload
(defun configuration|common|cedet-config|semantic|init ()
    (use-package semantic
      :defer t
      :config
      (configuration|common|cedet-config|semantic|config)))
(push 'semantic configuration|common|cedet-config|package-list)





;;;###autoload
(defun configuration|common|cedet-config|cedet|config ()
  (use-package semantic
      :defer t
      :config
      (progn
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

        (use-package eassist
            :defer t
            :config
            (defun alexott/c-mode-cedet-hook ()
              (local-set-key "\C-ct" 'eassist-switch-h-cpp)
              (local-set-key "\C-xt" 'eassist-switch-h-cpp)
              (local-set-key "\C-ce" 'eassist-list-methods)
              (local-set-key "\C-c\C-r" 'semantic-symref))
            (add-hook 'c-mode-common-hook 'alexott/c-mode-cedet-hook))

        )))

;;;###autoload
(defun configuration|common|cedet-config|cedet|init ()
    (use-package cedet
      :defer t
      :config
      (configuration|common|cedet-config|cedet|config)))
(push 'cedet configuration|common|cedet-config|package-list)

(push 'imenu configuration|common|cedet-config|package-list)
(push 'global configuration|common|cedet-config|package-list)





;;;###autoload
(defun configuration|common|cedet-config|config ()
  configuration|common|cedet-config|package-list)
;;;###autoload
(defun configuration|common|cedet-config|init ()
  (configuration|common|cedet-config|config))

;;;###autoload
(defun configuration|common|cedet-config|packages ()
  configuration|common|cedet-config|package-list)



(provide 'cedet-config)
