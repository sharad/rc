;;
;; cedet.el
;; Login : <spratap@spratap>
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


(deh-require-maybe ede
  (global-ede-mode 1)                      ; Enable the Project management system
  ;; (global-srecode-minor-mode 1)            ; Enable template insertion menu
  )

(deh-require-maybe semantic
  (if (functionp 'semantic-load-enable-code-helpers)
      (semantic-load-enable-code-helpers)))      ; Enable prototype help and smart completion



(deh-section "ecb"
;;(when nil
  (deh-require-maybe semantic

    (deh-require-maybe imenu
      (defun my-semantic-hook ()
        ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html
        (imenu-add-to-menubar "TAGS"))
      (add-hook 'semantic-init-hooks 'my-semantic-hook))


    ;; http://alexott.net/en/writings/emacs-devenv/EmacsCedet.html#sec6

    (require 'semantic/bovine/gcc)

    (semantic-add-system-include "~/exp/include/boost_1_37" 'c++-mode)
    (setq-mode-local c-mode semanticdb-find-default-throttle
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
      (semantic-load-enable-primary-exuberent-ctags-support))



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



    (deh-section "ddd"

      (setq qt4-base-dir "/usr/include/qt4")
      (semantic-add-system-include qt4-base-dir 'c++-mode)
      (add-to-list 'auto-mode-alist (cons qt4-base-dir 'c++-mode))
      (when (boundp 'semantic-lex-c-preprocessor-symbol-file)
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig.h"))
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qconfig-dist.h"))
          (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat qt4-base-dir "/Qt/qglobal.h"))))



    (deh-require-maybe semantic/db-javap
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
    (add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
    ))


(deh-section "https://gist.github.com/alexott/3968635"
;;(when nil
  ;;; cedet-1.1-startup.el --- Working configuration for CEDET 1.1 & below

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
  )


(provide 'cedet-config)

