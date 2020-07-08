;;; packages.el --- LAYER lotus-lang packages file for Spacemacs.
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
;; added to `lotus-lang-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-lang/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-lang/pre-init-PACKAGE' and/or
;;   `lotus-lang/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-lang-packages
  '(pgm-utils
    (elisp-mode :location local)
    (ispell    :locaion local)
    geiser
    tuareg
    merlin)


  "The list of Lisp packages required by the lotus-lang layer.

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

(defun lotus-lang/init-pgm-utils ()
  (use-package pgm-utils
    :defer t
    :init
    (progn
      (add-hook 'prog-mode-hook 'office-activate)
      (add-hook 'nxml-mode-hook 'office-activate))
    :config
    (progn
      (progn
        (setq office-git-remote-regex "fortinet")))))

(defun lotus-lang/init-elisp-mode ()
  (use-package elisp-mode
    :defer t
    :config
    (progn
      (progn
        (add-hook 'emacs-lisp-mode-hook
                  #'(lambda ()
                      (make-local-variable 'lisp-indent-function)
                      (setq lisp-indent-function #'lisp-indent-function)))))))

(defun lotus-lang/init-ispell ()
  (use-package ispell
    :defer t
    :config
    (progn
      (progn))))
        ;; (setq ispell-aspell-dict-dir "/run/current-system/profile/lib/aspell/")
        ;; ;; https://emacs.stackexchange.com/questions/38162/ispell-cant-find-my-dictionary
        ;; (setq ispell-aspell-data-dir ispell-aspell-dict-dir)

(defun lotus-lang/post-init-geiser ()
  (use-package geiser-impl
    :defer t
    :config
    (progn
      (progn
        ;; check (describe-variable 'geiser-active-implementations)
        (setq geiser-default-implementation 'guile)))))

(defun lotus-lang/post-init-tuareg ()
  (use-package tuareg
    :defer t
    :config
    (progn
      (progn
        (setq tuareg-indent-align-with-first-arg t)
        (setq tuareg-match-patterns-aligned t))
      (progn
        (add-hook 'tuareg-mode-hook
                  #'(lambda()
                      (when (functionp 'prettify-symbols-mode)
                        (prettify-symbols-mode))
                      (setq mode-name "🐫")))))))

(defun lotus-lang/post-init-merlin ()
  (use-package merlin
    :defer t
    :config
    (progn
      (progn
        (defun merlin-command ()
          (executable-find "ocamlmerlin")))
      (progn))))




;;; packages.el ends here
