;;; packages.el --- lotus-help layer packages file for Spacemacs.
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
;; added to `lotus-help-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-help/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-help/pre-init-PACKAGE' and/or
;;   `lotus-help/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-helpS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-help-packages
  '(
    pos-tip
    popup-pos-tip
    sdic-inline
    sdic-inline-pos-tip
    )
  "The list of Lisp packages required by the lotus-help layer.

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

(defun lotus-help/init-pos-tip ()
  (use-package pos-tip
      :defer t
      :config
      (progn
        ;; http://www.emacswiki.org/emacs/PosTip
        (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc3"
          (defun my-describe-function (function)
            "Display the full documentation of FUNCTION (a symbol) in tooltip."
            (interactive (list (function-called-at-point)))
            (if (null function)
                (pos-tip-show
                 "** You didn't specify a function! **" '("red"))
                (pos-tip-show
                 (with-temp-buffer
                   (let ((standard-output (current-buffer))
                         (help-xref-following t))
                     (prin1 function)
                     (princ " is ")
                     (describe-function-1 function)
                     (buffer-string)))
                 nil nil nil 0)))
          (define-key help-map (kbd ";") 'my-describe-function))



        (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc7"
          (defadvice popup-menu-show-quick-help
              (around pos-tip-popup-menu-show-quick-help () activate)
            "Show quick help using `pos-tip-show'."
            (if (eq window-system 'x)
                (let ((doc (popup-menu-document
                            menu (or item
                                     (popup-selected-item menu)))))
                  (when (stringp doc)
                    (pos-tip-show doc nil
                                  (if (popup-hidden-p menu)
                                      (or (plist-get args :point)
                                          (point))
                                      (overlay-end (popup-line-overlay
                                                    menu (+ (popup-offset menu)
                                                            (popup-selected-line menu)))))
                                  nil 0)
                    nil))
                ad-do-it)))

        (use-package popup-pos-tip
            :defer t
            :config
            (progn ;; "http://www.emacswiki.org/emacs/PosTip#toc8"
              (defadvice popup-tip
                  (around popup-pos-tip-wrapper (string &rest args) activate)
                (if (eq window-system 'x)
                    (apply 'popup-pos-tip string args)
                    ad-do-it))
              ))


        ;;(deh-require-maybe sdic-inline-pos-tip
        (when nil
          (deh-require-maybe sdic-inline
            (sdic-inline-mode t))

          (require 'sdic-inline-pos-tip)
          (deh-section "http://www.emacswiki.org/emacs/PosTip#toc8"
            ;; *Change the following lines according to your environment*
            (setq sdic-inline-eiwa-dictionary "/usr/share/dict/gene.sdic")
            (setq sdic-inline-waei-dictionary "/usr/share/dict/jedict.sdic")

            ;; The following is optional. Uncomment if necessary.
            ;; (mapc (lambda (mode)
            ;;         (add-to-list 'sdic-inline-enable-modes mode))
            ;;       '(help-mode Info-mode))
            ;; (mapc (lambda (face)
            ;;         (add-to-list 'sdic-inline-enable-faces face))
            ;;       '(font-lock-doc-face))

            (setq sdic-inline-search-func 'sdic-inline-search-word-with-stem)
            (setq sdic-inline-display-func 'sdic-inline-pos-tip-show)

            (define-key sdic-inline-map "\C-c\C-p" 'sdic-inline-pos-tip-show)

            (sdic-inline-mode 1))))))

(defun lotus-help/init-popup-pos-tip ()
  (use-package ample
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-help/init-sdic-inline ()
  (use-package ample
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-help/init-sdic-inline-pos-tip ()
  (use-package ample
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-LAYER/init-PKG ()
  (use-package ample
      :defer t
      :config
      (progn
        (progn
          ))))
;;; packages.el ends here
