;;; packages.el --- lotus-math layer packages file for Spacemacs.
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
;; added to `lotus-math-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-math/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-math/pre-init-PACKAGE' and/or
;;   `lotus-math/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-mathS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-math-packages
  '(
    (maxima :location local)
    (imaxima :location local)
    gnuplot)
  "The list of Lisp packages required by the lotus-math layer.

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

(defun lotus-math/init-maxima ()
  (use-package maxima
      :defer t
      :config
      (progn
        ;; M-x imaxima
        ;; http://www.emacswiki.org/emacs/MaximaMode
        ;; (add-to-list 'load-path "/usr/local/share/maxima/5.18.1/emacs/")
        (autoload 'maxima-mode "maxima" "Maxima mode" t)
        (autoload 'maxima "maxima" "Maxima interaction" t)
        (autoload 'imath-mode "imath" "Imath mode for math formula input" t)
        (setq imaxima-use-maxima-mode-flag t)
        (add-to-list 'auto-mode-alist '("\\.ma[cx]" . maxima-mode)))))

(defun lotus-math/init-imaxima ()
  (use-package imaxima
      :defer t
      :config
      (progn
        (autoload 'imaxima "imaxima" "Frontend for maxima with Image support" t))))

(defun lotus-math/post-init-gnuplot ()
  (use-package gnuplot
      :defer t
      :config
      (progn
        ;; M-x gnuplot-make-buffer
        ;; http://www.emacswiki.org/emacs/GnuplotMode
        ;; see http://xafs.org/BruceRavel/GnuplotMode
        ;; see http://xafs.org/BruceRavel/GnuplotMode?action=AttachFile&do=view&target=gpelcard.pdf
        ;; this line automatically causes all files with the .gp extension to
        ;; be loaded into gnuplot mode
        (setq auto-mode-alist
              (append
               (list
                '("\\.gp$"  . gnuplot-mode)
                '("\\.plt$" . gnuplot-mode))
               auto-mode-alist))

        ;; if you have the latest win32 version of gnuplot
        (add-hook 'gnuplot-load-hook
                  '(lambda ()
                    ;; (setq gnuplot-gnuplot-buffer "plot.plt") ; name of a new gnuplot file
                    ;;; (setq show-trailing-whitespace t)
                    (setq whitespace-check-buffer-ateol t))))))

;;; packages.el ends here
