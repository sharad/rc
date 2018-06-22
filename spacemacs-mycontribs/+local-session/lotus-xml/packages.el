;;; packages.el --- lotus-xml layer packages file for Spacemacs.
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
;; added to `lotus-xml-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-xml/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-xml/pre-init-PACKAGE' and/or
;;   `lotus-xml/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-xml-packages
  '(
    (nxml-mode :location local)
    xquery-mode
    (xslt-process :location local)
    (xslide :location local)
    (xslide-process :location local)
    )
  "The list of Lisp packages required by the lotus-xml layer.

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

(defun lotus-xml/init-nxml-mode ()
  (use-package nxml-mode
    :defer t
    :config
    (progn
      (progn
        ;; #16449 - 24.3.50; emacs hangs while deleting comment in xml file with flyspell-mode on - GNU bug report logs
        ;; https://debbugs.gnu.org/cgi/bugreport.cgi?bug=16449
        (add-hook 'nxml-mode-hook
                  (function
                   (lambda ()
                    (flyspell-mode-off)
                    (turn-off-show-smartparens-mode)
                    (turn-off-smartparens-strict-mode)
                    (turn-off-smartparens-mode)))))
      (progn

        (setq
         ;; Emacs - nxhtml-mode - memory full - Stack Overflow
         ;; https://stackoverflow.com/questions/11247666/emacs-nxhtml-mode-memory-full/11409099#11409099
         ;; http://stackoverflow.com/a/11409099
         rng-nxml-auto-validate-flag nil

         nxml-slash-auto-complete-flag t)

        (setq nxml-child-indent 2))

      (progn
        ;;TODO
        ;;Showing XPath in modeline
        ;;Commenting comments
        ;;from: https://www.emacswiki.org/emacs/NxmlMode
        )

      (progn
        (use-package xslt-process
            :defer t
            :config
            ;; (require 'string)
            (autoload 'xslt-process-mode "xslt-process" "Emacs XSLT processing" t)
            (autoload 'xslt-process-install-docbook "xslt-process"
              "Register the DocBook package with XSLT-process" t)
            (add-hook 'sgml-mode-hook 'xslt-process-mode)
            (add-hook 'xml-mode-hook 'xslt-process-mode)
            (add-hook 'xsl-mode-hook 'xslt-process-mode)

            (defadvice xml-mode (after run-xml-mode-hooks act)
              "Invoke `xml-mode-hook' hooks in the XML mode."
              (run-hooks 'xml-mode-hook)))))))

(defun lotus-xml/init-xquery-mode ()
  (use-package xquery-mode
    :defer t
    :config
    (progn
      )))

(defun lotus-xml/init-xslt-process ()
  (use-package xslt-process
    :defer t
    :config
    (progn
      )))

(defun lotus-xml/init-xslide ()
  (use-package xslide
    :defer t
    :config
    (progn
      (use-package xslide
        :defer t
        :config
        ;; XSL mode
        (autoload 'xsl-mode "xslide" "Major mode for XSL stylesheets." t)
        ;; (autoload 'xsl-mode "xslide2" "Major mode for XSL stylesheets." t)
        ;; Uncomment if you want to use `xsl-grep' outside of XSL files.
        (autoload 'xsl-grep "xslide" "Grep for PATTERN in files matching FILESPEC." t)))))

(defun lotus-xml/init-xslide-process ()
  (use-package xslide-process
    :defer t
    :config
    (progn
      (use-package xslide-process
        :defer t
        :config
        ;; Uncomment if you want to use `xslide-process' in `xml-mode'.
        (autoload 'xsl-process "xslide-process" "Process an XSL stylesheet." t)))))

;; (defun lotus-xml/init-PACKAGE ()
;;   (use-package PACKAGE
;;     :defer t
;;     :config
;;     (progn
;;       )))

;;; packages.el ends here
