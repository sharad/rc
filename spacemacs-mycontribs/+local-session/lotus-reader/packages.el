;;; packages.el --- lotus-reader layer packages file for Spacemacs.
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
;; added to `lotus-reader-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-reader/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-reader/pre-init-PACKAGE' and/or
;;   `lotus-reader/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-readerS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-reader-packages
  '(
    (reader-mode :location local)
    page-ext
    files)
  "The list of Lisp packages required by the lotus-reader layer.

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

(defun lotus-reader/init-reader-mode ()
  (use-package reader-mode
      :defer t
      :config
      (progn
        (add-element-to-lists
         #'(lambda ()
             (reader-mode 1))
         reader-requester))))

(defun lotus-reader/init-page-ext ()
  (use-package page-ext
    :defer t
    :config
    (progn
      (progn ;; "pb"
        (defun page-break-xx-display-table (window)
          "Create a display-table that displays page-breaks prettily."
          (let ((table (or (copy-sequence (window-display-table window))
                           (make-display-table))))
            (aset table ?\^L
                  "abc")
            table))

        (defun set-xx-table (window)
          (set-window-display-table window
                                    (page-break-xx-display-table window)))

        (defun page-break-xx-mode-hook-function  ()
          (interactive)
          "Function called for updating display table"
          (mapcar 'set-xx-table
                  (window-list nil 'no-minibuffer))))

      (progn ;; "page break"
        ;; http://www.emacswiki.org/emacs/PageBreaks
        (defvar page-break-face 'bold)
        (defvar page-break-string-char ?-)

        (defun page-break-display-table (window)
          "Create a display-table that displays page-breaks prettily."
          (let ((table (or (copy-sequence (window-display-table window))
                           (make-display-table))))
            (aset table ?\^L
                  (let ((face-offset (lsh (face-id page-break-face) 19)))
                    (vconcat (mapcar (lambda (c) (+ face-offset c))
                                     (make-string (1- (window-width window))
                                                  page-break-string-char)))))
            table))

        ;; http://ergoemacs.org/emacs/modernization_formfeed.html
        ;; "§ ────────── ────────── ────────── ────────── ──────────"

        (defun page-break-mode-hook-function  ()
          "Function called for updating display table"
          (mapcar (lambda (window)
                    (set-window-display-table window
                                              (page-break-display-table window)))
                  (window-list nil 'no-minibuffer)))

        (define-minor-mode page-break-mode
          "Toggle Page Break mode.

In Page Break mode, page breaks (^L characters) are displayed as a
horizontal line of `page-break-string-char' characters."
          :global t
          :lighter " Pgbrk"
          (if page-break-mode
              (add-hook 'window-configuration-change-hook
                        'page-break-mode-hook-function )
            (remove-hook 'window-configuration-change-hook
                         'page-break-mode-hook-function)))

        (defun turn-on-page-break-mode ()
          (page-break-mode 1))

        (defun turn-off-page-break-mode ()
          (page-break-mode -1))

        ;; (turn-on-page-break-mode)
        ;; (turn-off-page-break-mode)
        ))))

(defun lotus-reader/post-init-files ()
  (use-package files
      :defer t
      :config
      (progn
        (progn
          (setq
           view-read-only t
           )))))

;;; packages.el ends here
