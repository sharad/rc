;;; packages.el --- lotus-flymake layer packages file for Spacemacs.
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
;; added to `lotus-flymake-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-flymake/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-flymake/pre-init-PACKAGE' and/or
;;   `lotus-flymake/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-flymakeS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-flymake-packages
  '(
    flymake
    flymake-cursor
    )
  "The list of Lisp packages required by the lotus-flymake layer.

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

(defun lotus-flymake/init-flymake ()
  ;; http://stackoverflow.com/questions/20377288/setup-makefile-to-check-both-c-and-c-source-files-with-emacs-flymake
  ;; (add-hook 'find-file-hook 'flymake-find-file-hook)

  (use-package flymake
      :defer t
      :config
      (progn

        (progn
          (setq
           ;; http://stackoverflow.com/questions/2571436/emacs-annoying-flymake-dialog-box
           ;; flymake-gui-warnings-enabled nil       ;need to know.
           flymake-gui-warnings-enabled nil       ;need to know.
           flymake-run-in-place
           ;; https://github.com/illusori/emacs-flymake/issues/1
           t)

          (use-package warnings
            :defer t
            :config
            (progn
           (progn
            ;; Overwrite flymake-display-warning so that no annoying dialog box is
            ;; used.

            ;; This version uses lwarn instead of message-box in the original version.
            ;; lwarn will open another window, and display the warning in there.
            (defun flymake-display-warning (warning)
              "Display a warning to the user, using lwarn"
              (lwarn 'flymake :warning warning))

            ;; Using lwarn might be kind of annoying on its own, popping up windows and
            ;; what not. If you prefer to recieve the warnings in the mini-buffer, use:
            (defun flymake-display-warning (warning)
              "Display a warning to the user, using lwarn"
              (message warning)))))


          ;;;; general init-cleanup and helper routines
          ;; TODO: rename these to something sane and deprecate the current names.
          (defun flymake-create-temp-copy (file-name prefix) ;source of sorrow with desktop-vc-read
            "Make filename for a temporary copy of FILE-NAME.

If `flymake-run-in-place' is true it will use `flymake-create-temp-inplace',
otherwise it will use `flymake-create-temp-intemp'.

Note that this function, despite its name, does not actually create a
copy of the file: it only choses and returns a filename for the temp
copy."
            (if (and flymake-run-in-place
                     (file-writable-p (dirname-of-file file-name)))
                (flymake-create-temp-inplace file-name prefix)
                (flymake-create-temp-intemp file-name prefix)))

          (with-eval-after-load "sessions-mgr"
            (progn
                (add-to-list
                 'desktop-minor-mode-handlers
                 (cons 'flymake-mode (desktop-get-readonly-proof-mode flymake-mode)))))

          ;; (deh-require-maybe session-config
          ;;   (add-to-list 'desktop-minor-mode-handlers (cons 'flymake-mode
          ;;                                                   (desktop-get-readonly-proof-mode flymake-mode))))
          )


        )))


(defun lotus-flymake/init-flymake-cursor ()
  ;; http://www.emacswiki.org/emacs/flymake-cursor.el
  (use-package flymake-cursor
    :defer t
    :config
    (progn
      (progn
        ))))

;;; packages.el ends here
