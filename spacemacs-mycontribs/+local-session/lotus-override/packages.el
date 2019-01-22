;;; packages.el --- lotus-override layer packages file for Spacemacs.
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
;; added to `lotus-override-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-override/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-override/pre-init-PACKAGE' and/or
;;   `lotus-override/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-override-packages
  '(
    (lsdb    :location local)
    git-gutter+
    )
  "The list of Lisp packages required by the lotus-override layer.

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

(defun lotus-override/init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (progn
      (progn
        (defun lsdb-gnus-update-record ()
          (with-current-buffer (or
                                gnus-article-current-summary
                                gnus-original-article-buffer
                                (current-buffer))
            (lsdb-update-records-and-display)))))))





(defun lotus-override/post-init-git-gutter+ ()
  (use-package git-gutter+
    :defer t
    :config
    (progn
      (progn
        ;; Error
        ;; "Selecting deleted buffer"
        ;; is coming from set-buffer
        ;; in (helm-resume)
        ;; (helm-resume-select-buffer)
        ;; helm-last-buffer
        ;; (helm-resume (nth arg helm-buffers))
        ;; (helm-resume-p resume)
        ;; basically we need to clean up helm-buffers from deleted buffers
        ;; so advice helm-resume to clean up.

        ;;or basically in case of this error
        ;; helm should not call helm-resume by default.

        ;;other similar issue https://github.com/syl20bnr/spacemacs/issues/6945
        ;;but not with helm

        ;;TODO: after-change-major-mode-hook from subr.el
        ;;TODO: with-current-buffer          from subr.el
        ;; here a new run-hooks like run-hook-failsafe
        ;; which will remove hook function it throw error
        ;; than after-change-major-mode-hook should be run with it.
        ;; also with-current-buffer buffer could also add some error check for
        ;; for killed buffers.

        ;;; from subr.el

        ;; (defmacro with-current-buffer (buffer-or-name &rest body)
        ;;           "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
        ;; BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
        ;; The value returned is the value of the last form in BODY.  See
        ;; also `with-temp-buffer'."
        ;;           (declare (indent 1) (debug t))
        ;;           `(save-current-buffer
        ;;              (set-buffer ,buffer-or-name)
        ;;              ,@body))

        (message "Fixed error \"Selecting deleted buffer\" in git-gutter+ package")

        (defun git-gutter+-reenable-buffers ()
          ;; (message "test")
          (dolist (buf git-gutter+-buffers-to-reenable)
            (if (and
                 (bufferp buf)
                 (buffer-live-p buf))
                (with-current-buffer buf
                  (git-gutter+-turn-on))
              (message "buffer %s is not buffer or already killed" buf)))
          (setq git-gutter+-buffers-to-reenable nil))))))
