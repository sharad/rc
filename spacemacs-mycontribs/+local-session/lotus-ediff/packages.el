;;; packages.el --- lotus-ediff layer packages file for Spacemacs.
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
;; added to `lotus-ediff-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-ediff/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-ediff/pre-init-PACKAGE' and/or
;;   `lotus-ediff/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-ediffS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-ediff-packages
  '(
    (ediff :location local)
    )
  "The list of Lisp packages required by the lotus-ediff layer.

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

(defun lotus-ediff/post-init-ediff ()
  (use-package ediff
      :defer t
      :config
      (progn
        (progn

          (defun ediff-write-merge-buffer ()
            (let ((file ediff-merge-store-file))
              (set-buffer ediff-buffer-C)
              (write-region (point-min) (point-max) file)
              (message "Merge buffer saved in: %s" file)
              (set-buffer-modified-p nil)
              (sit-for 1)))

          ;; (setq ;; ediff-quit-hook 'kill-emacs
          ;;  ediff-quit-merge-hook 'ediff-write-merge-buffer)

          ;; (ediff-merge-files-with-ancestor \\\"$LOCAL\\\" \\\"$REMOTE\\\"
          ;;                                  \\\"$BASE\\\" nil \\\"$MERGED\\\")\"

          (defvar ediff-after-quit-hooks nil
            "* Hooks to run after ediff or emerge is quit.")

          (defadvice ediff-quit (after edit-after-quit-hooks activate)
            (run-hooks 'ediff-after-quit-hooks))

          (setq git-mergetool-emacsclient-ediff-active nil)

          (defun local-ediff-frame-maximize ()
            (let* ((bounds (display-usable-bounds))
                   (x (nth 0 bounds))
                   (y (nth 1 bounds))
                   (width (/ (nth 2 bounds) (frame-char-width)))
                   (height (/ (nth 3 bounds) (frame-char-height))))
              (set-frame-width (selected-frame) width)
              (set-frame-height (selected-frame) height)
              (set-frame-position (selected-frame) x y)))

          (setq ediff-window-setup-function 'ediff-setup-windows-plain)
          (setq ediff-split-window-function 'split-window-horizontally)

          (defun local-ediff-before-setup-hook ()
            (setq local-ediff-saved-frame-configuration (current-frame-configuration))
            (setq local-ediff-saved-window-configuration (current-window-configuration))
            ;; (local-ediff-frame-maximize)
            (if git-mergetool-emacsclient-ediff-active
                (raise-frame)))

          (defun local-ediff-quit-hook ()
            (set-frame-configuration local-ediff-saved-frame-configuration)
            (set-window-configuration local-ediff-saved-window-configuration))

          (defun local-ediff-suspend-hook ()
            (set-frame-configuration local-ediff-saved-frame-configuration)
            (set-window-configuration local-ediff-saved-window-configuration))

          (add-hook 'ediff-before-setup-hook 'local-ediff-before-setup-hook)
          (add-hook 'ediff-quit-hook 'local-ediff-quit-hook 'append)
          (add-hook 'ediff-suspend-hook 'local-ediff-suspend-hook 'append)

          ;; Useful for ediff merge from emacsclient.
          (defun git-mergetool-emacsclient-ediff (local remote base merged)
            (setq git-mergetool-emacsclient-ediff-active t)
            (if (file-readable-p base)
                (ediff-merge-files-with-ancestor local remote base nil merged)
                (ediff-merge-files local remote nil merged))
            (recursive-edit))

          (defun git-mergetool-emacsclient-ediff-after-quit-hook ()
            (exit-recursive-edit))

          (add-hook 'ediff-after-quit-hooks 'git-mergetool-emacsclient-ediff-after-quit-hook 'append))

        (progn ;; "binary file hexal mode"
          ;; http://trey-jackson.blogspot.in/2010/10/emacs-tip-38-automatically-diff-binary.html
          (defvar ediff-do-hexl-diff nil
            "variable used to store trigger for doing diff in hexl-mode")
          (defadvice ediff-files-internal (around ediff-files-internal-for-binary-files activate)
            "catch the condition when the binary files differ

the reason for catching the error out here (when re-thrown from the inner advice)
is to let the stack continue to unwind before we start the new diff
otherwise some code in the middle of the stack expects some output that
isn't there and triggers an error"
            (let ((file-A (ad-get-arg 0))
                  (file-B (ad-get-arg 1))
                  ediff-do-hexl-diff)
              (condition-case err
                  (progn
                    ad-do-it)
                (error
                 (if ediff-do-hexl-diff
                     (let ((buf-A (find-file-noselect file-A))
                           (buf-B (find-file-noselect file-B)))
                       (with-current-buffer buf-A
                         (hexl-mode 1))
                       (with-current-buffer buf-B
                         (hexl-mode 1))
                       (ediff-buffers buf-A buf-B))
                     (error (error-message-string err)))))))

          (defadvice ediff-setup-diff-regions (around ediff-setup-diff-regions-for-binary-files activate)
            "when binary files differ, set the variable "
            (condition-case err
                (progn
                  ad-do-it)
              (error
               (setq ediff-do-hexl-diff
                     (and (string-match-p "^Errors in diff output.  Diff output is in.*"
                                          (error-message-string err))
                          (string-match-p "^\\(Binary \\)?[fF]iles .* and .* differ"
                                          (buffer-substring-no-properties
                                           (line-beginning-position)
                                           (line-end-position)))
                          (y-or-n-p "The binary files differ, look at the differences in hexl-mode? ")))
               (error (error-message-string err))))))
        )))

;;; packages.el ends here
