;;; packages.el --- lotus-remem layer packages file for Spacemacs.
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
;; added to `lotus-remem-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-remem/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-remem/pre-init-PACKAGE' and/or
;;   `lotus-remem/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-rememS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-remem-packages
  '(
    (remem :location local)
    )
  "The list of Lisp packages required by the lotus-remem layer.

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

(defun lotus-remem/init-remem ()
  (use-package remem
    :defer t
    :config
    (progn
      ;; (require 'basic-utils)
      ;; (require 'misc-utils)
      ;; (define-key perly-sense-map (kbd "C-d") 'perly-sense-smart-docs-at-point)
      ;; (define-key perly-sense-map (kbd "C-g") 'perly-sense-smart-go-to-at-point)
      ;; (require 'misc-utils) ;; auto-config-file
      (setq remem-logfile (lotus-cache-file "remem/remem-log-file")
            remem-prog-dir "/usr/bin"
            remem-database-dir "~/.RA-indexes"
            remem-scopes-list '(("doc" 6 5 500)
                                ("mail" 6 5 500)
                                ("office" 6 5 500))
            ;;(setq remem-terminal-mode t)
            remem-load-original-suggestion  t)


      ;; C-c r t - toggle

      ;; C-c r [number] see numbered suggestion

      ;; C-c r

      ;;{{ Faces
      (set-face-foreground 'remem-odd  "White")
      ;;}}

      ;;from: http://1010.co.uk/tech_notes.html
      ;;
      (defun other-buffer-to-kill ()
        "other buffer - most prob remembrance - to kill ring"
        (interactive)
        (save-excursion
          (other-window 1)
          (let* ((beg (point-min))
                 (end (point-max)))
            (kill-ring-save beg end))
          (other-window 1)))
      ;; complete remem buffer to kill ring code:
      (defun remem-to-kill ()
        (with-current-buffer "*remem-display*"
          (kill-ring-save (point-min) (point-max))))

      (defun splice-buffer ()
        (save-excursion
          (setf (point) (point-min))
          (insert "\n<example>\n")
          (setf (point) (point-max))
          (insert "\n</example>")))

      (defun remem-yank ()
        (interactive)
        (let ((target (current-buffer)))
          (with-temp-buffer
            (let ((source (current-buffer)))
              (yank)
              (splice-buffer)
              (with-current-buffer target
                (insert-buffer-substring source))))))

      (defun remem-append ()
        "remem-display buffer is appended to buffer with correct example tags"
        (interactive)
        (save-excursion
          (remem-to-kill)
          (goto-char (point-max))
          (remem-yank))))))

;;; packages.el ends here
