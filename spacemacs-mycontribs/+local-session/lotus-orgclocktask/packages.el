;;; packages.el --- lotus-orgclocktask layer packages file for Spacemacs.
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
;; added to `lotus-orgclocktask-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-orgclocktask/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-orgclocktask/pre-init-PACKAGE' and/or
;;   `lotus-orgclocktask/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-orgclocktask-packages
  '(
    ;; (PACKAGE :location local)
    org-clock-utils
    org-clock-daysummary
    org-clocktable-alt
    org-context-clocking
    org-misc-utils
    org-nagora-report
    org-timesheet
    timesheet
    wakatime-mode
    task-manager)
  "The list of Lisp packages required by the lotus-orgclocktask layer.

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

(defun lotus-orgclocktask/init-org-clock-utils ()
  (use-package org-clock-utils
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-clock-daysummary ()
  (use-package org-clock-daysummary
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-clocktable-alt ()
  (use-package org-clocktable-alt
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-context-clocking ()
  (use-package org-context-clocking
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-misc-utils ()
  (use-package org-misc-utils
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-nagora-report ()
  (use-package org-nagora-report
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-org-timesheet ()
  (use-package org-timesheet
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-timesheet ()
  ;; https://github.com/tmarble/timesheet.el
  (use-package timesheet
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-wakatime-mode ()
  ;; https://github.com/tmarble/timesheet.el
  (use-package wakatime-mode
      :defer t
      :config
      (progn
        (global-wakatime-mode))))

(defun lotus-orgclocktask/init-task-manager ()
  (use-package task-manager
      :defer t
      :commands 'office-mode
      :config
      (progn
        (progn
          (define-minor-mode office-mode
              "Prepare for working with collarative office project. This
is the mode to be enabled when I am working in some files on
which other peoples are also working."
            :initial-value nil
            :lighter " Office"
            :global nil
            (condition-case e
                (when office-mode
                  (message "calling office mode")
                  (if (or (eq major-mode 'c-mode)
                          (eq major-mode 'c++-mode))
                      (c-set-style "stroustrup" 1))
                  (set (make-local-variable 'before-save-hook) before-save-hook)
                  (remove-hook 'before-save-hook 'delete-trailing-whitespace t)
                  (message "called office mode"))
              (error (message "Error: %s" e))))))))

;;; packages.el ends here
