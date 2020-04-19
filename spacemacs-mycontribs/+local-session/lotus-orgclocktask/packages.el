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
    org
    ;; org-notify
    org-doing
    org-alert
    ;; org-wild-notifier
    lotus-utils
    ;; org-misc-utils-lotus
    org-clock-unnamed-task
    org-clock-hooks
    org-clock-check
    org-clock-in-if-not
    ;; org-clock-utils-lotus
    org-clock-wrapper
    org-clock-daysummary
    org-clock-table-misc-lotus
    occ
    activity
    org-clock-resolve-advanced
    timesheet
    ;; wakatime-mode
    task-manager
    startup-hooks
    counsel-org-clock
    org-clock-split)


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

;; (spacemacs|use-package-add-hook org
;;   :pre-init
;;   (package-initialize))

(defun lotus-orgclocktask/post-init-org ()
  (use-package org
    :defer t
    :init
    (lotus-orgclocktask/post-init-org-init)
    :config
    (lotus-orgclocktask/post-init-org-config)))

(defun lotus-orgclocktask/init-org-doing ()
  (use-package org-doing
    :defer t
    :init
    (lotus-orgclocktask/init-org-doing-init)
    :config
    (lotus-orgclocktask/init-org-doing-config)))

(defun lotus-orgclocktask/init-org-alert ()
  ;; https://www.reddit.com/r/emacs/comments/3nx0d0/introducing_orgalert_system_notifications_for/
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
  ;; https://www.reddit.com/r/emacs/comments/8bywj3/orgnotify_to_show_scheduled_items/
  (use-package org-alert
    :init
    (lotus-orgclocktask/init-org-alert-init)
    :commands (org-alert-enable)
    :defer t
    :config
    (lotus-orgclocktask/init-org-alert-config)))

;; not working
;; (defun lotus-orgclocktask/init-org-wild-notifier ()
;;   ;; see also https://github.com/akhramov/org-wild-notifier.el
;;   (use-package org-wild-notifier
;;     :init
;;     (org-wild-notifier-mode)
;;     :commands (org-wild-notifier-mode)
;;     :defer t
;;     :config
;;     (progn)))

(defun lotus-orgclocktask/init-lotus-utils ()
  (use-package org-misc-utils-lotus
    :defer t
    :init
    (lotus-orgclocktask/init-lotus-utils-init)
    :config
    (lotus-orgclocktask/init-lotus-utils-config)))

(defun lotus-orgclocktask/init-org-clock-unnamed-task ()
  (use-package org-clock-unnamed-task
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-unnamed-task-init)
    :config
    (lotus-orgclocktask/init-org-clock-unnamed-task-config)))

(defun lotus-orgclocktask/init-org-clock-hooks ()
  (use-package org-clock-hooks
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-hooks-init)
    :config
    (lotus-orgclocktask/init-org-clock-hooks-config)))

(defun lotus-orgclocktask/init-org-clock-check ()
  (use-package org-clock-check
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-check-init)
    :config
    (lotus-orgclocktask/init-org-clock-check-config)))

(defun lotus-orgclocktask/init-org-clock-in-if-not ()
  (use-package org-clock-in-if-not
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-in-if-not-init)
    :config
    (lotus-orgclocktask/init-org-clock-in-if-not-config)))

(defun lotus-orgclocktask/init-org-clock-utils-lotus ()
  (use-package org-clock-utils-lotus
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-utils-lotus-init)
    :config
    (lotus-orgclocktask/init-org-clock-utils-lotus-config)))

(defun lotus-orgclocktask/init-org-clock-wrapper ()
  (use-package org-clock-wrapper
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-wrapper-init)
    :config
    (progn
      (progn))))


(defun lotus-orgclocktask/init-org-clock-daysummary ()
  (use-package org-clock-daysummary
    ;; :defer t
    :defer t
    :commands (org-clock-work-day-mode-line-add)
    :init
    (lotus-orgclocktask/init-org-clock-daysummary-init)
    :config
    (lotus-orgclocktask/init-org-clock-daysummary-config)))

(defun lotus-orgclocktask/init-org-clock-table-misc-lotus ()
  (use-package org-clock-table-misc-lotus
      :defer t
      :config
      (progn
        ))
  (use-package org-nagora-report
      :defer t
      :config
      (progn
        ))
  (use-package org-timesheet
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-occ ()
  (progn
    (use-package occ
      ;; :commands (occ-mode occ-uninsinuate)
      :defer t
      :init
      (lotus-orgclocktask/init-occ-init)
      :config
      (lotus-orgclocktask/init-occ-config))))

(defun lotus-orgclocktask/init-activity ()
  (use-package activity
    :defer t
    :init
    (activity-activate-all)
    :config
    (progn)))

(defun lotus-orgclocktask/init-org-clock-resolve-advanced ()
  (use-package org-clock-resolve-advanced
    :defer t
    :init
    (lotus-orgclocktask/init-org-clock-resolve-advanced-init)
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

;; (defun lotus-orgclocktask/post-init-wakatime-mode ()
;;   ;; https://github.com/tmarble/timesheet.el
;;   (use-package wakatime-mode
;;       :defer t
;;       :config
;;       (progn)))                            ;do not need it now. will see later.
;;         ;; (global-wakatime-mode)


(defun lotus-orgclocktask/init-task-manager ()
  (use-package task-manager
    :defer t
    :commands (office-mode
               task-current-party-select-set
               task-current-party task-party-dir
               task-select-party-dir
               find-task-dir)
    :config
    (lotus-orgclocktask/init-task-manager-config)))


(defun lotus-orgclocktask/post-init-startup-hooks () ;getting run when run-each-hooks called at last
  (use-package startup-hooks
    :defer t
    :config
    (lotus-orgclocktask/post-init-startup-hooks-config)))

(defun lotus-orgclocktask/init-counsel-org-clock ()
  (use-package counsel-org-clock
    :defer t
    :config
    (progn
      )))

(defun lotus-orgclocktask/init-org-clock-split ()
  (use-package org-clock-split
    :defer t
    :config
    (progn)))

;;; packages.el ends here
