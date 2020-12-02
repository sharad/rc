;;; packages.el --- lotus-orgmode layer packages file for Spacemacs.
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
;; added to `lotus-orgmode-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-orgmode/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-orgmode/pre-init-PACKAGE' and/or
;;   `lotus-orgmode/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-orgmode-packages
  '(
    ;; (PACKAGE :location local)
    ;; (org :location built-in)
    org
    org-agenda
    ob-tangle
    publishing
    (org-timer    :location local)
    (org-clock    :location local)
    (org2rem      :location local)
    (org-feed     :location local)
    (org-git-link :location local)
    worf
    orgnav
    ;; (orgstruct :location local)
    ;; (orgstruct++ :location local)
    outorg                              ;https://github.com/alphapapa/outorg
    outshine                            ;https://github.com/alphapapa/outshibe
    (outline-ivy :location local)
    poporg
    navi-mode
    org-password-manager
    org-parser
    ;; org-tempo
    ;; ox-reveal
    ;; new
    org-context
    org-dashboard
    org-email
    org-mobile-sync
    org-mru-clock
    org-noter
    org-repo-todo
    org-snooze
    org-randomnote
    )
  ;; abgaben


  "The list of Lisp packages required by the lotus-orgmode layer.

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


(spacemacs|use-package-add-hook org
  :pre-init
  (package-initialize))

(defun lotus-orgmode/post-init-org ()
  (use-package org
    :defer t
    :config
    (progn
      (lotus-orgmode-config/post-init-org)
      (assert (null org-show-notification-handler))))
  (use-package org-tempo
    :defer t
    :commands (org-tempo-setup org-tempo-complete-tag)
    :init
    (add-hook 'org-mode-hook 'org-tempo-setup)
    (add-hook 'org-tab-before-tab-emulation-hook 'org-tempo-complete-tag)

    ;; Enable Org Tempo in all open Org buffers.
    (dolist (b (org-buffer-list 'files))
      (with-current-buffer b (org-tempo-setup)))

    :config
    (progn))
  ;; (lotus-orgmode-config/init-org-tempo)
  (use-package task-manager
    :defer t
    :config
    (progn
      (progn
        (use-package occ
          :defer t
          :config
          (progn
            (lotus-orgmode-config/post-init-org-task-manager-occ)))))))

(defun lotus-orgmode/post-init-org-agenda ()
  (use-package org-agenda
    ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
    :defer t
    :config
    (progn
      (progn
        (lotus-orgmode-config/post-init-org-agenda))
      (progn
        (use-package appt
          :defer t
          :config
          (progn
            (org-agenda-to-appt)))))))

(defun lotus-orgmode/post-init-ob-tangle ()
  (use-package ob-tangle
    ;; http://notmuchmail.org/emacstips/
    ;; (add-to-list 'load-path "/usr/share/org-mode/lisp")
    :defer t
    :config
    (progn
      (lotus-orgmode-config/post-init-ob-tangle))))

(defun lotus-orgmode/post-init-publishing ()
  (use-package publishing
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-orgmode/init-org-timer ()
  (use-package org-timer
      :defer t
      :config
      (progn
        (lotus-orgmode-config/init-org-timer))))

(defun lotus-orgmode/init-org-clock ()
  (use-package org-clock
      :defer t
      :config
      (progn
        (lotus-orgmode-config/init-org-clock))))

(defun lotus-orgmode/init-org-git-link ()
  (use-package org-git-link
      :defer t
      :config
      (progn
        (lotus-orgmode-config/init-org-git-link))))

(defun lotus-orgmode/init-org-feed ()
  (use-package org-feed
    :defer t
    :config
    (progn
      (lotus-orgmode-config/init-org-feed))))

(defun lotus-orgmode/init-org2rem ()
  (use-package org2rem
    :defer t
    :config
    (progn
      (lotus-orgmode-config/init-org2rem))))

(defun lotus-orgmode/init-ol-notmuch ()
  (use-package ol-notmuch
      ;; http://notmuchmail.org/emacstips/
      ;; (add-to-list 'load-path "/usr/share/org-mode/lisp")
      :defer t
      :config
      (progn
        )))

(defun lotus-orgmode/init-worf ()
  (use-package worf
      :defer t
      :config
      (progn)))


(defun lotus-orgmode/init-orgnav ()
  (use-package orgnav
    :defer t
    :config
    (progn
      (lotus-orgmode-config/init-orgnav))))

;; from ~/.emacs.d/elpa/org-plus-contrib-20181230/etc/ORG-NEWS
;; * Version 9.2
;; ** Incompatible changes
;; *** Removal of OrgStruct mode mode and radio lists

;; OrgStruct minor mode and radio lists mechanism (~org-list-send-list~
;;                                                 and ~org-list-radio-lists-templates~) are removed from the code base.

;; Note that only radio /lists/ have been removed, not radio tables.

;; If you want to manipulate lists like in Org in other modes, we suggest
;; to use orgalist.el, which you can install from GNU ELPA.

;; If you want to use Org folding outside of Org buffers, you can have a
;; look at the outshine package in the MELPA repository.
;;
;;
;; (defun lotus-orgmode/init-orgstruct ()
;;   (use-package orgstruct
;;       :defer t
;;       :config
;;       (progn
;;         )))
;;
;; (defun lotus-orgmode/init-orgstruct++ ()
;;   (use-package orgstruct++
;;       :defer t
;;       :config
;;       (progn
;;         (progn))))
;;           ;;(setq orgstruct-heading-prefix-regexp "^;; ")

(defun lotus-orgmode/post-init-outorg ()
  ;; https://github.com/alphapapa/outorg
  (use-package outorg
    :defer t
    :config
    (progn
      (lotus-orgmode-config/post-init-outorg))))

(defun lotus-orgmode/post-init-outshine ()
  (use-package outshine
    :init
    ;; https://github.com/alphapapa/outshine
    (defvar outline-minor-mode-prefix "\M-#")
    :defer t
    :config
    (progn
      (lotus-orgmode-config/post-init-outshine))))

(defun lotus-orgmode/init-outline-ivy ()
  ;; http://www.modernemacs.com/post/outline-bullets/
  (use-package outline-ivy
    :defer t
    :config
    (progn
      )))

(defun lotus-orgmode/init-poporg ()
  (use-package poporg
    :defer t
    :commands (poporg-dwim)
    :config
    (progn
      )))

(defun lotus-orgmode/init-navi-mode ()
  (use-package navi-mode
      :defer t
      :config
      (progn
        )))

(defun lotus-orgmode/init-org-password-manager ()
  (use-package org-password-manager
               :defer t
               :config
               (progn)))

(defun lotus-orgmode/init-org-parser ()
  (use-package org-parser
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-context ()
  (use-package org-context
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-dashboard ()
  (use-package org-dashboard
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-email ()
  (use-package org-email
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-mobile-sync ()
  (use-package org-mobile-sync
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-mru-clock ()
  (use-package org-mru-clock
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-noter ()
  (use-package org-noter
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-repo-todo ()
  (use-package org-repo-todo
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-snooze ()
  (use-package org-snooze
    :defer t
    :config
    (progn)))

(defun lotus-orgmode/init-org-randomnote ()
  (use-package org-randomnote
    :defer t
    :config
    (progn)))

;;; packages.el ends here
