;;; packages.el --- lotus-mailnews layer packages file for Spacemacs.
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
;; added to `lotus-mailnews-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-mailnews/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-mailnews/pre-init-PACKAGE' and/or
;;   `lotus-mailnews/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-mailnews-packages
  '(
    outlook
    org-outlook
    (gnus          :location local)
    (nndraft       :location local)
    (gnus-gravatar :location local)
    bbdb
    lsdb
    shimbun
    notmuch
    (gnus-win      :location local)
    (gnus-sum      :location local)
    (gnus-msg      :location local)
    (gnus-pers     :location local)
    (gnus-namazu   :location local)
    (gnus-dired    :location local)
    (gnus-demon    :location local)
    (message       :location local)
    (sendmail      :location local)
    (dbus          :location local)
    (mailcrypt     :location local)
    (nnheader      :location local)
    (gnus-group    :location local)
    (mm-decode     :location local)
    (todo-gnus        :location local)
    rs-gnus-exts
    (gnus-start    :location local)
    (common-info   :location local)
    (host-info     :location local)
    (common-info   :location local)
    (passwds       :location local)
    (mu4e-message  :location local)
    (mu4e-view     :location local))
  "The list of Lisp packages required by the lotus-mailnews layer.

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

(defun lotus-mailnews/init-outlook ()
  (interactive)
  (use-package outlook
    :init
    (lotus-mailnews/init-outlook-init)
    :defer t
    :config
    (lotus-mailnews/init-outlook-config)))

(defun lotus-mailnews/init-org-outlook ()
  (interactive)
  (use-package org-outlook
    :init
    (lotus-mailnews/init-org-outlook-init)
    :defer t
    :config
    (lotus-mailnews/init-org-outlook-config)))

(defun lotus-mailnews/post-init-gnus ()
  (interactive)
  (use-package gnus
    :init
    (lotus-mailnews/post-init-gnus-init)
    :defer t
    :config
    (lotus-mailnews/post-init-gnus-config)))

(defun lotus-mailnews/init-nndraft ()
  (use-package nndraft
    :defer t
    :config
    (lotus-mailnews/init-nndraft-config)))

(defun lotus-mailnews/init-gnus-gravatar ()
  (use-package gnus-gravatar
    :defer t
    :config
    (lotus-mailnews/init-gnus-gravatar-config)))

(defun lotus-mailnews/post-init-bbdb ()
  (use-package bbdb
    :defer t
    :config
    (lotus-mailnews/post-init-bbdb-config)))

(defun lotus-mailnews/post-init-lsdb ()
  (use-package lsdb
    :defer t
    :config
    (lotus-mailnews/post-init-lsdb-config)))

(defun lotus-mailnews/init-shimbun ()
  (use-package sb-rss-blogs ;; shimbun
    :defer t
    :config
    (lotus-mailnews/init-shimbun-config)))

(defun lotus-mailnews/post-init-notmuch ()
  (use-package notmuch-address
    :defer t
    :config
    (lotus-mailnews/init-notmuch-config)))

(defun lotus-mailnews/init-gnus-win ()
  (use-package gnus-win
    :defer t
    :config
    (lotus-mailnews/init-gnus-win-config)))

(defun lotus-mailnews/init-gnus-sum ()
  (use-package gnus-sum
    :defer t
    :config
    (lotus-mailnews/init-gnus-sum-config)))

(defun lotus-mailnews/init-gnus-msg ()
  (use-package gnus-msg
    :defer t
    :config
    (lotus-mailnews/init-gnus-msg-config)))

(defun lotus-mailnews/init-gnus-art ()
  (use-package gnus-art
    :defer t
    :config
    (lotus-mailnews/init-gnus-art-config)))

(defun lotus-mailnews/init-nnmail ()
  (use-package nnmail
    :defer t
    :config
    (lotus-mailnews/init-nnmail-config)))

(defun lotus-mailnews/init-gnus-pers ()
  (use-package gnus-pers
    :defer t
    :config
    (lotus-mailnews/init-gnus-pers-config)))

(defun lotus-mailnews/init-gnus-namazu ()
  (use-package gnus-namazu
    :defer t
    :config
    (lotus-mailnews/init-gnus-namazu-config)))

(defun lotus-mailnews/init-gnus-dired ()
  (use-package gnus-dired
    :defer t
    :config
    (lotus-mailnews/init-gnus-dired-config)))

(defun lotus-mailnews/init-gnus-demon ()
  (use-package gnus-demon
    :init
    (lotus-mailnews/init-gnus-demon-init)
    :defer t
    :config
    (lotus-mailnews/init-gnus-demon-config)))

(defun lotus-mailnews/init-message ()
  (use-package message
    :defer t
    :config
    (lotus-mailnews/init-message-config)))

(defun lotus-mailnews/init-sendmail ()
  (use-package sendmail
    :defer t
    :config
    (lotus-mailnews/init-sendmail-config)))

(defun lotus-mailnews/post-init-dbus ()
  (use-package dbus
    :defer t
    :config
    (lotus-mailnews/init-dbus-config)))

(defun lotus-mailnews/init-mailcrypt ()
  ;; use EasyPG as mailcrypt not available now
  ;; https://www.gnu.org/software/emacs/manual/html_node/gnus/Security.html
  (use-package mailcrypt
    :defer t
    :commands (mc-install-read-mode mc-install-write-mode)
    :config
    (lotus-mailnews/init-mailcrypt-config)))

(defun lotus-mailnews/init-nnheader ()
  (use-package nnheader
    :defer t
    :config
    (lotus-mailnews/init-nnheader-config)))

(defun lotus-mailnews/init-gnus-group ()
  (use-package gnus-group
    :defer t
    :config
    (lotus-mailnews/init-gnus-group-config)))

(defun lotus-mailnews/init-mm-decode ()
  (use-package mm-decode
    :defer t
    :config
    (lotus-mailnews/init-mm-decode-config)))

(defun lotus-mailnews/init-todo-gnus ()
  ;; (require 'todo-gnus)
  (use-package todo-gnus
    :defer t
    :config
    (lotus-mailnews/init-todo-gnus-config)))

(defun lotus-mailnews/init-rs-gnus-exts ()
  (use-package gnus-summary-stripe
    :defer t
    :config
    (lotus-mailnews/init-rs-gnus-exts-config)))

(defun lotus-mailnews/init-gnus-start ()
  (use-package gnus-start
    :defer t
    :config
    (lotus-mailnews/init-gnus-start-config)))

(defun lotus-mailnews/init-host-info ()
  (use-package host-info
    :defer t
    :config
    (lotus-mailnews/init-host-info-config)))

(defun lotus-mailnews/init-common-info ()
  (use-package common-info
    :defer t
    :config
    (lotus-mailnews/init-common-info-config)))

(defun lotus-mailnews/post-init-passwds ()
  (use-package passwds
    :defer t
    :config
    (lotus-mailnews/init-passwds-config)))

(defun lotus-mailnews/init-mu4e-message ()
  (use-package mu4e-message
    :commands (mu4e-message-at-point)
    :defer t
    :config
    (lotus-mailnews/init-mu4e-message-config)))

(defun lotus-mailnews/init-mu4e-view ()
  (use-package mu4e-view
    :commands (org-mu4e-store-link)
    :defer t
    ;; :init
    ;; (defvar mu4e~view-message)
    :config
    (lotus-mailnews/init-mu4e-view-config)))

;;; packages.el ends here
