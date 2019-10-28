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
    (gnus :location local)
    bbdb
    lsdb
    shimbun
    notmuch
    (gnus-win :location local)
    (gnus-sum :location local)
    (gnus-msg :location local)
    (gnus-pers :location local)
    (gnus-namazu :location local)
    gnus-demon
    (gnus-dired :location local)
    (message :location local)
    sendmail
    dbus
    (mailcrypt :location local)
    (nnheader :location local)
    (gnus-group :location local)

    (mm-decode :location local)
    (nntodo :location local)
    (gnus-summary :location local)
    rs-gnus-exts
    (gnus-start :location local)
    (common-info :location local)
    (host-info :location local)
    common-info
    passwds)
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


(defun lotus-mailnews/pre-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (lotus-mailnews/pre-init-gnus-config))

  (progn                                ;unconditionally
    (progn
      (progn
        (setq gnus-init-file "~/.gnus.el"))
      (make-directory (expand-file-name ".cache/autoconfig/gnus/" user-emacs-directory) t)
      (setq
       gnus-home-directory (expand-file-name "autoconfig/gnus/" user-emacs-directory))
      (setq
       gnus-directory      (concat gnus-home-directory "News/"))
      (setq
       nndraft-directory (concat gnus-directory "drafts/")))))

(defun lotus-mailnews/post-init-gnus ()
  (use-package gnus
    :defer t
    :config
    (lotus-mailnews/post-init-gnus-config)))

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

(defun lotus-mailnews/init-notmuch ()
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
  (use-package lsdb
    :defer t
    :config
    (lotus-mailnews/init-gnus-msg-config)))


        ;; (".*"
        ;;  (From
        ;;   (with-current-buffer gnus-article-buffer
        ;;     (message-fetch-field "to")))

(defun lotus-mailnews/init-gnus-art ()
  (use-package gnus-art
    :defer t
    :config
    (lotus-mailnews/init-gnus-art-config)))

(defun lotus-mailnews/init-nnmail ()
  (use-package lsdb
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

(defun lotus-mailnews/init-gnus-daemon ()
  (use-package gnus-daemon
    :defer t
    :config
    (lotus-mailnews/init-gnus-daemon-config)))

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

(defun lotus-mailnews/init-dbus ()
  (use-package dbus
    :defer t
    :config
    (lotus-mailnews/init-dbus-config)))

(defun lotus-mailnews/init-mailcrypt ()
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

(defun lotus-mailnews/init-nntodo ()
  ;; (require 'todo-gnus)
  (use-package nntodo
    :defer t
    :config
    (lotus-mailnews/init-nntodo-config)))

(defun lotus-mailnews/init-gnus-summary ()
  (use-package gnus-summary
    :defer t
    :config
    (lotus-mailnews/init-gnus-summary-config)))

(defun lotus-mailnews/init-rs-gnus-exts ()
  (use-package gnus-summary-stripe
    :defer t
    :config
    (lotus-mailnews/init-rs-gnus-exts-config)))

(defun lotus-mailnews/init-gnus-start ()
  (use-package gnus-start
    :defer t
    :config
    (progn
      (progn
        (setq
         ;;see http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC13

         ;;  1.9 Auto Save

         ;; Whenever you do something that changes the Gnus data (reading
         ;; articles, catching up, killing/subscribing groups), the change is
         ;; added to a special dribble buffer. This buffer is auto-saved the
         ;; normal Emacs way. If your Emacs should crash before you have saved
         ;; the `.newsrc' files, all changes you have made can be recovered
         ;; from this file.

         ;; If Gnus detects this file at startup, it will ask the user whether
         ;; to read it. The auto save file is deleted whenever the real
         ;; startup file is saved.

         ;; If gnus-use-dribble-file is nil, Gnus won't create and maintain a
         ;; dribble buffer. The default is t.

         ;; Gnus will put the dribble file(s) in gnus-dribble-directory. If
         ;; this variable is nil, which it is by default, Gnus will dribble
         ;; into the directory where the `.newsrc' file is located. (This is
         ;; normally the user's home directory.) The dribble file will get the
         ;; same file permissions as the .newsrc file.

         ;; If gnus-always-read-dribble-file is non-nil, Gnus will read the
         ;; dribble file on startup without querying the user.

         gnus-dribble-directory (expand-file-name ".cache/autoconfig/gnus/gnus-data" user-emacs-directory)
         gnus-always-read-dribble-file t
         ;http://sunsite.ualberta.ca/Documentation/Gnu/emacs-20.7/html_chapter/gnus_1.html#SEC12
         ;; You can turn off writing the `.newsrc' file by setting
         ;; gnus-save-newsrc-file to nil, which means you can delete the file
         ;; and save some space, as well as exiting from Gnus faster. However,
         ;; this will make it impossible to use other newsreaders than
         ;; Gnus. But hey, who would want to, right?
         gnus-save-newsrc-file nil)))))

(defun lotus-mailnews/init-host-info ()
  (use-package host-info
    :defer t
    :config
    (progn
      (progn))))

(defun lotus-mailnews/init-common-info ()
  (use-package common-info
    :defer t
    :config
    (progn
      (progn))))

(defun lotus-mailnews/init-passwds ()
  (use-package passwds
    :defer t
    :config
    (progn
      (progn))))
;;; packages.el ends here
