;;; server-config.el --- Summary relate functions, summary format etc.

;; Copyright (C) 2011  Sharad Pratap

;; Author:
;; Keywords: lisp

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

;; I subscribed a bunch of mailing lists via news.gmane.org.
;; I use the gmane server as my primary news source.



(require 'nnheader)
(require 'host-info)
(require 'common-info)
(require 'passwds)

;; http://www.delorie.com/gnu/docs/emacs/gnus_316.html Q1.11
;; (require 'tm-setup)
;; (require 'gnus)
;; (require 'mime-compose)

;; use M-x password-reset (from password-cache.el)
;; if .authinfo.gpg  became active later.

(setq gnus-select-method '(nntp "news.gmane.org"))

(add-to-list
 'gnus-secondary-select-methods
 '(nnimap "localhost"
   (nnimap-address "localhost")
   ;; (nnimap-server-port 993)
   ;; (nnimap-server-port 443)
   (nnimap-server-port 143)
   (nnimap-stream network)
   (nnimap-authenticator login)
   (nnimap-authinfo-file "~/.authinfo.gpg")
   (nnir-search-engin imap)))

(add-to-list
 'gnus-secondary-select-methods
 `(nnvirtual
   ,(if (equal (system-name) office-host-name)
        "Office\\.INBOX\\|Office\\.sent-mail"
        "Gmail\\.INBOX\\|Gmail\\.sent-mail")))


;;{{ make it working only for Outlook Office
(setq gnus-message-archive-method
      '(nnimap "localhost"
        (nnimap-address "localhost")
        ;; (nnimap-server-port 993)
        ;; (nnimap-server-port 443)
        (nnimap-server-port 143)
        ;; (nnimap-stream ssl)
        (nnimap-authinfo-file "~/.authinfo.gpg")))
;;}}


;; ;; ;; (add-to-list 'gnus-secondary-select-methods
;; ;; ;;              '(nntp "gnu"))

;; ;; ;; Set the prefix when using jump to select a newsgroup.
;; ;; ;; needs a newer gnus
;; ;; ;; (setq gnus-group-jump-to-group-prompt '((0 . "nnml:mail.") (1 .  "gmane.")
;; ;; ;;                                         (2 . "nnshimbun+")
;; ;; ;;                                         (3 .  "nnfolder+archive:")))
;; ;; ;; (setq gnus-group-jump-to-group-prompt "nnimap:gmail.")

;; ;; ;; (setq
;; ;; ;;  gnus-select-method '(nntp "us.usenet-news.net")
;; ;; ;;  message-send-mail-function 'smtpmail-send-it
;; ;; ;;  smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
;; ;; ;;  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "sh4r4d _at_ _G-mail_" nil))
;; ;; ;;  smtpmail-auth-credentials '(("smtp.gmail.com" 587 "sh4r4d _at_ _G-mail_" nil))
;; ;; ;;  smtpmail-default-smtp-server "smtp.gmail.com"
;; ;; ;;  smtpmail-smtp-server "smtp.gmail.com"
;; ;; ;;  smtpmail-smtp-service 587
;; ;; ;;  smtpmail-local-domain "taj")


(provide 'server-config)
;;; server-config.el ends here
