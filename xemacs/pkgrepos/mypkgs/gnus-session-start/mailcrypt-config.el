;;; mailcrypt-config.el --- mailcrypt encryption

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords:

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


(deh-require-maybe mailcrypt
  ;; http://www.suse.de/~garloff/Writings/mutt_gpg/node18.html
  (mc-setversion "gpg")
  (autoload 'mc-install-write-mode "mailcrypt" nil t)
  (autoload 'mc-install-read-mode "mailcrypt" nil t)
  (add-hook 'gnus-summary-mode-hook 'mc-install-read-mode)
  (add-hook 'message-mode-hook 'mc-install-write-mode)
  (add-hook 'news-reply-mode-hook 'mc-install-write-mode)

  ;; Next time you start Gnus, you get a menu called ¨Mailcrypt¨ which
  ;; gives you an easy access to GnuPG.

  ;; Some variable settings which might be helpful are:

  ;; Use the pgp2 compatibility wrapper
  (setq mc-gpg-path "/usr/bin/gpg-compat")

  ;; If you have more than one key, specify the one to use
  (setq mc-gpg-user-id "0x12345678")

  ;; Always sign encrypted messages
  (setq mc-pgp-always-sign t)

  ;; How long should mailcrypt remember your passphrase
  (setq mc-passwd-timeout 600)

  ;; To sign automatically every message you send, you need to add some
  ;; lisp code. After adding the following lines to your .emacs file,
  ;; (X)Emacs will ask you if the message is to be signed before sending
  ;; it.

  ;; (add-hook 'message-send-hook 'my-sign-message)
  (defun my-sign-message ()
    (if (yes-or-no-p "Sign message? ")
        (mc-sign-message))))


(provide 'mailcrypt-config)
;;; mailcrypt-config.el ends here
