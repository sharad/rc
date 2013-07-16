;;; gnusmisc-config.el --- Summary relate functions, summary format etc.

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


(setq

 ;; message-send-mail-function 'message-send-mail-with-sendmail
 ;; message-sendmail-envelope-from 'header
 ;; message-sendmail-f-is-evil nil

 ;; tls-checktrust 'ask
 ;; tls-program '("gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h"
 ;;               "gnutls-cli --x509cafile /etc/ssl/certs/ca-certificates.crt -p %p %h --protocols ssl3"
 ;;               "openssl s_client -connect %h:%p -CAfile /etc/ssl/certs/ca-certificates.crt -no_ssl2 -ign_eof")
 gnus-agent-synchronize-flags t
 ;; gnus-agent-queue-mail 'always
 ;; gnus-agent-prompt-send-queue t
 ;; gnus-asynchronous t
 ;; gnus-agent-go-online t
 ;; mm-text-html-renderer 'gnus-w3m
 gnus-summary-display-arrow t
 gnus-completing-read-function 'gnus-ido-completing-read
 mail-user-agent 'gnus-user-agent
 read-mail-command 'gnus
 ;;gnus-treat-display-smileys nil
 )
(autoload 'sendmail-send-it "sendmail")



(provide 'gnusmisc-config)
;;; gnumisc-config.el ends here
