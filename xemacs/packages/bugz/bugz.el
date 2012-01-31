;;; bugz.el ---  Bugzilla Emacs Interface

;; Copyright (C) 2012  Sharad Pratap

;; Author: Sharad Pratap <sharad at home>
;; Keywords: data

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

;; Hi

;;; Code:


(require 'xml-rpc)

;; (defvar traker-default-url "https://bugzilla.mozilla.org/xmlrpc.cgi" "Bugz xmlrpc url.")

;; (defvar trakers-alist nil ""

;; (setfq trackers-alist
;;        '(
;;          (username . ((bugz . "login")))
;;          (password . ((bugz . "password")))))

;; (defun tracker/login (username password &optional url method opts)
;;   (xml-rpc-method-call
;;    traker-default-url method '(("login"."login")
;;                                ("password"."password"))))


(defvar bugz-url "https://bugzilla.mozilla.org/xmlrpc.cgi" "Bugz xmlrpc url.")
(defvar bugz-default-username nil "Bugzilla default username used in search.")

;; http://www.emacswiki.org/emacs/UrlPackage#toc3
;; (setq url-cookie-confirmation t
;;       url-cookie-trusted-urls '("^http://\\(www\\.\\)?emacswiki\\.org/.*")
;;       url-cookie-untrusted-urls '("^https?://")
;;       ;; url-privacy-level
;;       ;; url-cookie-storage
;;       ;; url-cookie-save-interval
;;       ;; url-cookie-file
;;       )

(defun bugz-dispatch (method &optional args url)
  (xml-rpc-method-call (or url bugz-url) method args))

(defun bugz/User.logout ()
  (bugz-dispatch 'User.logout))

(defun bugz/User.login (&optional opts username password url)
  (let* ((url (or url bugz-url))
         (username (or username
                       (read-from-minibuffer (format "Bugzilla [%s] User: " url))))
         (password (or password
                       (read-passwd (format "Bugzilla [%s] Password: " url)))))
      (bugz-dispatch 'User.login
                     `(("login".,username)
                       ("password".,password)
                        ,@opts))))

(defun bugz/Bug.search (criteria)
  (bugz-dispatch 'Bug.search criteria))

(defun bugz-bug-search (status &optional username)
  (bugz/Bug.search `(("assigned_to" . ,(or username bugz-default-username))
                     ("status" . ,status))))

(defun bugz-bug-get-bugs-attributes (attributes bugs)
  (flet ((first-belong-in-attributesp (e)
                                      (member (car e) attributes))
          (modify-list (l)
                       (remove-if-not #'first-belong-in-attributesp l)))
    (mapcar #'modify-list bugs)))

(testing

 (bugz/User.login '(("rememberlogin" . 0)))

 (bugz-bug-search '("ASSIGNED" "REOPNED" "NEW"))

 (bugz-bug-get-bugs-attributes
  '("id" "summary")
  (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

 (bugz-bug-get-bugs-attributes
  '("id" . "summary")
  (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

 (bugz-bug-search '("ASSIGNED" "REOPNED" "NEW")))


(provide 'bugz)
;;; bugz.el ends here

