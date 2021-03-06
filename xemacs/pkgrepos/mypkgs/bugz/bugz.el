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

(eval-when-compile
  '(require 'cl))

(require 'xml-rpc)
(require 'read-utils)
(require 'tree)

(defvar bugz-url "https://bugzilla.mozilla.org" "Bugz url.")
(defvar bugz-xmlrpc-url-path "/xmlrpc.cgi" "Bugz xmlrpc url path.")
(defvar bugz-xmlrpc-url (concat bugz-url "/xmlrpc.cgi") "Bugz xmlrpc url.")
(defvar bugz-showbug-url-path "/show_bug.cgi?id=" "Bugz showbug url path.")
(defvar bugz-showbug-url (concat bugz-url "/show_bug.cgi?id=") "Bugz showbug url.")
(defvar bugz-default-username nil "Bugzilla default username used in search.")

(defun bugz-get-xmlrpc-url (&optional url)
  (concat
   (or url bugz-url)
   bugz-xmlrpc-url-path))

(defun bugz-get-showbug-url (&optional url)
  (concat
   (or url bugz-url)
   bugz-showbug-url-path))

;; http://www.emacswiki.org/emacs/UrlPackage#toc3
;; (setq url-cookie-confirmation t
;;       url-cookie-trusted-urls '("^http://\\(www\\.\\)?emacswiki\\.org/.*")
;;       url-cookie-untrusted-urls '("^https?://")
;;       ;; url-privacy-level
;;       ;; url-cookie-storage
;;       ;; url-cookie-save-interval
;;       ;; url-cookie-file
;;       )APL FUNCTIONAL SYMBOL QUAD RIGHTWARDS ARROW


;;;; core
(defun bugz-dispatch (method &optional args url)
  (xml-rpc-method-call (bugz-get-xmlrpc-url url) method args))

(defun bugz/Bug.method (method criteria &optional url)
  (bugz-dispatch method criteria url))

;; (defun bugz-method (method ret criteria &optional url)
;;   (cdr (assoc ret (bugz/Bug.method method criteria url))))

(defun bugz-method (method ret criteria &optional url)
  (mapcar '(lambda (e)
            (tree-leaves e 1))
          (cdr (assoc ret (bugz/Bug.method method criteria url)))))

;;{{

(defun bugz-get-items-attributes (attributes items)
  (flet ((first-belong-in-attributesp (e)
                                      (member (car e) attributes))
          (modify-list (l)
                       (remove-if-not #'first-belong-in-attributesp l)))
    (mapcar #'modify-list items)))
;;;;


;;;;
(when nil
  (read-from-minibuffer-fns
   (lambda (inittext km)
     (read-from-minibuffer (concat "value"  ": ") inittext km))
   (lambda (inittext km)
     (read-list-from-minibuffer (concat "value for list"  ": ") inittext km))))
;;;;

;;;; critaria management
(defvar bugz-search-criterias
      `(("assigned to me and status OPEN" . (,@(if (boundp 'bugz-default-username)
                                                   (list `("assigned_to" . ,bugz-default-username)))
                                               ("status" . ,(if (boundp 'bugz-default-status)
                                                                bugz-default-status
                                                                "OPEN")))))
  "Bug search criteria.")


(defun bugz-get-attribute-name ()
  (let ((retval (read-from-minibuffer "attributes: ")))
    (if (not (string-equal retval ""))
        retval)))



(defun bugz-get-attribute-value (attribute)
  (read-from-minibuffer-fns
   (lambda (inittext km)
     (read-from-minibuffer (concat "value for " attribute ": ") inittext km))
   (lambda (inittext km)
     (read-list-from-minibuffer (concat "list of values for " attribute ": ") inittext km))))


(defun bugz-make-search-criteria ()
  (interactive)
  (let ((criteria
         (let (attribute)
           (loop until (not (setq attribute (bugz-get-attribute-name)))
              collect (cons attribute (bugz-get-attribute-value attribute)))))
        (name (read-from-minibuffer "Search Name: ")))
    (if (and name
             (not (string-equal name "")))
        (nconc bugz-search-criterias (list (cons name criteria))))
    criteria))

;;}}

(defun bugzilla-method (method ret &optional attributes criteria url)
  (let ((attributes (or attributes (list "summary")))
         (criteria (cond
                     ((equal criteria t) (bugz-make-search-criteria))
                     ((null criteria) (cdar bugz-search-criterias))
                     (t criteria))))
     (bugz-get-items-attributes attributes
                                (bugz-method method ret criteria url))))


; (bugz-method 'Bug.get "bugs" (bugz-make-search-criteria))
; (defvar bugzresult nil )
; (setq bugzresult (bugz/Bug.method 'Bug.get (bugz-make-search-criteria)))




; (bugzilla-method 'Bug.search "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") '(("ids" 12123 32121)))



;; logout
(defun bugz/User.logout ()
  (bugz-dispatch 'User.logout))

;; login
(defun bugz/User.login (&optional opts username password url)
  (let* ((url (bugz-get-xmlrpc-url url))
         (username (or username
                       (read-from-minibuffer (format "Bugzilla [%s] User: " url))))
         (password (or password
                       (read-passwd (format "Bugzilla [%s] Password: " url)))))
      (bugz-dispatch 'User.login
                     `(("login".,username)
                       ("password".,password)
                        ,@opts)
                     url)))

;; (testing

;;  (bugzilla-search-bugs '("summary" "id") '(("id" . 786)))

;;  (bugz/User.login '(("rememberlogin" . 0)))

;;  (bugz-bug-search '("ASSIGNED" "REOPNED" "NEW"))

;;  (bugz-get-items-attributes
;;   '("id" "summary")
;;   (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

;;  (bugz-get-items-attributes
;;   '("id" . "summary")
;;   (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

;;  (bugz-bug-search '("ASSIGNED" "REOPENED" "NEW")))






;;;; utils funs: could change

(defun bugzilla-search-bugs (attributes criteria &optional url)
  (bugzilla-method 'Bug.search "bugs" attributes criteria url))

(defun bugzilla-get-bugs (attributes criteria &optional url)
  (bugzilla-method 'Bug.get "bugs" attributes criteria url))

; (bugzilla-method 'Bug.search "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") '(("ids" 12123 32121)))
; (bugz-method 'Bug.get "bugs" '(("ids" 12123 32121)))
; (bugz/Bug.method 'Bug.get '(("ids" 37026 )))
; (bugz-method 'Bug.get "bugs" '(("ids" 37026)))
; (bugzilla-get-bugs '("id" "summary") '(("ids" 37026)))

;;;;



(provide 'bugz)
;;; bugz.el ends here
