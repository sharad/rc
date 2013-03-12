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
;;       )APL FUNCTIONAL SYMBOL QUAD RIGHTWARDS ARROW


;;;; core
(defun bugz-dispatch (method &optional args url)
  (xml-rpc-method-call (or url bugz-url) method args))

(defun bugz/Bug.method (method criteria)
  (bugz-dispatch method criteria))

(defun bugzilla-method (method ret &optional attributes criteria)
  (let ((attributes (or attributes (list "summary")))
         (criteria (cond
                     ((equal criteria t) (bugz-make-bug-search-criteria))
                     ((null criteria) (cdar bug-search-criterias))
                     (t criteria))))
     (bugz-bug-get-bugs-attributes attributes
                                   (bugz-bug-method method ret criteria))))

(bugzilla-bugs-method 'Bug.search "bugs")


;; logout
(defun bugz/User.logout ()
  (bugz-dispatch 'User.logout))

;; login
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

;; general
;; (defmacro bugz/Bug.method (method criteria)
;;   `(bugz-dispatch
;;     ,(intern (concat "Bug." (symbol-name method)))
;;     ,criteria))

(defun bugz/Bug.method (method criteria)
  (bugz-dispatch method criteria))

;; (defmacro testmm (s)
;;     `(setq ,(intern-soft (concat "Bug." (symbol-name s))) 1))


;; search
(defun bugz/Bug.get (criteria)
  (bugz-dispatch 'Bug.get criteria))

;; search
(defun bugz/Bug.search (criteria)
  (bugz-dispatch 'Bug.search criteria))

;; fix it.
;; (defun bugz-bug-search (criteria)
;;   (nconc
;;    (cdr (assoc "bugs" (bugz/Bug.search criteria)))
;;    (list (cons "_bugz-url" bugz-url))))

;; (setq x '( a b))

;; (nconc (cdr x) '(r))

;; general
;; (defmacro bugz-bug-method (method criteria)
;;   `(cdr (assoc "bugs"
;;                (bugz-dispatch
;;                 ,(intern (concat "Bug." (symbol-name method)))
;;                 ,criteria))))

(defun bugz-method (method ret criteria)
  (cdr (assoc ret (bugz/Bug.method method criteria))))


(defun bugz-bug-method (method criteria)
  (cdr (assoc "bugs" (bugz/Bug.method method criteria))))


(defun bugz-bug-search (criteria)
  (cdr (assoc "bugs" (bugz/Bug.search criteria))))

(defun bugz-bug-get (criteria)
  (cdr (assoc "bugs" (bugz/Bug.get criteria))))






(defun bugz-bug-get-bugs-attributes (attributes bugs)
  (flet ((first-belong-in-attributesp (e)
                                      (member (car e) attributes))
          (modify-list (l)
                       (remove-if-not #'first-belong-in-attributesp l)))
    (mapcar #'modify-list bugs)))
;;;;

;;;; critaria management
(setq bug-search-criterias
      `(("assigned to me and status OPEN" . (,@(if (boundp 'bugz-default-username)
                                                   (list `("assigned_to" . ,bugz-default-username)))
                                               ("status" . ,(if (boundp 'bugz-default-status)
                                                                bugz-default-status
                                                                "OPEN"))))))
  "Bug search critarias.")


(defun bugz-get-attribute-name ()
  (let (retval)
    (setq retval (read-from-minibuffer "attributes: "))
    (if (not (string-equal retval ""))
        retval)))

(defun bugz-get-attribute-value (attribute)
  (read-from-minibuffer (concat "value for " attribute ": ")))

(defun bugz-make-bug-search-criteria ()
  (interactive)
  (let ((criteria
         (let (attribute)
           (loop until (not (setq attribute (bugz-get-attribute-name)))
              collect (cons attribute (bugz-get-attribute-value attribute)))))
        (name (read-from-minibuffer "Search Name: ")))
    (if (and name
             (not (string-equal name "")))
        (nconc bug-search-criterias (list (cons name criteria))))
    criteria))


;; (setq x '(a))

;; (nconc x '(c))

;; (bugz-make-bug-search-criteria)
;;;;

;; general
;; (defmacro bugzilla-bugs-method (method &optional attributes criteria)
;;   `(let ((attributes (or ,attributes (list "summary")))
;;          (criteria (cond
;;                      ((equal ,criteria t) (bugz-make-bug-search-criteria))
;;                      ((null ,criteria) (cdar bug-search-criterias))
;;                      (t ,criteria))))
;;      (bugz-bug-get-bugs-attributes attributes
;;                                    (cdr (assoc "bugs"
;;                                                (bugz-dispatch
;;                                                 ,(intern (concat "Bug." (symbol-name method)))
;;                                                 ,criteria))))))


(defun bugzilla-bugs-method (method &optional attributes criteria)
  (let ((attributes (or attributes (list "summary")))
         (criteria (cond
                     ((equal criteria t) (bugz-make-bug-search-criteria))
                     ((null criteria) (cdar bug-search-criterias))
                     (t criteria))))
     (bugz-bug-get-bugs-attributes attributes
                                   (bugz-bug-method method criteria))))


(defun bugzilla-method (method ret &optional attributes criteria)
  (let ((attributes (or attributes (list "summary")))
         (criteria (cond
                     ((equal criteria t) (bugz-make-bug-search-criteria))
                     ((null criteria) (cdar bug-search-criterias))
                     (t criteria))))
     (bugz-bug-get-bugs-attributes attributes
                                   (bugz-bug-method method ret criteria))))

(bugzilla-bugs-method 'Bug.search "bugs")



;;
(defun bugzilla-search-bugs-main (attributes criteria)
  "get bug @attribute from bugzilla for @criteria."
  (bugz-bug-get-bugs-attributes attributes (bugz-bug-search criteria)))


(defun bugzilla-search-bugs (&optional attributes criteria)
  (let ((attributes (or attributes ("summary")))
        (criteria (cond
                    ((equal criteria t) (bugz-make-bug-search-criteria))
                    ((null criteria) (cdar bug-search-criterias))
                    (t criteria))))
    ;; (bugzilla-search-bugs-main attributes criteria)))
    (bugz-bug-get-bugs-attributes attributes (bugz-bug-search criteria))))


(defun bugzilla-get-bugs (&optional attributes criteria)
  (let ((attributes (or attributes ("summary")))
        (criteria (cond
                    ((equal criteria t) (bugz-make-bug-search-criteria))
                    ((null criteria) (cdar bug-search-criterias))
                    (t criteria))))
    ;; (bugzilla-search-bugs-main attributes criteria)))
    (bugz-bug-get-bugs-attributes attributes (bugz-bug-get criteria))))





;; (testing

;;  (bugzilla-search-bugs '("summary" "id") '(("id" . 786)))

;;  (bugz/User.login '(("rememberlogin" . 0)))

;;  (bugz-bug-search '("ASSIGNED" "REOPNED" "NEW"))

;;  (bugz-bug-get-bugs-attributes
;;   '("id" "summary")
;;   (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

;;  (bugz-bug-get-bugs-attributes
;;   '("id" . "summary")
;;   (cdr (assoc "bugs" (bugz-bug-search '("REOPENED")))))

;;  (bugz-bug-search '("ASSIGNED" "REOPENED" "NEW")))




(provide 'bugz)
;;; bugz.el ends here
