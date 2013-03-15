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

(defun bugz-method (method ret criteria)
  (cdr (assoc ret (bugz/Bug.method method criteria))))



;;{{

(defun bugz-get-items-attributes (attributes items)
  (flet ((first-belong-in-attributesp (e)
                                      (member (car e) attributes))
          (modify-list (l)
                       (remove-if-not #'first-belong-in-attributesp l)))
    (mapcar #'modify-list items)))
;;;;

;;;; critaria management
(defvar bugz-search-criterias
      `(("assigned to me and status OPEN" . (,@(if (boundp 'bugz-default-username)
                                                   (list `("assigned_to" . ,bugz-default-username)))
                                               ("status" . ,(if (boundp 'bugz-default-status)
                                                                bugz-default-status
                                                                "OPEN")))))
  "Bug search critarias.")


(defun bugz-get-attribute-name ()
  (let ((retval (read-from-minibuffer "attributes: ")))
    (if (not (string-equal retval ""))
        retval)))

(defun bugz-get-attribute-value (attribute)
  (let ((km
         (define-keymap (copy-keymap minibuffer-local-map) (kbd "C-v")
           #'(lambda () "test"))
          ))
    (read-from-minibuffer (concat "value for " attribute ": ") km)))

;;{{ using condition-case
(defun testb ()
  (interactive)
  (setq deactivate-mark nil)
  (throw 'exit "test"))


(defun eg-read-any-data-using-condcase ()
  (let ((km (copy-keymap minibuffer-local-map)))
    (define-key km (kbd "C-v") 'testb)
    (condition-case test
        (read-from-minibuffer (concat "value for "  ": ") nil km)
      (error (read-from-minibuffer "iooo: ")))))
;;}}

;;{{ using throw catch

(defvar dolist nil)
(defvar  mbstr nil)

(defun throwgoforlist ()
  (interactive)
  (setq redoeg-read t)
  (setq deactivate-mark nil)
  (throw 'goforlist
    (progn
      (setq dolist (not dolist))
      (setq mbstr (buffer-string))
      (exit-minibuffer)
      )))

(defun condread (fn1 fn2 inittext)
  (let ((km (copy-keymap minibuffer-local-map)))
        (define-key km (kbd "C-v") 'throwgoforlist)
        (if dolist
            (funcall fn1 inittext km)
            (funcall fn2 inittext km))))

(defun eg-read-any-data-using-tag (fn1 fn2)
  (message "mbstr %s" mbstr)
  (catch 'goforlist
    (setq redoeg-read nil)
    (setq retval (condread fn1 fn2 mbstr))
    (if redoeg-read
        (eg-read-any-data-using-tag fn1 fn2)
        retval)))

;; (eg-read-any-data-using-tag
;;  #'(lambda (inittext km)
;;      (read-from-minibuffer (concat "value"  ": ") inittext km))
;;  #'(lambda (inittext km)
;;      (read-from-minibuffer (concat "value for list"  ": ") inittext km)))

;;}}

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

(defun bugzilla-method (method ret &optional attributes criteria)
  (let ((attributes (or attributes (list "summary")))
         (criteria (cond
                     ((equal criteria t) (bugz-make-search-criteria))
                     ((null criteria) (cdar bugz-search-criterias))
                     (t criteria))))
     (bugz-get-items-attributes attributes
                                   (bugz-method method ret criteria))))



; (bugzilla-method 'Bug.search "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") t)
; (bugzilla-method 'Bug.get "bugs" '("id" "summary") '(("ids" 12123 32121)))



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




(provide 'bugz)
;;; bugz.el ends here
