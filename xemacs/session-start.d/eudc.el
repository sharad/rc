;;; eudc.el --- EUCD setup

;; Copyright (C) 2011  Sharad Pratap

;; Author: Sharad Pratap <ssdfd at asdfsf.com>
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

;; from http://www.emacswiki.org/emacs/EUDC

(deh-section "LDAP BBDB backend"

  (when (and (xrequire 'ldap)
             (xrequire 'eudc))


    (defun eudc-ldap-datas ()
      (or (and (boundp 'eudc-ldap-datas) eudc-ldap-datas) '()))

    (setq ldap-default-base "ou=addressbook,dc=your_dc_here,dc=fr")

    (setq ;; eudc-default-return-attributes nil
     eudc-default-return-attributes 'all
          eudc-strict-return-matches nil
          ldap-ldapsearch-args '("-tt" "-LLL" "-x")
          eudc-inline-query-format '(
                                     (givenName)
                                     (sn)
                                     (givenName sn)
                                     (email)
                                     )
          ;; eudc-inline-query-format nil
          eudc-inline-expansion-format '("%s %s <%s>" givenName name email)
          ;; eudc-inline-expansion-format '("%s <%s>" givenName email)

          ;; (setq ldap-host-parameters-alist
          ;;       (quote (("your_server" base "ou=addressbook,dc=your_dc_here,dc=fr"
          ;;                              binddn "cn=admin,dc=your_dc_here,dc=fr"
          ;;                              passwd "your_password"))))

          ldap-host-parameters-alist
          ;; `(("your_server" base "ou=addressbook,dc=your_dc_here,dc=fr"
          ;;                  binddn "cn=admin,dc=your_dc_here,dc=fr"
          ;;                  passwd "your_password"))
          `(,(cdr (assoc 'office (eudc-ldap-datas))))

          eudc-server-hotlist `(( ,(car (cdr (assoc 'office (eudc-ldap-datas)))) . ldap ))

          eudc-inline-expansion-servers 'hotlist)

    (eudc-set-server (car
                      (cdr (assoc 'office (eudc-ldap-datas)))) 'ldap t)

    (defun enz-eudc-expand-inline()
      (interactive)
      (move-end-of-line 1)
      (insert "*")
      (unless (condition-case nil
                  (eudc-expand-inline)
                (error nil))
        (backward-delete-char-untabify 1)))


    (defun sharad/enz-eudc-expand-inline()
      (interactive)
      (move-end-of-line 1)
      (insert "*")
      (unless (condition-case nil
                  (eudc-expand-inline)
                (error nil))
        (progn
          (backward-delete-char-untabify 1))))



    (defun sharad/eudc-show-at-point ()
      (ietf-drums-parse-address ) ;; from .gnus.d/article.el
      (eudc-display-records (eudc-query '((mail . "spratap@arubanetworks.com")) ) t) )


    (defun eudc-select (choices beg end)
      "Choose one from CHOICES using a completion.
BEG and END delimit the text which is to be replaced."
      ;; (message choices)
      (let ((replacement))
        (setq replacement
              (completing-read "Multiple matches found; choose one: "
                               (mapcar 'list choices)))
        (delete-region beg end)
        (insert replacement)))


;; (eudc-protocol-set 'eudc-attribute-display-method-alist
;;                    '(("jpegphoto" . eudc-display-jpeg-inline)
;;                      ("thumbnailPhoto" . eudc-display-jpeg-inline)
;;                      ("labeledurl" . eudc-display-url)
;;                      ("audio" . eudc-display-sound)
;;                      ("labeleduri" . eudc-display-url)
;;                      ("mail" . eudc-display-mail)
;;                      ("url" . eudc-display-url))
;;                    'ldap)


    ;; Adds some hooks

(eval-after-load "message"
  '(define-key message-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline))
(eval-after-load "sendmail"
  '(define-key mail-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline))
(eval-after-load "post"
  '(define-key post-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline))))


;; (eudc-display-jpeg-inline
;;  (cdaar (remove-if 'null (eudc-query '(("givenName" . "Sharad")) '(thumbnailPhoto)))))




;; /usr/share/emacs/23.1.50/lisp/gnus/gnus-art.el.gz

;; (defun article-display-face (&optional force)
;;   "Display any Face headers in the header."
;;   (interactive (list 'force))
;;   (let ((wash-face-p buffer-read-only))
;;     (gnus-with-article-headers
;;       ;; When displaying parts, this function can be called several times on
;;       ;; the same article, without any intended toggle semantic (as typing `W
;;       ;; D d' would have). So face deletion must occur only when we come from
;;       ;; an interactive command, that is when the *Article* buffer is
;;       ;; read-only.
;;       (if (and wash-face-p (memq 'face gnus-article-wash-types))
;; 	  (gnus-delete-images 'face)
;; 	(let ((from (message-fetch-field "from"))
;; 	      face faces)
;; 	  (save-current-buffer
;; 	    (when (and wash-face-p
;; 		       (gnus-buffer-live-p gnus-original-article-buffer)
;; 		       (not (re-search-forward "^Face:[\t ]*" nil t)))
;; 	      (set-buffer gnus-original-article-buffer))
;; 	    (save-restriction
;; 	      (mail-narrow-to-head)
;; 	      (when (or force
;; 			;; Check whether this face is censored.
;; 			(not (and gnus-article-x-face-too-ugly
;; 				  (or from
;; 				      (setq from (message-fetch-field "from")))
;; 				  (string-match gnus-article-x-face-too-ugly
;; 						from))))
;; 		(while (gnus-article-goto-header "Face")
;; 		  (push (mail-header-field-value) faces)))))
;; 	  (when faces
;; 	    (goto-char (point-min))
;; 	    (let (png image)
;; 	      (unless (setq from (gnus-article-goto-header "from"))
;; 		(insert "From:")
;; 		(setq from (point))
;; 		(insert " [no `from' set]\n"))
;; 	      (while faces
;; 		(when (setq png (gnus-convert-face-to-png (pop faces)))
;; 		  (setq image
;; 			(apply 'gnus-create-image png 'png t
;; 			       (cdr (assq 'png gnus-face-properties-alist))))
;; 		  (goto-char from)
;; 		  (gnus-add-wash-type 'face)
;; 		  (gnus-add-image 'face image)
;; 		  (gnus-put-image image nil 'face))))))))))


(user-provide 'eudc)
;;; eudc.el ends here
