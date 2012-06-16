;;; eudc-config.el --- EUCD setup

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


  (deh-require-maybe (and eudc ldap passwds)
    "LDAP BBDB backend"


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

    (eudc-set-server (car (cdr (assoc 'office (eudc-ldap-datas)))) 'ldap t)

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



    (defun sharad/eudc-show-at-point (arg)
      (interactive "P")
      ;; from .gnus.d/article.el
      (let ((email (thing-at-point 'fullemail)))
        (if email
            (eudc-display-records (eudc-query (list (cons 'mail (car (ietf-drums-parse-address email)))) ) arg)
            (message "Not able to parse any email at point."))))

    (defun eudc-select (choices beg end)
      "Choose one from CHOICES using a completion.
BEG and END delimit the text which is to be replaced."
      (let ((replacement))
        (setq replacement
              (completing-read "Multiple matches found; choose one: "
                               (mapcar 'list choices)))
        (delete-region beg end)
        (insert replacement)))


(eudc-protocol-set 'eudc-attribute-display-method-alist
                   '(("jpegphoto" . eudc-display-jpeg-inline)
                     ("thumbnailphoto" . eudc-display-jpeg-inline)
                     ("labeledurl" . eudc-display-url)
                     ("audio" . eudc-display-sound)
                     ("labeleduri" . eudc-display-url)
                     ("mail" . eudc-display-mail)
                     ("url" . eudc-display-url))
                   'ldap)

(eudc-protocol-set 'eudc-attribute-display-method-alist
                   '(("jpegphoto" . eudc-display-jpeg-inline)
                     ("thumbnailphoto" . eudc-display-jpeg-inline)
                     ("labeledurl" . eudc-display-url)
                     ("audio" . eudc-display-sound)
                     ("labeleduri" . eudc-display-url)
                     ("mail" . eudc-display-mail)
                     ("url" . eudc-display-url))
                   )


    ;; Adds some hooks

(eval-after-load "message"
  '(define-key message-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline))
(eval-after-load "sendmail"
  '(define-key mail-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline))
(eval-after-load "post"
  '(define-key post-mode-map (kbd "H-c TAB") 'sharad/enz-eudc-expand-inline)))


(testing
 (eudc-display-jpeg-inline
  (cdaar (remove-if 'null (eudc-query '(("givenName" . "Sharad")) '(thumbnailPhoto))))))



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




(defun eudc-expand-inline (&optional replace)
  "Query the directory server, and expand the query string before point.
The query string consists of the buffer substring from the point back to
the preceding comma, colon or beginning of line.
The variable `eudc-inline-query-format' controls how to associate the
individual inline query words with directory attribute names.
After querying the server for the given string, the expansion specified by
`eudc-inline-expansion-format' is inserted in the buffer at point.
If REPLACE is non-nil, then this expansion replaces the name in the buffer.
`eudc-expansion-overwrites-query' being non-nil inverts the meaning of REPLACE.
Multiple servers can be tried with the same query until one finds a match,
see `eudc-inline-expansion-servers'"
  (interactive)
  (if (memq eudc-inline-expansion-servers
	    '(current-server server-then-hotlist))
      (or eudc-server
	  (call-interactively 'eudc-set-server))
    (or eudc-server-hotlist
	(error "No server in the hotlist")))
  (let* ((end (point))
	 (beg (save-excursion
		(if (re-search-backward "\\([:,]\\|^\\)[ \t]*"
					(save-excursion
					  (beginning-of-line)
					  (point))
					'move)
		    (goto-char (match-end 0)))
		(point)))
	 (query-words (split-string (buffer-substring beg end) "[ \t]+"))
	 query-formats
	 response
	 response-string
	 response-strings
	 (eudc-former-server eudc-server)
	 (eudc-former-protocol eudc-protocol)
	 servers)

    ;; Prepare the list of servers to query
    (setq servers (copy-sequence eudc-server-hotlist))
    (setq servers
	  (cond
	   ((eq eudc-inline-expansion-servers 'hotlist)
	    eudc-server-hotlist)
	   ((eq eudc-inline-expansion-servers 'server-then-hotlist)
	    (cons (cons eudc-server eudc-protocol)
		  (delete (cons eudc-server eudc-protocol) servers)))
	   ((eq eudc-inline-expansion-servers 'current-server)
	    (list (cons eudc-server eudc-protocol)))
	   (t
	    (error "Wrong value for `eudc-inline-expansion-servers': %S"
		   eudc-inline-expansion-servers))))
    (if (and eudc-max-servers-to-query
	     (> (length servers) eudc-max-servers-to-query))
	(setcdr (nthcdr (1- eudc-max-servers-to-query) servers) nil))

    (condition-case signal
	(progn
	  (setq response
		(remove-if
                 #'null
                 (catch 'found
                   ;; Loop on the servers
                   (while servers
                     (eudc-set-server (eudc-caar servers) (eudc-cdar servers) t)

                     ;; Determine which formats apply in the query-format list
                     (setq query-formats
                           (or
                            (eudc-extract-n-word-formats eudc-inline-query-format
                                                         (length query-words))
                            (if (null eudc-protocol-has-default-query-attributes)
                                '(name))))

                     ;; Loop on query-formats
                     (while query-formats
                       (setq response
                             (eudc-query
                              (eudc-format-query query-words (car query-formats))
                              (eudc-translate-attribute-list
                               (cdr eudc-inline-expansion-format))))
                       (if response
                           (throw 'found response))
                       (setq query-formats (cdr query-formats)))
                     (setq servers (cdr servers)))
                   ;; No more servers to try... no match found
                   nil)))


	  (if (null response)
	      (error "No match")

	    ;; Process response through eudc-inline-expansion-format
	    (while response
	      (setq response-string (apply 'format
					   (car eudc-inline-expansion-format)
					   (mapcar (function
						    (lambda (field)
						      (or (cdr (assq field (car response)))
							  "")))
						   (eudc-translate-attribute-list
						    (cdr eudc-inline-expansion-format)))))
	      (if (> (length response-string) 0)
		  (setq response-strings
			(cons response-string response-strings)))
	      (setq response (cdr response)))

	    (if (or
		 (and replace (not eudc-expansion-overwrites-query))
		 (and (not replace) eudc-expansion-overwrites-query))
		(kill-ring-save beg end))
	    (cond
	     ((or (= (length response-strings) 1)
		  (null eudc-multiple-match-handling-method)
		  (eq eudc-multiple-match-handling-method 'first))
	      (delete-region beg end)
	      (insert (car response-strings)))
	     ((eq eudc-multiple-match-handling-method 'select)
	      (eudc-select response-strings beg end))
	     ((eq eudc-multiple-match-handling-method 'all)
	      (delete-region beg end)
	      (insert (mapconcat 'identity response-strings ", ")))
	     ((eq eudc-multiple-match-handling-method 'abort)
	      (error "There is more than one match for the query"))))
	  (or (and (equal eudc-server eudc-former-server)
		   (equal eudc-protocol eudc-former-protocol))
	      (eudc-set-server eudc-former-server eudc-former-protocol t)))
      (t
       (or (and (equal eudc-server eudc-former-server)
		(equal eudc-protocol eudc-former-protocol))
	   (eudc-set-server eudc-former-server eudc-former-protocol t))
       (signal (car signal) (cdr signal))))))

(provide 'eudc-config)
;;; eudc.el ends here
