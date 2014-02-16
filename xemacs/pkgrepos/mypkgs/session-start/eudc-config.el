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

(defun myeudc-ldap-format-query-as-rfc1558 (query)
  "Format the EUDC QUERY list as a RFC1558 LDAP search filter."
  (format "(|%s)"
	  (apply 'concat
		 (mapcar '(lambda (item)
			    (format "(%s=%s)"
				    (car item)
				    (eudc-ldap-escape-query-special-chars (cdr item))))
			 query))))

  (defun broad-eudc-ldap-get-field-list (dummy &optional objectclass)
  "Return a list of valid attribute names for the current server.
OBJECTCLASS is the LDAP object class for which the valid
attribute names are returned. Default to `person'"
  (interactive)
  (or eudc-server
      (call-interactively 'eudc-set-server))
  (let ((ldap-host-parameters-alist
	 (list (cons eudc-server
		     '(scope subtree sizelimit 1)))))
    (mapcar 'eudc-ldap-cleanup-record-simple
	    (ldap-search
	     (myeudc-ldap-format-query-as-rfc1558
	      (list
               (cons "objectClass" "person")
               (cons "objectClass" "group")))
	     eudc-server nil t))))

(eudc-protocol-set 'eudc-list-attributes-function 'broad-eudc-ldap-get-field-list
		   'ldap)


  (defun eudc-ldap-datas ()
    (or (and (boundp 'eudc-ldap-datas) eudc-ldap-datas) '()))

  (setq ldap-default-base "ou=addressbook,dc=your_dc_here,dc=fr")

(setq ;; eudc-default-return-attributes nil
 eudc-default-return-attributes 'all
 eudc-strict-return-matches nil
 ldap-ldapsearch-args '("-tt" "-LLL" "-x")
 eudc-ldap-attributes-translation-alist
 '((adname . name)
   (name . sn)
   (firstname . givenname)
   (email . mail)
   (phone . telephonenumber))

 eudc-inline-query-format '(
                            (displayName)
                            (adname)
                            (mailNickname)
                            (givenName)
                            (sn)
                            (givenName sn)
                            (mail)    ;note email have been changed to mail
                            (name)
                            )
   ;; eudc-inline-query-format nil
 eudc-inline-expansion-format '("%s %s <%s>" givenName name mail)
 eudc-inline-expansion-formats '(
                                 ("%s <%s>" displayName mail)
                                 ("%s <%s>" name mail)
                                 ("%s %s <%s>" givenName name mail)
                                 ("%s <%s>" adname mail)
                                 ("%s <%s>" name mail)
                                 ("%s" mail))
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

(eudc-protocol-set 'eudc-protocol-attributes-translation-alist
		   'eudc-ldap-attributes-translation-alist 'ldap)

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


;; (testing
;;  (eudc-display-jpeg-inline
;;   (cdaar (remove-if 'null (eudc-query '(("givenName" . "Sharad")) '(thumbnailPhoto))))))



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
	 ;; (query-words (split-string (buffer-substring beg end) "[ \t]+"))
         ;; for partial names.
	 (query-words (mapcar
                       '(lambda (w) (concat "*" w "*"))
                       (split-string (buffer-substring beg end) "[ \t]+")))
	 query-formats
         inline-expansion-formats
         inline-expansion-format
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
                       (setq inline-expansion-formats
                             (if eudc-inline-expansion-formats
                                 eudc-inline-expansion-formats
                                 (if eudc-inline-expansion-format
                                     (list eudc-inline-expansion-format))))
                       (setq inline-expansion-format (car inline-expansion-formats))
                       (dmessage 'eudc 7 "X: inline-expansion-formats %s\nX: query-formats %s\n" inline-expansion-formats query-formats)
                       (while inline-expansion-formats
                         (setq response
                               (eudc-query
                                (eudc-format-query query-words (car query-formats))
                                (eudc-translate-attribute-list
                                 (cdr inline-expansion-format))))
                         (dmessage 'eudc 7 "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                         (setq response (remove nil response))
                         (dmessage 'eudc 7 "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                         (if response
                             (throw 'found response))
                         (setq inline-expansion-formats (cdr inline-expansion-formats))
                         (setq inline-expansion-format (car inline-expansion-formats)))

                       (dmessage 'eudc 7 "Z: inline-expansion-formats %s\nZ: query-formats %s\n" inline-expansion-formats query-formats)

                       (setq query-formats (cdr query-formats)))
                     (setq servers (cdr servers)))
                   ;; No more servers to try... no match found
                   nil)))
          (dmessage 'eudc 7 "Y: inline-expansion-format %s\nY: response %s\nY: query-formats %s\n" inline-expansion-format response query-formats)

	  (if (null response)
	      (error "No match")


              (dmessage 'eudc 7 "N: inline-expansion-format %s\nN: response %s\n" inline-expansion-format response)
              ;; Process response through eudc-inline-expansion-format
              (while response

                (let ((carresp (mapcar 'car (car response)))
                      (formats eudc-inline-expansion-formats))
                  (setq inline-expansion-format
                        (or
                         (catch 'fmtfound
                           (while formats
                             (dmessage 'eudc 7 "L: format %s\nL: carresp %s\nL:diff %s\n"
                                       (eudc-translate-attribute-list
                                        (cdr (car formats)))
                                       carresp
                                       (set-exclusive-or
                                        carresp
                                        (eudc-translate-attribute-list
                                         (cdr (car formats)))))
                             (unless (set-exclusive-or
                                      carresp
                                      (eudc-translate-attribute-list
                                       (cdr (car formats))))
                               (throw 'fmtfound (car formats)))
                             (dmessage 'eudc 7 "P: format %s\nP: carresp %s\n"
                                       (eudc-translate-attribute-list
                                        (cdr (car formats)))
                                       carresp)
                             (setq formats (cdr formats))
                             nil))
                       inline-expansion-format)))


                (setq response-string (apply 'format
                                             (car inline-expansion-format)
                                             (mapcar (function
                                                      (lambda (field)
                                                       (or (cdr (assq field (car response)))
                                                           "")))
                                                     (eudc-translate-attribute-list
                                                      (cdr inline-expansion-format)))))
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



(defun email-eudc-query-form (&optional attribute-value-alist get-fields-from-server)
  "Display a form to query the directory server.
If given a non-nil argument GET-FIELDS-FROM-SERVER, the function first
queries the server for the existing fields and displays a corresponding form."
  (interactive "P")
  (let ((fields (or (and get-fields-from-server
			 (eudc-get-attribute-list))
		    eudc-query-form-attributes))
	(buffer (get-buffer-create "*Directory Query Form*"))
	prompts
	widget
	(width 0)
	inhibit-read-only
	pt)
    (switch-to-buffer buffer)
    (setq inhibit-read-only t)
    (erase-buffer)
    (kill-all-local-variables)
    (make-local-variable 'eudc-form-widget-list)
    (widget-insert "Directory Query Form\n")
    (widget-insert "====================\n\n")
    (widget-insert "Current server is: " (or eudc-server
					     (progn
					       (call-interactively 'eudc-set-server)
					       eudc-server))
                   "\n")
    (widget-insert "Protocol         : " (symbol-name eudc-protocol) "\n")
    ;; Build the list of prompts
    (setq prompts (if eudc-use-raw-directory-names
		      (mapcar 'symbol-name (eudc-translate-attribute-list fields))
                      (mapcar (function
                               (lambda (field)
                                (or (and (assq field eudc-user-attribute-names-alist)
                                         (cdr (assq field eudc-user-attribute-names-alist)))
                                    (capitalize (symbol-name field)))))
                              fields)))
    ;; Loop over prompt strings to find the longest one
    (mapc (function
	   (lambda (prompt)
            (if (> (length prompt) width)
                (setq width (length prompt)))))
	  prompts)
    ;; Insert the first widget out of the mapcar to leave the cursor
    ;; in the first field
    (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
    (setq pt (point))
    (setq widget (widget-create 'editable-field :size 15))
    (setq eudc-form-widget-list (cons (cons (car fields) widget)
				      eudc-form-widget-list))
    (setq fields (cdr fields))
    (setq prompts (cdr prompts))
    (mapc (function
	   (lambda (field)
            (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
            (setq widget (widget-create 'editable-field
                                        :size 15
                                        (cdr (assoc field attribute-value-alist))))
            (setq eudc-form-widget-list (cons (cons field widget)
                                              eudc-form-widget-list))
            (setq prompts (cdr prompts))))
	  fields)
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-process-form))
		   "Query Server")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-query-form))
		   "Reset Form")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-this-buffer))
		   "Quit")
    (goto-char pt)
    (use-local-map widget-keymap)
    (widget-setup))
  )



(defun eudc-query-form (&optional get-fields-from-server)
  "Display a form to query the directory server.
If given a non-nil argument GET-FIELDS-FROM-SERVER, the function first
queries the server for the existing fields and displays a corresponding form."
  (interactive "P")
  (let ((fields (or (and get-fields-from-server
			 (eudc-get-attribute-list))
		    eudc-query-form-attributes))
	(buffer (get-buffer-create "*Directory Query Form*"))
	prompts
	widget
	(width 0)
	inhibit-read-only
	pt)
    (switch-to-buffer buffer)
    (setq inhibit-read-only t)
    (erase-buffer)
    (kill-all-local-variables)
    (make-local-variable 'eudc-form-widget-list)
    (widget-insert "Directory Query Form\n")
    (widget-insert "====================\n\n")
    (widget-insert "Current server is: " (or eudc-server
					     (progn
					       (call-interactively 'eudc-set-server)
					       eudc-server))
					     "\n")
    (widget-insert "Protocol         : " (symbol-name eudc-protocol) "\n")
    ;; Build the list of prompts
    (setq prompts (if eudc-use-raw-directory-names
		      (mapcar 'symbol-name (eudc-translate-attribute-list fields))
		    (mapcar (function
			     (lambda (field)
			       (or (and (assq field eudc-user-attribute-names-alist)
					(cdr (assq field eudc-user-attribute-names-alist)))
				   (capitalize (symbol-name field)))))
			    fields)))
    ;; Loop over prompt strings to find the longest one
    (mapc (function
	   (lambda (prompt)
	     (if (> (length prompt) width)
		 (setq width (length prompt)))))
	  prompts)
    ;; Insert the first widget out of the mapcar to leave the cursor
    ;; in the first field
    (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
    (setq pt (point))
    (setq widget (widget-create 'editable-field :size 15))
    (setq eudc-form-widget-list (cons (cons (car fields) widget)
				      eudc-form-widget-list))
    (setq fields (cdr fields))
    (setq prompts (cdr prompts))
    (mapc (function
	   (lambda (field)
	     (widget-insert "\n\n" (format (concat "%" (int-to-string width) "s: ") (car prompts)))
	     (setq widget (widget-create 'editable-field
					 :size 15))
	     (setq eudc-form-widget-list (cons (cons field widget)
					       eudc-form-widget-list))
	     (setq prompts (cdr prompts))))
	  fields)
    (widget-insert "\n\n")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-process-form))
		   "Query Server")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (eudc-query-form))
		   "Reset Form")
    (widget-insert " ")
    (widget-create 'push-button
		   :notify (lambda (&rest ignore)
			     (kill-this-buffer))
		   "Quit")
    (goto-char pt)
    (use-local-map widget-keymap)
    (widget-setup))
  )

(provide 'eudc-config)
;;; eudc.el ends here
