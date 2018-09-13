;;; packages.el --- lotus-eudc layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-eudc-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-eudc/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-eudc/pre-init-PACKAGE' and/or
;;   `lotus-eudc/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-eudcS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-eudc-packages
  '(
    ;; (PACKAGE :location local)
    (passwds :location local)
    eudc
    ldap
    )
  "The list of Lisp packages required by the lotus-eudc layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-eudc/init-passwds ()
  (use-package passwds
      :defer t
      :config
      (progn
        )))

(defun lotus-eudc/init-eudc ()
  ;; from http://www.emacswiki.org/emacs/EUDC
  (use-package eudc
      :defer t
      :commands (lotus-enz-eudc-expand-inline)
      :config
      (progn
       (progn
         (defun myeudc-ldap-format-query-as-rfc1558 (query)
           "Format the EUDC QUERY list as a RFC1558 LDAP search filter."
           (format "(|%s)"
                   (apply 'concat
                          (mapcar #'(lambda (item)
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

         (when nil
          (eudc-protocol-set 'eudc-list-attributes-function 'broad-eudc-ldap-get-field-list
                            'ldap))

         ;; (setq eudc-office 'meru)
         (setq eudc-office 'fortinet)

         (require 'passwds)
         (defun eudc-ldap-datas ()
           (or (and (boundp 'eudc-ldap-datas) eudc-ldap-datas) '()))

         (setq ldap-default-base "cn=accounts,dc=corp,dc=fortinet,dc=com")

         (setq
          ;; eudc-default-return-attributes nil
          eudc-default-return-attributes 'all
          eudc-strict-return-matches nil
          ;; ldap-ldapsearch-args '("-tt" "-LLL" "-x" "-ZZ")
          ;; ldap-ldapsearch-args '()
          ;; ldap-ldapsearch-args '("-tt" "-LLL" "-x")
          ldap-ldapsearch-args '("-tt" "-LLL" "-x")
          eudc-ldap-attributes-translation-alist
          '((uid . uid)
            (adname . name)
            (name . sn)
            (firstname . givenName)
            (email . mail)
            (phone . telephoneNumber))

          eudc-inline-query-format '(
                                     (uid)
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
                                          ("%s <%s>" uid mail)
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
          `(,(cdr (assoc eudc-office (eudc-ldap-datas))))

          eudc-server-hotlist `(
                                ("" . bbdb)
                                ( ,(car (cdr (assoc eudc-office (eudc-ldap-datas)))) . ldap )
                                ("" . bbdb) )

          eudc-inline-expansion-servers 'hotlist)

         (when nil
           (eudc-protocol-set 'eudc-protocol-attributes-translation-alist
                              'eudc-ldap-attributes-translation-alist 'ldap))

         (eudc-set-server (car (cdr (assoc eudc-office (eudc-ldap-datas)))) 'ldap t)

         (when t
           (setq
            eudc-ldap-attributes-translation-alist
            '((adname . uid)
              (name . sn)
              (firstname . givenName)
              (email . mail)
              (displayName . displayname)
              (phone . telephoneNumber))

            eudc-inline-query-format '(
                                       (displayName)
                                       ;; (adname)
                                       ;; (mailNickname)
                                       (uid)
                                       (givenName)
                                       (sn)
                                       (givenName sn)
                                       (mail)    ;note email have been changed to mail
                                       ;; (name)
                                       )))

         (defun enz-eudc-expand-inline ()
           (interactive)
           (move-end-of-line 1)
           (insert "*")
           (unless (condition-case nil
                       (eudc-expand-inline)
                     (error nil))
             (backward-delete-char-untabify 1)))


         (defun lotus-enz-eudc-expand-inline ()
           (interactive)
           (move-end-of-line 1)
           (insert "*")
           (unless (condition-case err
                       (eudc-expand-inline)
                     (error (message "%s" err)))
             (progn
               (backward-delete-char-untabify 1))))



         (defun lotus-eudc-show-at-point (arg)
           (interactive "P")
           ;; from .gnus.d/article.el
           (let ((email (thing-at-point 'fullemail)))
             (if email
                 (eudc-display-records (eudc-query (list (cons 'mail (car (ietf-drums-parse-address email)))) ) arg)
                 (message "Not able to parse any email at point."))))

         (defun eudc-select (choices beg end)
           "Choose one from CHOICES using a completion.
BEG and END delimit the text which is to be replaced."
           (let ((replacement)
                 ;; (complete-fn 'completing-read)
                 (complete-fn
                  (if (< (length choices) 10)
                      'ido-completing-read
                      'completing-read)))
             (setq replacement
                   (funcall complete-fn "Multiple matches found; choose one: "
                            (mapcar 'list choices)))
             (delete-region beg end)
             (insert replacement)))


         (when nil

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
                              ))


         ;; Adds some hooks

         (eval-after-load "message"
           '(define-key message-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline))
         (eval-after-load "sendmail"
           '(define-key mail-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline))
         (eval-after-load "post"
           '(define-key post-mode-map (kbd "H-c TAB") 'lotus-enz-eudc-expand-inline)))


       (progn

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
           (cond
             ((eq eudc-inline-expansion-servers 'current-server)
              (or eudc-server
                  (call-interactively 'eudc-set-server)))
             ((eq eudc-inline-expansion-servers 'server-then-hotlist)
              (or eudc-server
                  ;; Allow server to be nil if hotlist is set.
                  eudc-server-hotlist
                  (call-interactively 'eudc-set-server)))
             ((eq eudc-inline-expansion-servers 'hotlist)
              (or eudc-server-hotlist
                  (error "No server in the hotlist")))
             (t
              (error "Wrong value for `eudc-inline-expansion-servers': %S"
                     eudc-inline-expansion-servers)))
           (let* ((end (point))
                  (beg (save-excursion
                         (if (re-search-backward "\\([:,]\\|^\\)[ \t]*"
                                                 (point-at-bol) 'move)
                             (goto-char (match-end 0)))
                         (point)))
                  (query-words (split-string (buffer-substring-no-properties beg end)
                                             "[ \t]+"))

                  ;; (query-words (mapcar
                  ;;               #'(lambda (w) (concat "*" w "*"))
                  ;;               (split-string (buffer-substring-no-properties beg end)
                  ;;                             "[ \t]+")))
                  query-formats
                  response

                  inline-expansion-formats
                  inline-expansion-format


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
                      (if eudc-server
                          (cons (cons eudc-server eudc-protocol)
                                (delete (cons eudc-server eudc-protocol) servers))
                        eudc-server-hotlist))
                     ((eq eudc-inline-expansion-servers 'current-server)
                      (list (cons eudc-server eudc-protocol)))))
             (if (and eudc-max-servers-to-query
                      (> (length servers) eudc-max-servers-to-query))
                 (setcdr (nthcdr (1- eudc-max-servers-to-query) servers) nil))

             (unwind-protect
                  (progn

                    (if nil
                        (setq response  ;old
                              (catch 'found
                                ;; Loop on the servers
                                (while servers
                                  (eudc-set-server (caar servers) (cdar servers) t)

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
                                nil))

                      (setq response
                            (remove-if
                             #'null
                             (catch 'found
                               ;; Loop on the servers
                               (while servers
                                 (eudc-set-server (caar servers) (cdar servers) t)

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
                                   (lwarn 'eudc :debug "X: inline-expansion-formats %s\nX: query-formats %s\n" inline-expansion-formats query-formats)
                                   (while inline-expansion-formats
                                     (setq response
                                           (eudc-query
                                            (eudc-format-query query-words (car query-formats))
                                            (eudc-translate-attribute-list
                                             (cdr inline-expansion-format))))
                                     (lwarn 'eudc :debug "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                                     (setq response (remove nil response))
                                     (lwarn 'eudc :debug "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                                     (if response
                                         (throw 'found response))
                                     (setq inline-expansion-formats (cdr inline-expansion-formats))
                                     (setq inline-expansion-format (car inline-expansion-formats)))

                                   (lwarn 'eudc :debug "Z: inline-expansion-formats %s\nZ: query-formats %s\n" inline-expansion-formats query-formats)

                                   (setq query-formats (cdr query-formats)))
                                 (setq servers (cdr servers)))
                               ;; No more servers to try... no match found
                               nil)
                             )))

                    (if nil
                        (if (null response) ;old
                            (error "No match")

                          (lwarn 'eudc :debug "Z: response= %s\n" response)
                          ;; Process response through eudc-inline-expansion-format
                          (while response
                            (setq response-string
                                  (apply 'format
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

                      (if (null response)
                          (error "No match")

                        (lwarn 'eudc :debug "Z: result => response= %s\n" response)
                        (lwarn 'eudc :debug "N: inline-expansion-format %s\nN: response %s\n" inline-expansion-format response)
                        ;; Process response through eudc-inline-expansion-format
                        (while response


                          (let ((carresp (mapcar 'car (car response)))
                                (formats eudc-inline-expansion-formats))
                            (lwarn 'eudc :debug "X: carresp = %s, formats = %s" carresp formats)
                            (setq inline-expansion-format
                                  (or
                                   (catch 'fmtfound
                                     (while formats
                                       (lwarn 'eudc :debug "L: format %s\nL: carresp %s\nL:diff %s\n"
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
                                       (lwarn 'eudc :debug "P: format %s\nP: carresp %s\n"
                                              (eudc-translate-attribute-list
                                               (cdr (car formats)))
                                              carresp)
                                       (setq formats (cdr formats))
                                       nil))
                                   inline-expansion-format)))


                          (lwarn 'eudc :debug "S: using format inline-expansion-format = %s" inline-expansion-format)
                          (setq response-string (apply 'format
                                                       (car inline-expansion-format)
                                                       (mapcar (function
                                                                (lambda (field)
                                                                 (or (cdr (assq field (car response)))
                                                                     "")))
                                                               (eudc-translate-attribute-list
                                                                (cdr inline-expansion-format)))))
                          (when (> (length response-string) 0)
                            (progn
                              (lwarn 'eudc :debug "adding response-string %s in list" response-string)
                              (setq response-strings
                                    (cons response-string response-strings))))
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
                           (error "There is more than one match for the query"))))))
               (or (and (equal eudc-server eudc-former-server)
                        (equal eudc-protocol eudc-former-protocol))
                   (eudc-set-server eudc-former-server eudc-former-protocol t)))))


         )

       (progn ;; wrapper old

         (defun eudc-expand-inline-old (&optional replace)
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
                                #'(lambda (w) (concat "*" w "*"))
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
                              (eudc-set-server (caar servers) (cdar servers) t)

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
                                (lwarn 'eudc :debug "X: inline-expansion-formats %s\nX: query-formats %s\n" inline-expansion-formats query-formats)
                                (while inline-expansion-formats
                                  (setq response
                                        (eudc-query
                                         (eudc-format-query query-words (car query-formats))
                                         (eudc-translate-attribute-list
                                          (cdr inline-expansion-format))))
                                  (lwarn 'eudc :debug "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                                  (setq response (remove nil response))
                                  (lwarn 'eudc :debug "O: inline-expansion-formats %s\nO: query-formats %s\n response %s\n" inline-expansion-formats query-formats response)
                                  (if response
                                      (throw 'found response))
                                  (setq inline-expansion-formats (cdr inline-expansion-formats))
                                  (setq inline-expansion-format (car inline-expansion-formats)))

                                (lwarn 'eudc :debug "Z: inline-expansion-formats %s\nZ: query-formats %s\n" inline-expansion-formats query-formats)

                                (setq query-formats (cdr query-formats)))
                              (setq servers (cdr servers)))
                            ;; No more servers to try... no match found
                            nil)))
                   (lwarn 'eudc :debug "Y: inline-expansion-format %s\nY: response %s\nY: query-formats %s\n" inline-expansion-format response query-formats)

                   (if (null response)
                       (error "No match")


                       (lwarn 'eudc :debug "N: inline-expansion-format %s\nN: response %s\n" inline-expansion-format response)
                       ;; Process response through eudc-inline-expansion-format
                       (while response

                         (let ((carresp (mapcar 'car (car response)))
                               (formats eudc-inline-expansion-formats))
                           (setq inline-expansion-format
                                 (or
                                  (catch 'fmtfound
                                    (while formats
                                      (lwarn 'eudc :debug "L: format %s\nL: carresp %s\nL:diff %s\n"
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
                                      (lwarn 'eudc :debug "P: format %s\nP: carresp %s\n"
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

         )

       (progn
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



         ))))

(defun lotus-eudc/init-ldap ()
  (use-package ldap
      :defer t
      :config
      (progn
        (progn ;; wrapper
          (defun ldap-search-internal (search-plist)
            "Perform a search on a LDAP server.
SEARCH-PLIST is a property list describing the search request.
Valid keys in that list are:

  `auth-source', if non-nil, will use `auth-source-search' and
will grab the :host, :secret, :base, and (:user or :binddn)
tokens into the `host', `passwd', `base', and `binddn' parameters
respectively if they are not provided in SEARCH-PLIST.  So for
instance *each* of these netrc lines has the same effect if you
ask for the host \"ldapserver:2400\":

  machine ldapserver:2400 login myDN secret myPassword base myBase
  machine ldapserver:2400 binddn myDN secret myPassword port ldap
  login myDN secret myPassword base myBase

but if you have more than one in your netrc file, only the first
matching one will be used.  Note the \"port ldap\" part is NOT
required.

  `host' is a string naming one or more (blank-separated) LDAP servers
to try to connect to.  Each host name may optionally be of the form HOST:PORT.
  `filter' is a filter string for the search as described in RFC 1558.
  `attributes' is a list of strings indicating which attributes to retrieve
for each matching entry. If nil, return all available attributes.
  `attrsonly', if non-nil, indicates that only attributes are retrieved,
not their associated values.
  `auth' is one of the symbols `simple', `krbv41' or `krbv42'.
  `base' is the base for the search as described in RFC 1779.
  `scope' is one of the three symbols `sub', `base' or `one'.
  `binddn' is the distinguished name of the user to bind as (in RFC 1779 syntax).
  `auth' is one of the symbols `simple', `krbv41' or `krbv42'
  `passwd' is the password to use for simple authentication.
  `deref' is one of the symbols `never', `always', `search' or `find'.
  `timelimit' is the timeout limit for the connection in seconds.
  `sizelimit' is the maximum number of matches to return.
  `withdn' if non-nil each entry in the result will be prepended with
its distinguished name DN.
The function returns a list of matching entries.  Each entry is itself
an alist of attribute/value pairs."
            (let* ((buf (get-buffer-create " *ldap-search*"))
                   (bufval (get-buffer-create " *ldap-value*"))
                   (host (or (plist-get search-plist 'host)
                             ldap-default-host))
                   (uri (plist-get search-plist :uri))
                   ;; find entries with port "ldap" that match the requested host if any
                   (asfound (when (plist-get search-plist 'auth-source)
                              (nth 0 (auth-source-search :host (or host t)
                                                         :create t))))
                   ;; if no host was requested, get it from the auth-source entry
                   (host (or host (plist-get asfound :host)))
                   ;; get the password from the auth-source
                   (passwd (or (plist-get search-plist 'passwd)
                               (plist-get asfound :secret)))
                   ;; convert the password from a function call if needed
                   (passwd (if (functionp passwd) (funcall passwd) passwd))
                   ;; get the binddn from the search-list or from the
                   ;; auth-source user or binddn tokens
                   (binddn (or (plist-get search-plist 'binddn)
                               (plist-get asfound :user)
                               (plist-get asfound :binddn)))
                   (base (or (plist-get search-plist 'base)
                             (plist-get asfound :base)
                             ldap-default-base))
                   (filter (plist-get search-plist 'filter))
                   (attributes (plist-get search-plist 'attributes))
                   (attrsonly (plist-get search-plist 'attrsonly))
                   (scope (plist-get search-plist 'scope))
                   (auth (plist-get search-plist 'auth))
                   (deref (plist-get search-plist 'deref))
                   (timelimit (plist-get search-plist 'timelimit))
                   (sizelimit (plist-get search-plist 'sizelimit))
                   (withdn (plist-get search-plist 'withdn))
                   (numres 0)
                   arglist dn name value record result)
              (if (or (null filter)
                      (equal "" filter))
                  (error "No search filter"))
              (setq filter (cons filter attributes))
              (with-current-buffer buf
                (erase-buffer)
                (if (and host
                         (not (equal "" host)))
                    (setq arglist
                          (nconc
                           arglist
                           (list (format "-%s%s" (if (string-match "^ldap[s]?://" host) "H" "h") host)))))
                (if (and uri
                         (not (equal "" uri)))
                    (setq arglist (nconc arglist (list (format "-H%s" uri)))))
                (if (and attrsonly
                         (not (equal "" attrsonly)))
                    (setq arglist (nconc arglist (list "-A"))))
                (if (and base
                         (not (equal "" base)))
                    (setq arglist (nconc arglist (list (format "-b%s" base)))))
                (if (and scope
                         (not (equal "" scope)))
                    (setq arglist (nconc arglist (list (format "-s%s" scope)))))
                (if (and binddn
                         (not (equal "" binddn)))
                    (setq arglist (nconc arglist (list (format "-D%s" binddn)))))
                (if (and auth
                         (equal 'simple auth))
                    (setq arglist (nconc arglist (list "-x"))))
                (if (and passwd
                         (not (equal "" passwd)))
                    (setq arglist (nconc arglist (list (format "-w%s" passwd)))))
                (if (and deref
                         (not (equal "" deref)))
                    (setq arglist (nconc arglist (list (format "-a%s" (if (symbolp deref) (symbol-name deref) deref))))))
                (if (and timelimit
                         (not (equal "" timelimit)))
                    (setq arglist (nconc arglist (list (format "-l%s" timelimit)))))
                (if (and sizelimit
                         (not (equal "" sizelimit)))
                    (setq arglist (nconc arglist (list (format "-z%s" sizelimit)))))

                ;; (message "%s %s" ldap-ldapsearch-prog
                ;;          (append arglist ldap-ldapsearch-args filter))
                (apply #'call-process ldap-ldapsearch-prog
                       ;; Ignore stderr, which can corrupt results
                       nil (list buf nil) nil
                       (append arglist ldap-ldapsearch-args filter))
                (insert "\n")
                (goto-char (point-min))

                (while (re-search-forward "[\t\n\f]+ " nil t)
                  (replace-match "" nil nil))
                (goto-char (point-min))

                (if (looking-at "usage")
                    (error "Incorrect ldapsearch invocation")
                    (message "Parsing results... ")
                    ;; Skip error message when retrieving attribute list
                    (if (looking-at "Size limit exceeded")
                        (forward-line 1))
                    (if (looking-at "version:") (forward-line 1)) ;bug#12724.
                    (while (progn
                             (skip-chars-forward " \t\n")
                             (not (eobp)))
                      (setq dn (buffer-substring (point) (point-at-eol)))
                      (forward-line 1)
                      (while (looking-at "^\\([A-Za-z][-A-Za-z0-9]*\
\\|[0-9]+\\(?:\\.[0-9]+\\)*\\)\\(;[-A-Za-z0-9]+\\)*[=:\t ]+\
\\(<[\t ]*file://\\)\\(.*\\)$")
                        (setq name (match-string 1)
                              value (match-string 4))
                        ;; Need to handle file:///D:/... as generated by OpenLDAP
                        ;; on DOS/Windows as local files.
                        (if (and (memq system-type '(windows-nt ms-dos))
                                 (eq (string-match "/\\(.:.*\\)$" value) 0))
                            (setq value (match-string 1 value)))
                        ;; Do not try to open non-existent files
                        (if (equal value "")
                            (setq value " ")
                            (with-current-buffer bufval
                              (erase-buffer)
                              (set-buffer-multibyte nil)
                              (insert-file-contents-literally value)
                              (delete-file value)
                              (setq value (buffer-string))))
                        (setq record (cons (list name value)
                                           record))
                        (forward-line 1))
                      (cond (withdn
                             (push (cons dn (nreverse record)) result))
                            (record
                             (push (nreverse record) result)))
                      (setq record nil)
                      (skip-chars-forward " \t\n")
                      (message "Parsing results... %d" numres)
                      (1+ numres))
                    (message "Parsing results... done")
                    (nreverse result)))))))))

;;; packages.el ends here
