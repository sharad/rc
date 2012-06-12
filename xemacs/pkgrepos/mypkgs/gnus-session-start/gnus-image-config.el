;;; gnus-image.el --- Gnus Gravatar support

;; Copyright (C) 2010-2011 Free Software Foundation, Inc.

;; Author: Julien Danjou <julien@danjou.info>
;; Keywords: news

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;; Code:

(require 'gravatar)
(require 'gnus-art)
(require 'mail-extr) ;; Because of binding `mail-extr-disable-voodoo'.

(defgroup gnus-image nil
  "Gnus Image."
  :group 'gnus-visual)

(defcustom gnus-image-size nil
  "How big should images be displayed.
If nil, default to `image-size'."
  :type 'integer
  :version "24.1"
  :group 'gnus-image)

(defcustom gnus-image-properties '(:ascent center :relief 1)
  "List of image properties applied to Image images."
  :type 'list
  :version "24.1"
  :group 'gnus-image)

(defcustom gnus-image-too-ugly gnus-article-x-face-too-ugly
  "Regexp matching posters whose avatar shouldn't be shown automatically."
  :type '(choice regexp (const nil))
  :version "24.1"
  :group 'gnus-image)

(defvar gnus-image-display-funcs nil "Functions to display images")


(defun gnus-image-transform-address (header category fun &optional force)
  (gnus-with-article-headers
    (let* ((mail-extr-disable-voodoo t)
           (mail-extr-ignore-realname-equals-mailbox-name nil)
	   (addresses (mail-extract-address-components
		       (or (mail-fetch-field header) "") t))
	   (image-size (or gnus-image-size gravatar-size))
	   name)
      (dolist (address addresses)
	(when (and (setq name (car address))
		   (string-match "\\` +" name))
	  (setcar address (setq name (substring name (match-end 0)))))
	(when (or force
		  (not (and gnus-image-too-ugly
			    (or (string-match gnus-image-too-ugly
					      (or (cadr address) ""))
				(and name
				     (string-match gnus-image-too-ugly
						   name))))))
	  ;; (ignore-errors
	    (funcall fun
	     (cadr address)
	     'gnus-image-insert
	     (list header address category)))))))
;;)

;; (image-retrieve)
;; (gravatar-retrieve)

(defun gnus-image-insert (image header address category)
  "Insert IMAGE for ADDRESS in HEADER in current article buffer.
Set image category to CATEGORY."
  (unless (eq image 'error)
    (gnus-with-article-buffer
      (let ((mark (point-marker))
	    (inhibit-point-motion-hooks t)
	    (case-fold-search t))
	(save-restriction
	  (article-narrow-to-head)
	  ;; The buffer can be gone at this time
	  (when (buffer-live-p (current-buffer))
	    (gnus-article-goto-header header)
	    (mail-header-narrow-to-field)
	    (let ((real-name (car address))
		  (mail-address (cadr address)))
	      (when (if real-name
			(re-search-forward
			 (concat (gnus-replace-in-string
				  (regexp-quote real-name) "[\t ]+" "[\t\n ]+")
				 "\\|"
				 (regexp-quote mail-address))
			 nil t)
		      (search-forward mail-address nil t))
		(goto-char (1- (match-beginning 0)))
		;; If we're on the " quoting the name, go backward
		(when (looking-at "[\"<]")
		  (goto-char (1- (point))))
		;; Do not do anything if there's already a image. This can
		;; happens if the buffer has been regenerated in the mean time, for
		;; example we were fetching someaddress, and then we change to
		;; another mail with the same someaddress.
		(unless (memq 'gnus-image (text-properties-at (point)))
		  (let ((point (point)))
		    (unless (featurep 'xemacs)
		      (setq image (append image gnus-image-properties)))
		    (gnus-put-image image (buffer-substring (point) (1+ point)) category)
		    (put-text-property point (point) 'gnus-image address)
		    (gnus-add-wash-type category)
		    (gnus-add-image category image)))))))
	(goto-char (marker-position mark))))))

;;;###autoload
(defun gnus-treat-from-image (&optional force)
  "Display image in the From header.
If image is already displayed, remove it."
  (interactive (list t)) ;; When type `W D g'
  (gnus-with-article-buffer
    (if (memq 'from-image gnus-article-wash-types)
	(gnus-delete-images 'from-image)
        (loop for fun in gnus-image-display-funcs
           do (gnus-image-transform-address "from" 'from-image fun force)))))

;;;###autoload
(defun gnus-treat-mail-image (&optional force)
  "Display images in the Cc and To headers.
If images are already displayed, remove them."
  (interactive (list t)) ;; When type `W D h'
    (gnus-with-article-buffer
      (if (memq 'mail-image gnus-article-wash-types)
          (gnus-delete-images 'mail-image)
          (loop for fun in gnus-image-display-funcs
             do (progn
                  (gnus-image-transform-address "cc" 'mail-image fun force)
                  (gnus-image-transform-address "to" 'mail-image fun force))))))



;; image-retrieve
;; or gravatar-retrieve

(testing

 (eudc-display-jpeg-inline
  (with-temp-buffer
    (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))))

 (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))

 )




(testing
 (defun gravatar-retrieve (mail-address cb &optional cbargs)
  "Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS."
  (let ((url (gravatar-build-url mail-address)))
    (if (gravatar-cache-expired url)
	(let ((args (list url
			  'gravatar-retrieved
			  (list cb (when cbargs cbargs)))))
	  (when (> (length (if (featurep 'xemacs)
			       (cdr (split-string (function-arglist 'url-retrieve)))
			     (help-function-arglist 'url-retrieve)))
		   4)
	    (setq args (nconc args (list t))))
	  (apply #'url-retrieve args))
      (apply cb
               (with-temp-buffer
                 (mm-disable-multibyte)
                 (url-cache-extract (url-cache-create-filename url))
                 (gravatar-data->image))
               cbargs)))))


;; (defun image-dired-create-thumb (original-file thumbnail-file)

;; (defmacro with-string-as-temp-file (str if &optional of &body body)
;;   `(let ((if ,(make-temp-file "thumb"))
;;          (of ,(make-temp-file "thumb")))
;;      (append-to-file ,str nil if)
;;      (
;;           (find-file of)
;;        )
;;      ))

;; (defun image-dired-create-thumb (original-file thumbnail-file)



(defun eudc-ldap-retrieve (mail-address cb &optional cbargs)
  "Retrieve MAIL-ADDRESS gravatar and call CB on retrieval.
You can provide a list of argument to pass to CB in CBARGS."
  (apply cb
         (create-image
          (cdaar (remove-if 'null (eudc-query (list (cons 'mail mail-address)) '(thumbnailPhoto) t)))
          'jpeg t)
         cbargs))

(add-to-list 'gnus-image-display-funcs #'eudc-ldap-retrieve)
(add-to-list 'gnus-image-display-funcs #'gravatar-retrieve)

;; (defun image-dired-create-thumb (original-file thumbnail-file)




(provide 'gnus-image-config)

;;; gnus-image.el ends here



(testing

 (gnus-with-article-headers
   (let* ((mail-extr-disable-voodoo t)
          (mail-extr-ignore-realname-equals-mailbox-name nil)
          (addresses (mail-extract-address-components
                      (or (mail-fetch-field "from") "") t))
          (image-size (or gnus-image-size gravatar-size))
          name)
     (dolist (address addresses)
       (when (and (setq name (car address))
                  (string-match "\\` +" name))
         (setcar address (setq name (substring name (match-end 0)))))
       (when (or t
                 (not (and gnus-image-too-ugly
                           (or (string-match gnus-image-too-ugly
                                             (or (cadr address) ""))
                               (and name
                                    (string-match gnus-image-too-ugly
                                                  name))))))
         (gravatar-retrieve
          ;; (cadr address)
          ;; nil
          email-personal
          'gnus-image-insert
          (list "to"
                `(nil ,email-personal)
                'from-image))))))

 (gravatar-retrieve email-personal #'identity)



 (gnus-image-insert (gravatar-retrieve email-personal #'identity)
                    "to" `(nil email-personal) 'from-image)


 (gnus-image-insert
  (list 'image :type 'jpeg :data
        (cdaar (remove-if 'null (eudc-query `((mail . ,email-office )) '(thumbnailPhoto)))))
  "from" `(nil ,email-friend) 'from-image)

 "Name Sur" <email@host.com>
 (list 'image :type 'jpeg :data
       (cdaar (remove-if 'null (eudc-query `((mail . ,email-office)) '(thumbnailPhoto))))) )





