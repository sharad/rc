;;; project-buffer-modified.el --- Project Buffer Modified

;; Copyright (C) 2014  Sharad Pratap

;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Author: Sharad Pratap <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

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



(defun projbuf-file--file-name-regexp-set (variable value)
  (set-default variable value)
  (if (fboundp 'projbuf-file-name-regexp-update)
      (projbuf-file-name-regexp-update)))

(defcustom projbuf-file-name-regexp (purecopy "\\.pjb\\(~\\|\\.~[0-9]+~\\)?\\'")
  "Regexp which matches filenames to be encrypted with GnuPG.

If you set this outside Custom while projbuf-file is already enabled, you
have to call `projbuf-file-name-regexp-update' after setting it to
properly update file-name-handler-alist.  Setting this through Custom
does that automatically."
  :type 'regexp
  :group 'projbuf-file
  :set 'projbuf-file--file-name-regexp-set)

(defcustom projbuf-file-inhibit-auto-save t
  "If non-nil, disable auto-saving when opening an encrypted file."
  :type 'boolean
  :group 'projbuf-file)

(defvar projbuf-file-handler
  (cons projbuf-file-name-regexp 'projbuf-file-handler))

(defvar projbuf-file-auto-mode-alist-entry
  (list projbuf-file-name-regexp nil 'projbuf-file))

(defun projbuf-file-name-regexp-update ()
  (interactive)
  (unless (equal (car projbuf-file-handler) projbuf-file-name-regexp)
    (setcar projbuf-file-handler projbuf-file-name-regexp)))

(defun projbuf-file-find-file-hook ()
  (if (and buffer-file-name
	   (string-match projbuf-file-name-regexp buffer-file-name)
	   projbuf-file-inhibit-auto-save)
      (auto-save-mode 0)))


;;;###autoload
(defun projbuf-file-handler (operation &rest args)
  (save-match-data
    (let ((op (get operation 'projbuf-file)))
      (if op
  	  (apply op args)
  	(projbuf-file-run-real-handler operation args)))))

(defun projbuf-file-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'projbuf-file-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun projbuf-file-decode-and-insert (string file visit beg end replace)
  (if (fboundp 'decode-coding-inserted-region)
      (save-restriction
	(narrow-to-region (point) (point))
	(insert (if enable-multibyte-characters
		    (string-to-multibyte string)
		  string))
	  (decode-coding-inserted-region
	   (point-min) (point-max)
	   (substring file 0 (string-match projbuf-file-name-regexp file))
	 visit beg end replace))
    (insert (projbuf-file--decode-coding-string string (or coding-system-for-read
						       'undecided)))))



(defun projbuf-file-decode-and-insert (string file visit beg end replace)
    (project-buffer-mode t)
    ;; (read-from-minibuffer "local-file: " local-file)
    ;; (project-buffer-raw-load local-file nil t)

    (let ((project-buffer (current-buffer))
          (status project-buffer-status))
      (with-temp-buffer
        (read-from-minibuffer "local-file1: " local-file)
        (projbuf-file-run-real-handler #'insert-file-contents (list local-file))
        (read-from-minibuffer "local-file2: " local-file)
        (goto-char (point-min))
        (let ((data-buffer (current-buffer))
              data-version
              block-header)
          (with-current-buffer project-buffer
            (setq data-version (project-buffer-read-header status data-buffer set-buffer-name t))
            ;; The rest of the file is defined by blocks:
            (while (project-buffer-read-block status data-buffer run-mode-hooks)))))
      (run-hooks 'project-buffer-post-load-hook))
    (setq project-buffer-file-name filename))

(defvar projbuf-file-error nil)
(defun projbuf-file--find-file-not-found-function ()
  (let ((error projbuf-file-error))
    (save-window-excursion
      (kill-buffer))
    (signal 'file-error
	    (cons "Opening input file" (cdr error)))))

(defvar last-coding-system-used)
(defun projbuf-file-insert-file-contents (file &optional visit beg end replace)
  (read-from-minibuffer "local-file0: " file)
  (barf-if-buffer-read-only)
  (if (and visit (or beg end))
      (error "Attempt to visit less than an entire file"))
  (setq file (expand-file-name file))
  (let* ((local-copy (projbuf-file-run-real-handler #'file-local-copy (list file))
           ;; (condition-case nil
	   ;;    (projbuf-file-run-real-handler #'file-local-copy (list file))
	   ;;  (error))
           )
	 (local-file (or local-copy file))
	 string length entry)
    (if visit
	(setq buffer-file-name file))

    (unwind-protect
	(progn
	  (if replace
	      (goto-char (point-min)))
	  (when nil ;; comment
           (condition-case error
	      (setq string (epg-decrypt-file context local-file nil))
	    (progn ;; error

	     ;; (if (setq entry (assoc file projbuf-file-passphrase-alist))
	     ;;     (setcdr entry nil))

	     ;; Hack to prevent find-file from opening empty buffer
	     ;; when decryption failed (bug#6568).  See the place
	     ;; where `find-file-not-found-functions' are called in
	     ;; `find-file-noselect-1'.
	     (when (file-exists-p local-file)
	       (make-local-variable 'projbuf-file-error)
	       (setq projbuf-file-error error)
	       (add-hook 'find-file-not-found-functions
			 'projbuf-file--find-file-not-found-function
			 nil t))
	     (signal 'file-error
		     (cons "Opening input file" (cdr error))))))
	  ;; (make-local-variable 'projbuf-file-encrypt-to)
	  ;; (setq projbuf-file-encrypt-to
	  ;;       (mapcar #'car (epg-context-result-for context 'encrypted-to)))
	  (if (or beg end)
	      (setq string (substring string (or beg 0) end)))
	  (save-excursion
	    ;; If visiting, bind off buffer-file-name so that
	    ;; file-locking will not ask whether we should
	    ;; really edit the buffer.
	    (let ((buffer-file-name
		   (if visit nil buffer-file-name)))
	      (save-restriction
		(narrow-to-region (point) (point))

		;; (projbuf-file-decode-and-insert string file visit beg end replace)

                (progn
                  (project-buffer-mode t)
                  ;; (read-from-minibuffer "local-file: " local-file)
                  ;; (project-buffer-raw-load local-file nil t)

                  (let ((project-buffer (current-buffer))
                        (status project-buffer-status))
                    (with-temp-buffer
                      (read-from-minibuffer "local-file1: " local-file)
                      (projbuf-file-run-real-handler #'insert-file-contents (list local-file))
                      (read-from-minibuffer "local-file2: " local-file)
                      (goto-char (point-min))
                      (let ((data-buffer (current-buffer))
                            data-version
                            block-header)
                        (with-current-buffer project-buffer
                          (setq data-version (project-buffer-read-header status data-buffer nil t))
                          ;; The rest of the file is defined by blocks:
                          (while (project-buffer-read-block status data-buffer nil))))
                      (read-from-minibuffer "local-file3: " local-file))
                    ;; (run-hooks 'project-buffer-post-load-hook)
                    )
                  (setq project-buffer-file-name filename))
                (read-from-minibuffer "local-file4: " local-file)
		(setq length (- (point-max) (point-min))))
	      (if replace
		  (delete-region (point) (point-max))))
	    (if visit
		(set-visited-file-modtime))))
      (if (and local-copy
	       (file-exists-p local-copy))
	  (delete-file local-copy)))
    (list file length)
    (top-level)))
(put 'insert-file-contents 'projbuf-file 'projbuf-file-insert-file-contents)


(defun project-buffer-save-string ()
  "Save the project data in FILENAME; the project can later be
reloaded through `project-buffer-raw-load' function."
  ;; (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let* ((status                  project-buffer-status)
	 (node                    (ewoc-nth status 0))
	 (buf-name                (buffer-name))
	 (buf-dir                 default-directory)
	 (project-buffer          (current-buffer))
	 (hooks-list              (mapcar (lambda (item) (cons item (and (local-variable-p item) (eval item))))
					  project-buffer-hooks-to-save))
	 (locals-list             (remove nil
				   (mapcar (lambda (item) (and (local-variable-p item) (cons item (eval item))))
					   project-buffer-locals-to-save))))
    (with-temp-buffer
      ;; First, let's write a quick header:
      (print (list 'project-buffer-mode
		   project-buffer-mode-version
		   buf-name
		   buf-dir) (current-buffer))
      ;; Save the hooks:
      (mapcar (lambda (item) (when (cdr item) (project-buffer-raw-print-hooks (car item) (cdr item))))
	      hooks-list)
      ;; Save the locals:
      (project-buffer-raw-print-locals locals-list)
      ;; Save each nodes:
      (print (list 'begin 'node-list) (current-buffer))
      (while node
	(let ((data (ewoc-data node)))
	  (unless (eq (project-buffer-node->type data) 'folder)
	    (print (list (project-buffer-node->name data)
			 (project-buffer-node->type data)
			 (project-buffer-node->filename data)
			 (project-buffer-node->project data)
			 (project-buffer-node->platform-list data)
			 (project-buffer-node->build-configurations-list data)
			 (project-buffer-node->user-data data)
			 (project-buffer-node->project-settings data))
		   (current-buffer))))
	(setq node (ewoc-next status node)))
      (print (list 'end 'node-list) (current-buffer))
      ;; Save the master project:
      (print (list 'one-line 'master-project (car (buffer-local-value 'project-buffer-master-project project-buffer)))
	     (current-buffer))
      ;; End of file:
      (print 'eof (current-buffer))
      ;; Finally: write the file.
      (buffer-string))))

(defun projbuf-file-write-region (start end file &optional append visit lockname
				    mustbenew)
  (if append
      (error "Can't append to the file"))
  (setq file (expand-file-name file))
  (let* ((coding-system (or coding-system-for-write
			    (if (fboundp 'select-safe-coding-system)
				;; This is needed since Emacs 22 has
				;; no-conversion setting for *.gpg in
				;; `auto-coding-alist'.
			        (let ((buffer-file-name
				       (file-name-sans-extension file)))
				  (select-safe-coding-system
				   (point-min) (point-max)))
			      buffer-file-coding-system)))
	 ;; (context (epg-make-context))
	 (coding-system-for-write 'binary)
	 string entry
	 ;; (recipients
	 ;;  (cond
	 ;;   ((listp projbuf-file-encrypt-to) projbuf-file-encrypt-to)
	 ;;   ((stringp projbuf-file-encrypt-to) (list projbuf-file-encrypt-to))))
         )



    (condition-case error
	(setq string
              (project-buffer-save-string)
;; 	      (epg-encrypt-string
;; 	       context
;; 	       (if (stringp start)
;; 		   (projbuf-file--encode-coding-string start coding-system)
;; 		 (unless start
;; 		   (setq start (point-min)
;; 			 end (point-max)))
;; 		 (projbuf-file--encode-coding-string (buffer-substring start end)
;; 						 coding-system))
;; 	       (if (or (eq projbuf-file-select-keys t)
;; 		       (and (null projbuf-file-select-keys)
;; 			    (not (local-variable-p 'projbuf-file-encrypt-to
;; 						   (current-buffer)))))
;; 		   (epa-select-keys
;; 		    context
;; 		    "Select recipients for encryption.
;; If no one is selected, symmetric encryption will be performed.  "
;; 		    recipients)
;; 		 (if projbuf-file-encrypt-to
;; 		     (epg-list-keys context recipients))))
              ))
    (projbuf-file-run-real-handler
     #'write-region
     (list string nil file append visit lockname mustbenew))
    (if (boundp 'last-coding-system-used)
	(setq last-coding-system-used coding-system))
    (if (eq visit t)
	(progn
	  (setq buffer-file-name file)
	  (set-visited-file-modtime))
      (if (stringp visit)
	  (progn
	    (set-visited-file-modtime)
	    (setq buffer-file-name visit))))
    (if (or (eq visit t)
	    (eq visit nil)
	    (stringp visit))
	(message "Wrote %s" buffer-file-name))))
(put 'write-region 'projbuf-file 'projbuf-file-write-region)

;;;###autoload
(defun projbuf-file-enable ()
  (interactive)
  (if (memq projbuf-file-handler file-name-handler-alist)
      (message "`projbuf-file' already enabled")
    (setq file-name-handler-alist
	  (cons projbuf-file-handler file-name-handler-alist))
    (add-hook 'find-file-hook 'projbuf-file-find-file-hook)
    (setq auto-mode-alist (cons projbuf-file-auto-mode-alist-entry auto-mode-alist))
    (message "`projbuf-file' enabled")))

;;;###autoload
(defun projbuf-file-disable ()
  (interactive)
  (if (memq projbuf-file-handler file-name-handler-alist)
      (progn
	(setq file-name-handler-alist
	      (delq projbuf-file-handler file-name-handler-alist))
	(remove-hook 'find-file-hook 'projbuf-file-find-file-hook)
	(setq auto-mode-alist (delq projbuf-file-auto-mode-alist-entry
				    auto-mode-alist))
	(message "`projbuf-file' disabled"))
    (message "`projbuf-file' already disabled")))

(provide 'project-buffer-modified)
;;; project-buffer-modified.el ends here
