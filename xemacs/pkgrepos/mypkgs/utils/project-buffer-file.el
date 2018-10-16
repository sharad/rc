;;; project-buffer-file.el --- Project buffer file

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience, files

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

;; Project buffer file

;;; Code:

(require 'project-buffer-mode)

(defgroup pbm-file nil
  "The EasyPG Assistant hooks for transparent file encryption"
  :version "23.1"
  :group 'pbm)

(defun pbm-file--file-name-regexp-set (variable value)
  (set-default variable value)
  (if (fboundp 'pbm-file-name-regexp-update)
      (pbm-file-name-regexp-update)))

(defcustom pbm-file-name-regexp (purecopy "\\.pb\\(~\\|\\.~[0-9]+~\\)?\\'")
  "Regexp which matches filenames to be encrypted with GnuPG.

If you set this outside Custom while pbm-file is already enabled, you
have to call `pbm-file-name-regexp-update' after setting it to
properly update file-name-handler-alist.  Setting this through Custom
does that automatically."
  :type 'regexp
  :group 'pbm-file
  :set 'pbm-file--file-name-regexp-set)

(defcustom pbm-file-inhibit-auto-save t
  "If non-nil, disable auto-saving when opening an encrypted file."
  :type 'boolean
  :group 'pbm-file)

(defvar pbm-file-encrypt-to nil
  "Recipient(s) used for encrypting files.
May either be a string or a list of strings.")

;; (put 'pbm-file-encrypt-to 'safe-local-variable
;;      (lambda (val)
;;        (or (stringp val)
;; 	   (and (listp val)
;; 		(catch 'safe
;; 		  (mapc (lambda (elt)
;; 			  (unless (stringp elt)
;; 			    (throw 'safe nil)))
;; 			val)
;; 		  t)))))

;; (put 'pbm-file-encrypt-to 'permanent-local t)

(defvar pbm-file-handler
  (cons pbm-file-name-regexp 'pbm-file-handler))

;; (defvar pbm-file-auto-mode-alist-entry
;;   (list pbm-file-name-regexp nil 'pbm-file))

(defun pbm-file-name-regexp-update ()
  (interactive)
  (unless (equal (car pbm-file-handler) pbm-file-name-regexp)
    (setcar pbm-file-handler pbm-file-name-regexp)))

(defun pbm-file-find-file-hook ()
  (if (and buffer-file-name
	   (string-match pbm-file-name-regexp buffer-file-name)
	   pbm-file-inhibit-auto-save)
      (auto-save-mode 0)))

;;;###autoload
(defun pbm-file-handler (operation &rest args)
  (save-match-data
    (let ((op (get operation 'pbm-file)))
      (if op
  	  (apply op args)
  	(pbm-file-run-real-handler operation args)))))

(defun pbm-file-run-real-handler (operation args)
  (let ((inhibit-file-name-handlers
	 (cons 'pbm-file-handler
	       (and (eq inhibit-file-name-operation operation)
		    inhibit-file-name-handlers)))
	(inhibit-file-name-operation operation))
    (apply operation args)))

(defun project-buffer-file-insert-format (fmt &rest args)
  (let ((str (apply #'format fmt args)))
    (insert str)))

(defun project-buffer-file-create-empty-file (filename)
  (assert (not (file-exists-p filename)))

  (condition-case nil
      (pbm-file-run-real-handler #'file-local-copy (list file))
    (error))

  (with-temp-buffer ;; with-current-buffer (find-file-noselect filename)
    (let ((rest-lines '("(begin locals)\n\n"
                        "(project-buffer-view-mode . folder-view)\n\n"
                        "(project-buffer-current-platform)\n\n"
                        "(project-buffer-current-build-configuration)\n\n"
                        "(end locals)\n\n"
                        "(begin node-list)\n\n"
                        "(end node-list)\n\n"
                        "(one-line master-project nil)\n\n"
                        "eof")))
      (progn
        (insert "\n")

        (project-buffer-file-insert-format
         "(project-buffer-mode %s \"%s\" \"%s\")\n\n"
         project-buffer-mode-version
         (file-name-nondirectory filename)
         (directory-file-name
          (expand-file-name
           (file-name-directory filename))))

        (dolist (line rest-lines)
          (insert line))

        ;; (write-region nil nil filename)

        (pbm-file-run-real-handler
         #'write-region
         (list (buffer-string) nil filename))
        ;; (save-buffer)
        ))))

(defun project-buffer-raw-load-with-real-handler (filename &optional set-buffer-name run-mode-hooks)
  "Load a project saved by `project-buffer-raw-data'.
This function does not restore the mode and assume the
project-buffer-mode to be set.  It doesn't clear the existing
nodes either."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
  (let ((project-buffer (current-buffer))
        (status project-buffer-status))
    (with-temp-buffer

      ;; (unless (file-exists-p filename)
      ;;   (project-buffer-file-create-empty-file filename))

      (pbm-file-run-real-handler #'insert-file-contents (list filename))

      (goto-char (point-min))
      (let ((data-buffer (current-buffer))
	    data-version
	    block-header)
	(with-current-buffer project-buffer
	  (setq data-version (project-buffer-read-header status data-buffer set-buffer-name t))
	  ;; The rest of the file is defined by blocks:
	  (while (project-buffer-read-block status data-buffer run-mode-hooks))
	  )))
    (run-hooks 'project-buffer-post-load-hook)))

(defun project-buffer-raw-save-with-real-handler (filename)
  "Save the project data in FILENAME; the project can later be
reloaded through `project-buffer-raw-load' function."
  (unless project-buffer-status (error "Not in project-buffer buffer"))
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
      (mapc
       (lambda (item) (when (cdr item) (project-buffer-raw-print-hooks (car item) (cdr item))))
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
			 (project-buffer-node->project-settings data)
			 ;; (project-buffer-node->dependent-project-list data)
       )
		   (current-buffer))))
	(setq node (ewoc-next status node)))
      (print (list 'end 'node-list) (current-buffer))
      ;; Save the master project:
      (print (list 'one-line 'master-project (car (buffer-local-value 'project-buffer-master-project project-buffer)))
	     (current-buffer))
      ;; End of file:
      (print 'eof (current-buffer))
      ;; Finally: write the file.
      (pbm-file-run-real-handler
       #'write-region
       (list (buffer-string) nil filename))
      ;; (write-file filename)
      )))

(defun pbm-file-insert-file-contents (file &optional visit beg end replace)
  (barf-if-buffer-read-only)
  (if (and visit (or beg end))
      (error "Attempt to visit less than an entire file"))
  (setq file (expand-file-name file))

  (unless (file-exists-p file)
    (project-buffer-file-create-empty-file file))

  (let* ((local-copy
          (condition-case nil
              (pbm-file-run-real-handler #'file-local-copy (list file))
            (error)))
         (local-file (or local-copy file))
         ;; (context (epg-make-context))
         string
         (length 0)
         entry)
    (if visit
        (setq buffer-file-name file))
    ;; (epg-context-set-passphrase-callback
    ;;  context
    ;;  (cons #'pbm-file-passphrase-callback-function
    ;;        local-file))
    ;; (epg-context-set-progress-callback
    ;;  context
    ;;  (cons #'pbm-progress-callback-function
    ;;        (format "Decrypting %s" file)))
    (unwind-protect
         (progn
           (if replace
               (goto-char (point-min)))
           ;; (condition-case error
           ;;     (setq string (epg-decrypt-file context local-file nil))
           ;;   (error
           ;;    (if (setq entry (assoc file pbm-file-passphrase-alist))
           ;;        (setcdr entry nil))
           ;;    ;; Hack to prevent find-file from opening empty buffer
           ;;    ;; when decryption failed (bug#6568).  See the place
           ;;    ;; where `find-file-not-found-functions' are called in
           ;;    ;; `find-file-noselect-1'.
           ;;    (when (file-exists-p local-file)
           ;;      (make-local-variable 'pbm-file-error)
           ;;      (setq pbm-file-error error)
           ;;      (add-hook 'find-file-not-found-functions
           ;;       	 'pbm-file--find-file-not-found-function
           ;;       	 nil t))
           ;;    (signal 'file-error
           ;;            (cons "Opening input file" (cdr error)))))

           ;; (make-local-variable 'pbm-file-encrypt-to)

           ;; (setq pbm-file-encrypt-to
           ;;       (mapcar #'car (epg-context-result-for context 'encrypted-to)))

           ;; (if (or beg end)
           ;;     (setq string (substring string (or beg 0) end)))
           (save-excursion
             ;; If visiting, bind off buffer-file-name so that
             ;; file-locking will not ask whether we should
             ;; really edit the buffer.
             (let ((buffer-file-name
                    (if visit nil buffer-file-name)))
               ;; (save-restriction
               ;;   (narrow-to-region (point) (point))
               ;;   (pbm-file-decode-and-insert string file visit beg end replace)
               ;;   (setq length (- (point-max) (point-min))))
               (when nil
                 (save-restriction
                   (project-buffer-mode t)
                   (project-buffer-raw-load-with-real-handler file nil t)
                   (setq project-buffer-file-name file)

                   (setq length (- (point-max) (point-min)))))

               ;; (if replace
               ;;     (delete-region (point) (point-max)))
               )
             (if visit
                 (set-visited-file-modtime))))
      (if (and local-copy
               (file-exists-p local-copy))
          (delete-file local-copy)))
    (list file length)))
(put 'insert-file-contents 'pbm-file 'pbm-file-insert-file-contents)


(defun pbm-file-write-region (start end file &optional append visit lockname
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
	 ;; string
         ;; entry
	 ;; (recipients
	 ;;  (cond
	 ;;   ((listp pbm-file-encrypt-to) pbm-file-encrypt-to)
	 ;;   ((stringp pbm-file-encrypt-to) (list pbm-file-encrypt-to))))
         )
;;     (epg-context-set-passphrase-callback
;;      context
;;      (cons #'pbm-file-passphrase-callback-function
;;            file))
;;     (epg-context-set-progress-callback
;;      context
;;      (cons #'pbm-progress-callback-function
;;            (format "Encrypting %s" file)))
;;     (epg-context-set-armor context pbm-armor)
;;     (condition-case error
;; 	(setq string
;; 	      (epg-encrypt-string
;; 	       context
;; 	       (if (stringp start)
;; 		   (pbm-file--encode-coding-string start coding-system)
;; 		 (unless start
;; 		   (setq start (point-min)
;; 			 end (point-max)))
;; 		 (pbm-file--encode-coding-string (buffer-substring start end)
;; 						 coding-system))
;; 	       (if (or (eq pbm-file-select-keys t)
;; 		       (and (null pbm-file-select-keys)
;; 			    (not (local-variable-p 'pbm-file-encrypt-to
;; 						   (current-buffer)))))
;; 		   (pbm-select-keys
;; 		    context
;; 		    "Select recipients for encryption.
;; If no one is selected, symmetric encryption will be performed.  "
;; 		    recipients)
;; 		 (if pbm-file-encrypt-to
;; 		     (epg-list-keys context recipients)))))
;;       (error
;;        (if (setq entry (assoc file pbm-file-passphrase-alist))
;; 	   (setcdr entry nil))
;;        (signal 'file-error (cons "Opening output file" (cdr error)))))

    ;; (pbm-file-run-real-handler
    ;;  #'write-region
    ;;  (list string nil file append visit lockname mustbenew))

    (project-buffer-raw-save-with-real-handler file)

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
(put 'write-region 'pbm-file 'pbm-file-write-region)


;;;###autoload
(defun pbm-file-enable ()
  (interactive)
  (if (memq pbm-file-handler file-name-handler-alist)
      (message "`pbm-file' already enabled")
    (setq file-name-handler-alist
	  (cons pbm-file-handler file-name-handler-alist))
    (add-hook 'find-file-hook 'pbm-file-find-file-hook)
    ;; (setq auto-mode-alist (cons pbm-file-auto-mode-alist-entry auto-mode-alist))
    (message "`pbm-file' enabled")))

;;;###autoload
(defun pbm-file-disable ()
  (interactive)
  (if (memq pbm-file-handler file-name-handler-alist)
      (progn
	(setq file-name-handler-alist
	      (delq pbm-file-handler file-name-handler-alist))
	(remove-hook 'find-file-hook 'pbm-file-find-file-hook)
	;; (setq auto-mode-alist (delq pbm-file-auto-mode-alist-entry
	;; 			    auto-mode-alist))
	(message "`pbm-file' disabled"))
    (message "`pbm-file' already disabled")))


(defun pbm-setup-local-key ()
  "Define a local key-bindings."
  (local-set-key [(control ?c) ?n] 'iproject-add-project)
  (local-set-key [(control ?c) ?+] 'iproject-add-files-to-current-project)
  (local-set-key [(control ?c) ?m] 'iproject-move-marked-files-or-current-file-within-project)

  (local-set-key [(control ?c) (control ?r)] 'project-buffer-revert)
  ;; (local-set-key [(control ?x) (control ?s)] 'project-buffer-save-file)
  (local-set-key [(control ?x) (control ?w)] 'project-buffer-write-file))

(defun pbm-mode (&optional skip-mode-hooks)
  "Major mode to view project.

Commands:
\\{project-buffer-mode-map}"
  (kill-all-local-variables)
  (buffer-disable-undo)
  (setq mode-name "project-buffer"
	major-mode 'project-buffer-mode
	buffer-read-only t)
  (use-local-map project-buffer-mode-map)
  (let ((buffer-read-only nil))
    (erase-buffer)
    (let ((status (ewoc-create 'project-buffer-prettyprint "" "" t)))
      (dolist (var
                '(project-buffer-status
                  project-buffer-view-mode
                  project-buffer-cache-project
                  project-buffer-cache-subdirectory
                  project-buffer-platforms-list
                  project-buffer-current-platform
                  project-buffer-build-configurations-list
                  project-buffer-current-build-configuration
                  project-buffer-master-project
                  project-buffer-projects-list
                  project-buffer-file-name
                  project-buffer-locals-to-save
                  project-buffer-hooks-to-save))
        (make-local-variable var))

      (setq
       project-buffer-status status
       project-buffer-view-mode 'folder-view
       project-buffer-cache-project nil
       project-buffer-cache-subdirectory nil
       project-buffer-platforms-list nil
       project-buffer-current-platform nil
       project-buffer-build-configurations-list nil
       project-buffer-current-build-configuration nil
       project-buffer-master-project nil
       project-buffer-projects-list nil
       project-buffer-file-name nil
       project-buffer-locals-to-save '(project-buffer-view-mode project-buffer-current-platform project-buffer-current-build-configuration)
       project-buffer-hooks-to-save '(project-buffer-mode-hook project-buffer-action-hook project-buffer-post-load-hook project-buffer-post-find-file-hook project-buffer-refresh-hook))

      (project-buffer-refresh-ewoc-hf status)

      (unless skip-mode-hooks
	(run-hooks 'project-buffer-mode-hook))

      ;; (remove-hook 'project-buffer-post-load-hook 'iproject-setup-local-key)

      (when buffer-file-name
        (project-buffer-raw-load-with-real-handler buffer-file-name nil t)
        (setq
         project-buffer-file-name buffer-file-name
         default-directory (dirname-of-file buffer-file-name))
        ;; (not-modified)
        (set-buffer-modified-p nil))

      ;; (local-unset-key (key "C-x C-s"))
      (add-hook 'project-buffer-post-load-hook 'pbm-setup-local-key nil t))))

(add-to-list 'auto-mode-alist '("\\.pb\\'" . pbm-mode))



(provide 'project-buffer-file)
;;; project-buffer-file.el ends here
