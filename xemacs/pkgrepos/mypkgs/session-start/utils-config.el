

(defvar resume-workdir "/home/s/paradise/Projects/doc/resume" "resume work dir.")

(defvar tags-from-resume nil "Tags from resume")

(defun tags-from-resume (prompt)
  (if tags-from-resume
      tags-from-resume
      (let ((resume-make-keys (format "make -sC %s resume=%s keys" resume-workdir "sharad")))
        (read-string prompt (shell-command-to-string resume-make-keys)))))



(defun insert-reply-object (resume object &optional keys attachment type discription)
  "Prepare reply object"
  (interactive
   (let*
       ((resume (read-from-minibuffer "who: " "sharad"))
        (object (read-from-minibuffer "object: " "resume"))
        (resume-make-keys (format "make -sC %s name=%s object=%s keys" resume-workdir resume object))
        (keys   (read-string "keys: " (shell-command-to-string resume-make-keys)))
        (type (or (read-from-minibuffer "type: " "pdf") "txt"))
        ;(attachment (y-or-n-p (format "make inline: ")))
        (discription "attachment")
        attachment)
     (list resume object keys attachment type disposition)))
  (let* ((resume-make-keys (format "make -sC %s name=%s object=%s keys" resume-workdir resume object))
         (keys (or keys  (read-string "keys: " (shell-command-to-string resume-make-keys))))
         (keys-of-resume-name (mapconcat 'identity (sort (split-string keys) #'string-lessp) "-"))
         (resume-make-cmd (format "make -C %s name=%s object=%s filter_targets='%s' %s link%s" resume-workdir resume object keys type type))
         (resume-attachable-file (format "%s/output/%s-%s.%s" resume-workdir resume object type))
         (resume-actual-file (format "%s/output/%s-%s-%s.%s" resume-workdir resume object keys-of-resume-name type))
         (resume-view-cmd (format "%s %s" "evince" resume-actual-file)))
    (message "preparing %s by: %s" object resume-make-cmd)
    (if (and
           (shell-command resume-make-cmd)
           (if attachment
               (shell-command resume-view-cmd) t)
           (file-exists-p resume-actual-file))
      (if attachment
          (mml-attach-file
           resume-attachable-file
           (mm-default-file-encoding resume-attachable-file)
           ;; (mml-minibuffer-read-type resume-attachable-file) ; "application/pdf"
           discription "inline")
          (insert-file-contents resume-actual-file))
      (message "Not able to %s %s."
               (if attachment "attach" "insert")
               cover))))









;; (interactive
;;    (let* ((file (mml-minibuffer-read-file "Attach file: "))
;; 	  (type (mml-minibuffer-read-type file))
;; 	  (description (mml-minibuffer-read-description))
;; 	  (disposition (mml-minibuffer-read-disposition type nil file)))
;;      (list file type description disposition)))

;; (let* ((file (mml-minibuffer-read-file "Attach file: "))
;;        (type (mml-minibuffer-read-type file))))


;; test
;; (trace-function 'attach-resume)
;; (untrace-function 'delete-file)
;; (trace-function 'make-symbolic-link)
;; (delete-file "asfasdf")



;;;###autoload
(defun sharad/read-file (filename)
  (when (file-exists-p filename)
    (with-temp-buffer
      (insert-file-contents-literally filename)
      (let ((contents (read (current-buffer))))
        contents))))

(defun shell-command-no-output (cmd)
  (if (equal 0 (call-process "/bin/bash" nil nil nil "-c" cmd))
      t))


(defun messageto (buf &rest text)
  (with-current-buffer (get-buffer-create buf)
    (apply 'insert text)
    (insert "\n")))

(messageto "*Complains*" "Bookmarking fecility, may consider Org mode which is unexpanded.")


(defcustom recentf-save-file-modes 384 ;; 0600
  "Mode bits of recentf save file, as an integer, or nil.
If non-nil, after writing `recentf-save-file', set its mode bits to
this value.  By default give R/W access only to the user who owns that
file.  See also the function `set-file-modes'."
  :group 'recentf
  :type '(choice (const :tag "Don't change" nil)
          integer))


(defcustom recentf-save-file (convert-standard-filename "~/.recentf")
  "File to save the recent list into."
  :group 'recentf
  :type 'file
  :initialize 'custom-initialize-default
  :set (lambda (symbol value)
         (let ((oldvalue (eval symbol)))
           (custom-set-default symbol value)
           (and (not (equal value oldvalue))
                recentf-mode
                (recentf-load-list)))))


(defconst recentf-save-file-coding-system
  (if (coding-system-p 'utf-8-emacs)
      'utf-8-emacs
    'emacs-mule)
  "Coding system of the file `recentf-save-file'.")

(defun recentf-save-list ()
  "Save the recent list.
Write data into the file specified by `recentf-save-file'."
  (interactive)
  (condition-case error
      (with-temp-buffer
        (erase-buffer)
        (set-buffer-file-coding-system recentf-save-file-coding-system)
        (insert (format recentf-save-file-header (current-time-string)))
        (recentf-dump-variable 'recentf-list recentf-max-saved-items)
        (recentf-dump-variable 'recentf-filter-changer-current)
        (insert "\n\n;; Local Variables:\n"
                (format ";; coding: %s\n" recentf-save-file-coding-system)
                ";; End:\n")
        (write-file (expand-file-name recentf-save-file))
        (when recentf-save-file-modes
          (set-file-modes recentf-save-file recentf-save-file-modes))
        nil)
    (error
     (warn "recentf mode: %s" (error-message-string error)))))




(provide 'utils-config)
