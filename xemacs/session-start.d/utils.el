

(defvar resume-workdir "/home/s/paradise/Projects/doc/resume" "resume work dir.")

(defvar tags-from-resume nil "Tags from resume")

(defun tags-from-resume (prompt)
  (if tags-from-resume
      tags-from-resume
      (let ((resume-make-keys (format "make -sC %s resume=%s keys" resume-workdir "sharad")))
        (read-string prompt (shell-command-to-string resume-make-keys)))))

(defun attach-resume (resume)
    "Attach resume with keys."
    ;; (interactive "skeys: \nsresume: ")
    (interactive "sresume: ")
    (let* ((resume (or resume "sharad"))
           (resume-make-keys (format "make -sC %s name=%s object=resume keys" resume-workdir resume))
           (keys (or (read-string "keys: " (shell-command-to-string resume-make-keys)) ".+"))
           (keys-of-resume-name (mapconcat 'identity (sort (split-string keys) #'string-lessp) "-"))
           (resume-make-cmd (format "make -C %s name=%s object=resume filter_targets='%s'" resume-workdir resume keys))
           (resume-attachable-file (format "%s/output/sharad-resume.pdf" resume-workdir))
           (resume-actual-file (format "%s/output/%s-resume-%s.pdf" resume-workdir resume keys-of-resume-name))
           (resume-view-cmd (format "%s %s" "evince" resume-actual-file)))
      (message "preparing resume by: %s" resume-make-cmd)
      (when (and
             (shell-command resume-make-cmd)
             (shell-command resume-view-cmd)
             (y-or-n-p (format "Should I attach it with message %s: " resume-actual-file))
             (file-exists-p resume-actual-file))
        (if (file-exists-p resume-attachable-file)
            (unless (string-equal (file-symlink-p resume-attachable-file) resume-actual-file)
              (delete-file resume-attachable-file)
              (make-symbolic-link resume-actual-file resume-attachable-file))
            (make-symbolic-link resume-actual-file resume-attachable-file))
        (mml-attach-file resume-attachable-file "application/pdf" "Sharad Pratap - Résumé" "inline"))
      (message "Not able to attach resume.")))



;; test
;; (trace-function 'attach-resume)
;; (untrace-function 'delete-file)
;; (trace-function 'make-symbolic-link)
;; (delete-file "asfasdf")



(user-provide 'utils)
