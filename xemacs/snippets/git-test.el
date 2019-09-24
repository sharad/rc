
(defun git-link--branch-merge (branch)
  (git-link--get-config (format "branch.%s.merge" branch)))

(defun git-link--branch-remote-merge ( &optional branch noerror)
  (let* ((branch         (or branch (git-link--branch)))
         (remote         (git-link--branch-remote branch))
         (merge          (git-link--branch-merge branch))
         (merge-basename (when merge (file-name-nondirectory merge))))
   (if (string= remote ".")
      (if merge
          (git-link--branch-remote-merge merge-basename)
        (if noerror
            branch
          (error "Not merge branch for %s" branch)))
     merge-basename)))

(defun git-link (remote start end)
  "Create a URL representing the current buffer's location in its
GitHub/Bitbucket/GitLab/... repository at the current line number
or active region. The URL will be added to the kill ring. If
`git-link-open-in-browser' is non-`nil' also call `browse-url'.

With a prefix argument prompt for the remote's name.
Defaults to \"origin\"."
  (interactive (let* ((remote (git-link--select-remote))
                      (region (when (or buffer-file-name (git-link--using-magit-blob-mode))
                                (git-link--get-region))))
                 (list remote (car region) (cadr region))))
  (let (filename branch commit handler remote-info (remote-url (git-link--remote-url remote)))
    (if (null remote-url)
        (message "Remote `%s' not found" remote)

      (setq remote-info (git-link--parse-remote remote-url)
            filename    (git-link--relative-filename)
            branch      (git-link--branch-remote-merge) ;; (git-link--branch)
            commit      (git-link--commit)
            handler     (git-link--handler git-link-remote-alist (car remote-info)))

      (cond ((null filename)
             (message "Can't figure out what to link to"))
            ((null (car remote-info))
             (message "Remote `%s' contains an unsupported URL" remote))
            ((not (functionp handler))
             (message "No handler found for %s" (car remote-info)))
            ;; TODO: null ret val
            (t
             (let ((vc-revison (git-link--parse-vc-revision filename)))
               (when vc-revison
                 (setq filename (car vc-revison)
                       commit   (cdr vc-revison)))

               (git-link--new
                (funcall handler
                         (car remote-info)
                         (cadr remote-info)
                         filename
                         (if (or (git-link--using-git-timemachine)
                                 (git-link--using-magit-blob-mode)
                                 vc-revison
                                 git-link-use-commit)
                             nil
                           (url-hexify-string branch))
                         commit
                         start
                         end))))))))
