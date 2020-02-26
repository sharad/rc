;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
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

(defun lotus-override/init-emacsql-sqlite-config ()
  (interactive)
  (when t ;; unless (boundp 'emacsql-sqlite-executable)
    (setq emacsql-sqlite-executable
      (expand-file-name emacsql-sqlite-executable-path
                        (if (or (file-writable-p emacsql-sqlite-data-root)
                                (file-exists-p (expand-file-name
                                                emacsql-sqlite-executable-path
                                                emacsql-sqlite-data-root)))
                            emacsql-sqlite-data-root
                          (expand-file-name
                           (concat "emacsql/" emacsql-version)
                           user-emacs-directory)))))
  (if emacsql-sqlite-executable
      (setq emacsql-sqlite-executable
            (concat emacsql-sqlite-executable "-"
                    (if (file-exists-p "/run/current-system/profile/bin/guix")
                        "guix"
                      "ubuntu")))))

(defun lotus-override/init-lsdb-config ()
  (progn
    (defun lsdb-gnus-update-record ()
      (with-current-buffer (or
                            gnus-article-current-summary
                            gnus-original-article-buffer
                            (current-buffer))
        (lsdb-update-records-and-display)))))

(defun lotus-override/post-init-git-gutter+-config ()
  (progn
    ;; Error
    ;; "Selecting deleted buffer"
    ;; is coming from set-buffer
    ;; in (helm-resume)
    ;; (helm-resume-select-buffer)
    ;; helm-last-buffer
    ;; (helm-resume (nth arg helm-buffers))
    ;; (helm-resume-p resume)
    ;; basically we need to clean up helm-buffers from deleted buffers
    ;; so advice helm-resume to clean up.

    ;;or basically in case of this error
    ;; helm should not call helm-resume by default.

    ;;other similar issue https://github.com/syl20bnr/spacemacs/issues/6945
    ;;but not with helm

    ;;TODO: after-change-major-mode-hook from subr.el
    ;;TODO: with-current-buffer          from subr.el
    ;; here a new run-hooks like run-hook-failsafe
    ;; which will remove hook function it throw error
    ;; than after-change-major-mode-hook should be run with it.
    ;; also with-current-buffer buffer could also add some error check for
    ;; for killed buffers.

    ;;; from subr.el

    ;; (defmacro with-current-buffer (buffer-or-name &rest body)
    ;;           "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
    ;; BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
    ;; The value returned is the value of the last form in BODY.  See
    ;; also `with-temp-buffer'."
    ;;           (declare (indent 1) (debug t))
    ;;           `(save-current-buffer
    ;;              (set-buffer ,buffer-or-name)
    ;;              ,@body))

    (message "Fixed error \"Selecting deleted buffer\" in git-gutter+ package")

    (defun git-gutter+-reenable-buffers ()
      ;; (message "start git-gutter+-reenable-buffers")
      (dolist (buf git-gutter+-buffers-to-reenable)
        (if (and buf
                 (bufferp buf)
                 (buffer-live-p buf))
            (with-current-buffer buf (git-gutter+-turn-on))
          (message "buffer %s is not buffer or already killed" buf)))
      (prog1
          (setq git-gutter+-buffers-to-reenable nil)))

    (defun git-gutter+-diff (curfile)
      ;; (debug)
      (let ((args (git-gutter+-diff-args curfile))
            (file (buffer-file-name))) ;; for tramp
        (with-temp-buffer
          (if (git-gutter+-insert-git-output args file)
              (progn (goto-char (point-min))
                     (let ((diff-header (git-gutter+-get-diff-header))
                           (diffinfos   (git-gutter+-get-diffinfos)))
                       (when (and diff-header
                                  diffinfos)
                        (list diff-header diffinfos))))
            (let ((git-gutter+-output (buffer-string)))
              (prog1
                  nil
                ;; (message "Error callling git diff:\n%s" (buffer-string))
                (message "Error callling git diff %s:\n%s" curfile git-gutter+-output)))))))))

(defun lotus-override/post-init-git-link-config ()
  (progn
    (defun git-link--branch-merge (branch)
      (git-link--get-config (format "branch.%s.merge" branch)))

    (defun git-link--branch-remote-merge (&optional branch noerror)
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
      (interactive
       (let* ((remote (git-link--select-remote))
              (region (when (or buffer-file-name (git-link--using-magit-blob-mode))
                        (git-link--get-region))))
         (list remote (car region) (cadr region))))

      (let (filename
            branch
            commit handler
            remote-info
            (remote-url (git-link--remote-url remote)))
        (if (null remote-url)
            (message "Remote `%s' not found" remote)

          (setq remote-info (git-link--parse-remote remote-url)
                filename    (git-link--relative-filename)
                branch      (or (git-link--branch-remote-merge)
                                (git-link--branch))
                commit      (git-link--commit)
                handler     (git-link--handler git-link-remote-alist
                                               (car remote-info)))

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
                             end))))))))))

(defun lotus-override/post-init-erc-identd-config ()
  (defun erc-identd-start (&optional port)
    "Start an identd server listening to port 8113.
Port 113 (auth) will need to be redirected to port 8113 on your
machine -- using iptables, or a program like redir which can be
run from inetd.  The idea is to provide a simple identd server
when you need one, without having to install one globally on your
system."
    (interactive (list (read-string "Serve identd requests on port: " "8113")))
    (unless port (setq port erc-identd-port))
    (when (stringp port)
      (setq port (string-to-number port)))
    (when erc-identd-process
      (delete-process erc-identd-process))
    (setq erc-identd-process
          (make-network-process :name "identd"
                                :buffer nil
                                :host 'local :service port
                                :server t :noquery t :nowait nil
                                :filter 'erc-identd-filter))
    (set-process-query-on-exit-flag erc-identd-process nil)))


(defun lotus-override/post-init-org-agenda-config ()
  (interactive)
  (progn
    (defun org-agenda-new-marker (&optional pos)
      "Return a new agenda marker.
Maker is at point, or at POS if non-nil.  Org mode keeps a list of
these markers and resets them when they are no longer in use."
      (let ((m (copy-marker (or pos (point)) t)))
        (setq org-agenda-last-marker-time (float-time))
        (if (and org-agenda-buffer
                 (bufferp org-agenda-buffer)
                 (buffer-live-p org-agenda-buffer))
            (with-current-buffer org-agenda-buffer
              (push m org-agenda-markers))
          (push m org-agenda-markers))
        m))))

(defun override-vc-registered ()
  (progn
    (defun vc-registered (file)
      "Return non-nil if FILE is registered in a version control system.

This function performs the check each time it is called.  To rely
on the result of a previous call, use `vc-backend' instead.  If the
file was previously registered under a certain backend, then that
backend is tried first."
      (let (handler)
        (cond
         ((and (file-name-directory file)
               (string-match vc-ignore-dir-regexp (file-name-directory file)))
          nil)
         ((and (boundp 'file-name-handler-alist)
               (setq handler (find-file-name-handler file 'vc-registered)))
          ;; handler should set vc-backend and return t if registered
          (funcall handler 'vc-registered file))
         (t
          ;; There is no file name handler.
          ;; Try vc-BACKEND-registered for each handled BACKEND.
          (catch 'found
            (let ((backend (vc-file-getprop file 'vc-backend)))
              (mapc
               (lambda (b)
                 (and (or (vc-call-backend b 'registered file)
                          (vc-call-backend b 'registered (file-truename file)))
                      (vc-file-setprop file 'vc-backend b)
                      (throw 'found t)))
               (if (or (not backend) (eq backend 'none))
                   vc-handled-backends
                 (cons backend vc-handled-backends))))
            ;; File is not registered.
            (vc-file-setprop file 'vc-backend 'none)
            nil)))))))

(defun lotus-override/post-init-vc-config ()
  (override-vc-registered))

(defun lotus-override/post-init-vc-hooks-config ()
  (override-vc-registered))

(defun lotus-override/post-init-vc-git-config ()
  (defun vc-git-mode-line-string (file)
    "Return a string for `vc-mode-line' to put in the mode line for FILE."
    (let* ((rev (vc-working-revision file 'Git))
           (disp-rev (or (vc-git--symbolic-ref file)
                         (when rev (substring rev 0 7))))
           (def-ml (vc-default-mode-line-string 'Git file))
           (help-echo (when def-ml (get-text-property 0 'help-echo def-ml)))
           (face   (when def-ml (get-text-property 0 'face def-ml))))
      (propertize (concat (substring def-ml 0 4) disp-rev)
                  'face face
                  'help-echo (concat help-echo "\nCurrent revision: " rev)))))

(defun lotus-override/all-functions ()
  (interactive)
  (lotus-override/init-emacsql-sqlite-config)
  (lotus-override/init-lsdb-config)
  (lotus-override/post-init-git-gutter+-config)
  (lotus-override/post-init-git-link-config)
  (lotus-override/post-init-org-agenda-config)
  (lotus-override/post-init-vc-config)
  (lotus-override/post-init-vc-hooks-config)
  (lotus-override/post-init-vc-git-config))

;;; config.el ends here
