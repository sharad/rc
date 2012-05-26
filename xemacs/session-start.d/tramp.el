

;; for editing remote files
;;(require 'tramp) ;stop error caused by no availability of tramp.
;; check C-h f require

(deh-require-maybe 'tramp
    (setq tramp-default-method "ssh"
          ido-enable-tramp-completion t ;this guy was missing
          tramp-debug-buffer t
          tramp-verbose 10
          tramp-default-user 'nil
          tramp-default-host "spratap")
    ;; http://www.gnu.org/software/tramp/#Remote-shell-setup
    (setenv "ESHELL" "bash")

    (tramp-set-completion-function "ssh"
                                   '((tramp-parse-sconfig "/etc/ssh_config")
                                     (tramp-parse-sconfig "~/.ssh/config")))


    ;; (defun sudo-edit (&optional arg)
    ;;   (interactive "p")
    ;;   (if arg
    ;;       (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
    ;;       (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

    ;; (defun sudo-edit-current-file ()
    ;;   (interactive)
    ;;   (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))

    ;; (global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)


    ;; remove p4
    (setq vc-handled-backends '( ;P4
                                RCS CVS SVN SCCS Bzr Git Hg Mtn Arch))

    (defun find-alternative-file-with-sudo () ; put in keybinding.el
      (interactive)
      (let ((fname (or buffer-file-name
                       dired-directory))
            ;; P4 creating problem.
            (vc-handled-backends '(RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)))
        (when fname
          (if (string-match "^/sudo:root@localhost:" fname)
              (setq fname (replace-regexp-in-string
                           "^/sudo:root@localhost:" ""
                           fname))
              (setq fname (concat "/sudo:root@localhost:" fname)))
          (find-alternate-file fname))))

    ;; http://www.gnu.org/software/tramp/#Multi_002dhops
    (add-to-list 'tramp-default-proxies-alist
                 `(,tramp-default-proxie "\\`root\\'" "/ssh:%h:"))
    ;; If you, for example, wants to work as ‘root’ on hosts in the
    ;; domain ‘your.domain’, but login as ‘root’ is disabled for
    ;; non-local access, you might add the following rule:

    ;; (add-to-list 'tramp-default-proxies-alist
    ;;              '("\\.your\\.domain\\'" "\\`root\\'" "/ssh:%h:"))

    ;; Opening /sudo:randomhost.your.domain: would connect first
    ;; ‘randomhost.your.domain’ via ssh under your account name, and
    ;; perform sudo -u root on that host afterwards. It is important to
    ;; know that the given method is applied on the host which has been
    ;; reached so far. sudo -u root, applied on your local host,
    ;; wouldn't be useful here.

;; {{http://ubuntuforums.org/archive/index.php/t-1375454.html
;; TRAMP beep when done downloading files
(defadvice tramp-handle-write-region
(after tramp-write-beep-advice activate)
" make tramp beep after writing a file."
(interactive)
(beep))
(defadvice tramp-handle-do-copy-or-rename-file
(after tramp-copy-beep-advice activate)
" make tramp beep after copying a file."
(interactive)
(beep))
(defadvice tramp-handle-insert-file-contents
(after tramp-copy-beep-advice activate)
" make tramp beep after copying a file."
(interactive)
(beep))
;; }}
    )


(defun update-ssh-agent ()
  (interactive)
  (let ((agent-file (concat "~/.emacs.d/ssh-agent-" (getenv "HOST") ".el"))
        ;; (agent-file (concat "~/.emacs.d/ssh-agent-" (system-name) ".el"))
        )
    (if (and
         (null (getenv "SSH_AGENT_PID")))
        (if (file-exists-p agent-file)
            (load agent-file t t)
            (message "Unable to find agent file.")))))

(defadvice tramp-file-name-handler
    (before ad-update-ssh-agent-env activate)
  "Support ssh agent."
  (update-ssh-agent))

(when nil
 (defun tramp-do-file-attributes-with-stat
     (vec localname &optional id-format)
   "Implement `file-attributes' for Tramp files using stat(1) command."
   (tramp-message vec 5 "file attributes with stat: %s" localname)
   (tramp-send-command-and-read
    vec
    (format
     ;; "((%s %s || %s -h %s) && %s -c '((\"%%N\") %%h %s %s %%Xe0 %%Ye0 %%Ze0 %%se0 \"%%A\" t %%ie0 -1)' %s || echo nil)"
     "((%s %s || %s -h %s) && %s -c '((\"%%n\") %%h %s %s %%Xe0 %%Ye0 %%Ze0 %%se0 \"%%A\" t %%ie0 -1)' %s || echo nil)"
     (tramp-get-file-exists-command vec)
     (tramp-shell-quote-argument localname)
     (tramp-get-test-command vec)
     (tramp-shell-quote-argument localname)
     (tramp-get-remote-stat vec)
     (if (eq id-format 'integer) "%u" "\"%U\"")
     (if (eq id-format 'integer) "%g" "\"%G\"")
     (tramp-shell-quote-argument localname))))



 (defun tramp-do-directory-files-and-attributes-with-stat
     (vec localname &optional id-format)
   "Implement `directory-files-and-attributes' for Tramp files using stat(1) command."
   (tramp-message vec 5 "directory-files-and-attributes with stat: %s" localname)
   (tramp-send-command-and-read
    vec
    (format
     (concat
      ;; We must care about filenames with spaces, or starting with
      ;; "-"; this would confuse xargs.  "ls -aQ" might be a solution,
      ;; but it does not work on all remote systems.  Therefore, we
      ;; quote the filenames via sed.
      "cd %s; echo \"(\"; (%s -a | sed -e s/\\$/\\\"/g -e s/^/\\\"/g | xargs "
      ; "%s -c '(\"%%n\" (\"%%N\") %%h %s %s %%Xe0 %%Ye0 %%Ze0 %%se0 \"%%A\" t %%ie0 -1)'); "
      "%s -c '(\"%%n\" (\"%%n\") %%h %s %s %%Xe0 %%Ye0 %%Ze0 %%se0 \"%%A\" t %%ie0 -1)'); "
      "echo \")\"")
     (tramp-shell-quote-argument localname)
     (tramp-get-ls-command vec)
     (tramp-get-remote-stat vec)
     (if (eq id-format 'integer) "%u" "\"%U\"")
     (if (eq id-format 'integer) "%g" "\"%G\"")))))




;; (setq vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|etc")


;; vc-handled-backends is a variable defined in `vc-hooks.el'.
;; Its value is
;; (P4 RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)



(autoload 'password-in-cache-p "password-cache")

(user-provide 'tramp)
