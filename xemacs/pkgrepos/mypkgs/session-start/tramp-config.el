

;; for editing remote files
;;(require 'tramp) ;stop error caused by no availability of tramp.
;; check C-h f require

(eval-after-load "tramp"
  '(sharad/disable-startup-inperrupting-feature))

(deh-require-maybe tramp

  (setq                                 ;very necessary.
   tramp-mode nil
   ido-mode nil)

  (deh-require-maybe ido
    (setq
     ;; ido-enable-tramp-completion t ;this guy was missing
     ;; ido-enable-tramp-completion nil ;this guy was missing
     ido-enable-tramp-completion t ;Very much require to complete tramp user /server names.
     ido-record-ftp-work-directories t ;must be true for tramp partial match, very useful.
     ))


  (deh-section "ido tramp problem"
    (when nil
      (setq ido-dir-file-cache (remove-if-not
        (lambda (f)
            (if (ido-is-ftp-directory (car f))
                 (eq (caadr f) 'ftp)
                 t))
            ido-dir-file-cache))
      ))


  (setq ;; tramp-default-method "ssh"
   ;; tramp-default-method "scpc" <- default
   tramp-debug-buffer t
   ;; tramp-verbose 10
   tramp-verbose 1
   tramp-default-user nil
   tramp-default-host "spratap")
  ;; http://www.gnu.org/software/tramp/#Remote-shell-setup
  (setenv "ESHELL" "bash")

  (tramp-set-completion-function "ssh"
                                 '((tramp-parse-sconfig "/etc/ssh_config")
                                   (tramp-parse-sconfig "~/.ssh/config")))

  (ignore-errors
    (deh-section "GVFS DBUS TRAMP IDO Avahi"
     ;; it is not working find why, get it working
     (deh-require-maybe tramp-gvfs
       ;; need it.
       ;; http://comments.gmane.org/gmane.emacs.tramp/6704
       ;; http://www.gnu.org/software/emacs/manual/html_node/tramp/GVFS-based-methods.html
      )))



  (deh-section "All Tramp"

    (deh-require-maybe (progn
                         tramp-cache
                         tramp-cmds
                         tramp-compat
                         tramp-fish
                         tramp-ftp
                         ;; tramp-gvfs
                         tramp-gw
                         tramp-imap
                         tramp-smb
                         tramp-uu
                         trampver)))


  ;; (defun sudo-edit (&optional arg)
  ;;   (interactive "p")
  ;;   (if arg
  ;;       (find-file (concat "/sudo:root@localhost:" (ido-read-file-name "File: ")))
  ;;       (find-alternate-file (concat "/sudo:root@localhost:" buffer-file-name))))

  ;; (defun sudo-edit-current-file ()
  ;;   (interactive)
  ;;   (find-alternate-file (concat "/sudo:root@localhost:" (buffer-file-name (current-buffer)))))

  ;; (global-set-key (kbd "C-c C-r") 'sudo-edit-current-file)



  (defun find-alternative-file-with-sudo () ; put in keybinding.el
    (interactive)
    (let ((fname (or buffer-file-name
                     dired-directory)))
      (when fname
        (if (string-match "^/sudo:root@localhost:" fname)
            (setq fname (replace-regexp-in-string
                         "^/sudo:root@localhost:" ""
                         fname))
            (setq fname (concat "/sudo:root@localhost:" fname)))
        (find-alternate-file fname))))

  ;; http://www.gnu.org/software/tramp/#Multi_002dhops
  (deh-require-maybe common-info
    (add-to-list 'tramp-default-proxies-alist
                 `(,tramp-default-proxie "\\`root\\'" "/ssh:%h:")))
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



  (defun ssh-agent-add-key ()
    (require 'misc-config)
    (provide 'host-info)
    ;; (message "Calling update-ssh-agent > ssh-agent-add-key")
    (if (and
         (boundp 'ssh-key-file)
         ssh-key-file)
        (unless (or (not tramp-mode)
                    (shell-command-no-output "ssh-add -l < /dev/null"))
          (shell-command-no-output (concat "ssh-add " ssh-key-file " < /dev/null")))
        (error "No ssh-key-file defined")))

  (defun update-ssh-agent (&optional force)
    (interactive "P")
    ;; (message "update-ssh-agent called")
    (if ido-auto-merge-timer
        (timer-activate ido-auto-merge-timer t))
    (unwind-protect
         (save-excursion
           (let ((enable-recursive-minibuffers t))
             (unless (tramp-tramp-file-p default-directory)
               (let ((agent-file (concat "~/.emacs.d/ssh-agent-" (system-name) ".el")))
                 ;; (if (or force (null (getenv "SSH_AGENT_PID")))
                 (if (or force (null (getenv "SSH_AGENT_PID")))
                     (if (file-exists-p agent-file)
                         (progn
                           (if force (tramp-cleanup-all-connections))
                           ;; (load agent-file t t)
                           (setenv "SSH_AGENT_PID" (getenv "SSH_AGENT_PID" (selected-frame)))
                           (setenv "SSH_AUTH_SOCK" (getenv "SSH_AUTH_SOCK" (selected-frame)))
                           (ssh-agent-add-key)
                           (message "update main pid and sock to frame pid %s sock %s"
                                    (getenv "SSH_AGENT_PID" (selected-frame))
                                    (getenv "SSH_AUTH_SOCK" (selected-frame)))
                           ;; (message "loading %s" agent-file)
                           )
                         (message "Unable to find agent file."))
                     (ssh-agent-add-key)))
               (let ()
                 ;; (message "update-ssh-agent yes authinfo")
                 (find-file-noselect (or
                                      (plist-get (car auth-sources) :source)
                                      "~/.authinfo.gpg"))))))
      (if ido-auto-merge-timer
          (timer-activate ido-auto-merge-timer))))

  (defadvice tramp-file-name-handler
      (before ad-update-ssh-agent-env activate)
    "Support ssh agent."
    (unless (tramp-tramp-file-p default-directory)
      (update-ssh-agent)))

  ;; run, do not run it, it trouble at start-up time.
  ;; (update-ssh-agent)

  (defun tramp-output-wash (&optional arg)
    (interactive)
    (save-excursion
      (let ((buffer-read-only nil))
        (goto-char (point-min))

        (while (re-search-forward "\n\\$ " nil t)
          (replace-match "\n$\n" nil nil))

        ;;(replace-regexp "\n\\$ " "\n$\n")
        )))

  (add-hook 'grep-mode-hook #'tramp-output-wash)
  (add-hook 'cscope-list-entry-hook #'tramp-output-wash)

  )

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



;;{{ from: http://stackoverflow.com/a/4371566
;; throwing error.
;; for emacs tramp timeout.
;; (defun tramp-find-file-timeout ()
;;   ;; (when tramp
;;   (when tramp-mode
;;     (with-timeout (4)
;;       (keyboard-quit))))
;; (add-hook 'find-file-hook 'tramp-find-file-timeout)
;;}}



;; (setq vc-ignore-dir-regexp "\\`\\(?:[\\/][\\/][^\\/]+[\\/]\\|/\\(?:net\\|afs\\|\\.\\.\\.\\)/\\)\\'\\|etc")


;; vc-handled-backends is a variable defined in `vc-hooks.el'.
;; Its value is
;; (P4 RCS CVS SVN SCCS Bzr Git Hg Mtn Arch)



(autoload 'password-in-cache-p "password-cache")

;; believe it need not be here
;; (sharad/disable-startup-inperrupting-feature)


(provide 'tramp-config)
