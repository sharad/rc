

(when nil

  (toggle-debug-on-error)


  (progn
    (advice-remove 'set-buffer #'set-buffer-debug)

    (defalias 'org-set-buffer #'set-buffer)

    (defmacro with-current-buffer-org-set-buffer (buffer-or-name &rest body)
      "Execute the forms in BODY with BUFFER-OR-NAME temporarily current.
 BUFFER-OR-NAME must be a buffer or the name of an existing buffer.
 The value returned is the value of the last form in BODY.  See
 also `with-temp-buffer'."
      (declare (indent 1) (debug t))
      `(save-current-buffer
         (org-set-buffer ,buffer-or-name)
         ,@body))

    (defmacro with-output-to-temp-buffer-org-set-buffer (bufname &rest body)
      (declare (debug t))
      (let ((old-dir (make-symbol "old-dir"))
            (buf (make-symbol "buf")))
        `(let* ((,old-dir default-directory)
                (,buf
                 (with-current-buffer-org-set-buffer (get-buffer-create ,bufname)
                                                     (prog1 (current-buffer)
                                                       (kill-all-local-variables)
                                                       ;; FIXME: delete_all_overlays
                                                       (setq default-directory ,old-dir)
                                                       (setq buffer-read-only nil)
                                                       (setq buffer-file-name nil)
                                                       (setq buffer-undo-list t)
                                                       (let ((inhibit-read-only t)
                                                             (inhibit-modification-hooks t))
                                                         (erase-buffer)
                                                         (run-hooks 'temp-buffer-setup-hook)))))
                (standard-output ,buf))
           (prog1 (progn ,@body)
             (internal-temp-output-buffer-show ,buf)))))

    (defun backtrace-to-buffer (buf)
      ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Internals-of-Debugger.html
      (with-output-to-temp-buffer-org-set-buffer buf ; "backtrace-output"
                                                 (let ((var 1))
                                                   (save-excursion
                                                     (setq var
                                                           (eval
                                                            '(progn
                                                               (if (boundp 'var) (1+ var))
                                                               (list 'testing (backtrace)))))))))

    (setq set-buffer-debug-file "~/tmp/set-buffer-debug-file-xx")

    (defun set-buffer-debug (orig-fun &rest args)
      (let* ((buff (car args)))
        (if (buffer-live-p buff)
            (apply orig-fun args)
          (let* ((debug-file set-buffer-debug-file)
                 (debug-buffer
                  (find-file-noselect debug-file)))
            (backtrace-to-buffer debug-buffer)
            (with-current-buffer-org-set-buffer debug-buffer
                                                (write-file debug-file))
            (apply orig-fun args)))))

    (defun set-buffer-debug-insinuate ()
      (interactive)
      (advice-add 'set-buffer :around #'set-buffer-debug))

    (defun set-buffer-debug-uninsinuate ()
      (interactive
           (advice-remove 'set-buffer #'set-buffer-debug))))
  (progn

    ;;replace set-buffer by set-buffer-debug

    (defun backtrace-to-buffer (buf)
      ;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Internals-of-Debugger.html
      (with-output-to-temp-buffer buf ; "backtrace-output"
        (let ((var 1))
          (save-excursion
            (setq var
                  (eval
                   '(progn
                      (if (boundp 'var) (1+ var))
                      (list 'testing (backtrace)))))))))

    (setq set-buffer-debug-file "~/tmp/set-buffer-debug-file-xx")

    (defun set-buffer-debug (buff)
      (let* ((buff buff))
        (if (buffer-live-p buff)
            (apply orig-fun args)
          (let* ((debug-file set-buffer-debug-file)
                 (debug-buffer
                  (find-file-noselect debug-file)))
            (backtrace-to-buffer debug-buffer)
            (with-current-buffer debug-buffer
              (write-file debug-file
                 (apply orig-fun args))))))) Emacs code present in ~/debian/build-src))
;; just use gdb with break buffer.c:2123
;; and find out who is culprit.




(when nil
  (defun org-clock-load ()
    "Load clock-related data from disk, maybe resuming a stored clock."
    (when (and org-clock-persist (not org-clock-loaded))
      (if (not (file-readable-p org-clock-persist-file))
          (message "Not restoring clock data; %S not found" org-clock-persist-file)
        (message "Restoring clock data")
        ;; Load history.
        (load-file org-clock-persist-file)
        (setq org-clock-loaded t)
        (pcase-dolist (`(,(and file (pred file-exists-p)) . ,position)
                       org-clock-stored-history)
          (org-clock-history-push position (find-file-noselect file)))
        ;; Resume clock.
        (pcase org-clock-stored-resume-clock
          (`(,(and file (pred file-exists-p)) . ,position)
           (with-current-buffer (find-file-noselect file)
             (when (or (not org-clock-persist-query-resume)
                       (y-or-n-p (format "Resume clock (%s) "
                                         (save-excursion
                                           (goto-char position)
                                           (org-get-heading t t)))))
               (goto-char position)
               (let ((org-clock-in-resume 'auto-restart)
                     (org-clock-auto-clock-resolution nil))
                 (org-clock-in)
                 (when (org-invisible-p) (org-show-context))))))
          (_ nil))))))
  







(when nil

  (defun run-hooks (&rest hooks)
    (dolist (hook hooks)
      (dolist (f (symbol-value hook))
        (condition-case e
            (unless (eq t f)
              (funcall f))
          (error
           (message "Error: function: %s error %s hooks %s"
                    f
                    e
                    hooks))))))
  (progn
    (setq helm--remap-mouse-mode-off-hook nil)
    (setq helm--remap-mouse-mode-on-hook nil)
    (setq evil-local-mode-on-hook nil)
    (setq evil-local-mode-off-hook nil)
    (setq font-lock-mode-off-hook nil))


  (defun git-gutter+-reenable-buffers ()
    (dolist (buf git-gutter+-buffers-to-reenable)
      (if (and
           (bufferp buf) (buffer-live-p buf))
          (with-current-buffer buf
            (git-gutter+-turn-on))
        (message "buffer %s is not buffer or already killed" buf)))
    (setq git-gutter+-buffers-to-reenable nil)))


;; git-gutter+-reenable-buffers error (error Selecting deleted buffer) hooks (after-change-major-mode-hook)

;; (git-gutter+-reenable-buffers)
