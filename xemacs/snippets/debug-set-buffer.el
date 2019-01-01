






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
    (interactive)
    (advice-remove 'set-buffer #'set-buffer-debug)))





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
            (write-file debug-file))
          (apply orig-fun args))))))

;;; Emacs code present in ~/debian/build-src
;; just use gdb with break buffer.c:2123
;; and find out who is culprit.
