

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
        nil)))))
