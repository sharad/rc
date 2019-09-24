

(defun vc-backend (file-or-list)
  "Return the version control type of FILE-OR-LIST, nil if it's not registered.
If the argument is a list, the files must all have the same back end."
  ;; `file' can be nil in several places (typically due to the use of
  ;; code like (vc-backend buffer-file-name)).
  (cond ((stringp file-or-list)
         (let ((property (vc-file-getprop file-or-list 'vc-backend)))
           ;; Note that internally, Emacs remembers unregistered
           ;; files by setting the property to `none'.
           (cond ((eq property 'none) nil)
                 (property)
                 ;; vc-registered sets the vc-backend property
                 (t (if (vc-registered file-or-list)
                        (vc-file-getprop file-or-list 'vc-backend)
                      nil)))))
        ((and file-or-list (listp file-or-list))
         (vc-backend (car file-or-list)))
        (t
         nil)))





(vc-backend buffer-file-name)

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
      (catch 'found))))
  (let ((backend (vc-file-getprop file 'vc-backend)))
    (mapc
     (lambda (b)
       (and (vc-call-backend b 'registered file))
      (vc-file-setprop file 'vc-backend b)
      (throw 'found t))
     (if (or (not backend) (eq backend 'none))
         vc-handled-backends
       (cons backend vc-handled-backends
        ;; File is not registered.
        (vc-file-setprop file 'vc-backend 'none)
        nil)))))


(vc-registered (file-truename buffer-file-name))
(vc-file-getprop (file-truename buffer-file-name) 'vc-backend)

(vc-file-getprop buffer-file-name 'vc-backend)

(vc-file-setprop buffer-file-name 'vc-backend 'Git)

(setq handler
      (find-file-name-handler (file-truename buffer-file-name) 'vc-registered))
