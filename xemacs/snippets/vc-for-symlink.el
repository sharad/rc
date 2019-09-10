




(defun vc-deduce-fileset (&optional observer allow-unregistered
                                    state-model-only-files)
  "Deduce a set of files and a backend to which to apply an operation.
Return (BACKEND FILESET FILESET-ONLY-FILES STATE CHECKOUT-MODEL).

If we're in VC-dir mode, FILESET is the list of marked files,
or the directory if no files are marked.
Otherwise, if in a buffer visiting a version-controlled file,
FILESET is a single-file fileset containing that file.
Otherwise, if ALLOW-UNREGISTERED is non-nil and the visited file
is unregistered, FILESET is a single-file fileset containing it.
Otherwise, throw an error.

STATE-MODEL-ONLY-FILES if non-nil, means that the caller needs
the FILESET-ONLY-FILES STATE and MODEL info.  Otherwise, that
part may be skipped.

BEWARE: this function may change the current buffer."
  ;; FIXME: OBSERVER is unused.  The name is not intuitive and is not
  ;; documented.  It's set to t when called from diff and print-log.
  (let (backend)
    (cond
     ((derived-mode-p 'vc-dir-mode)
      (vc-dir-deduce-fileset state-model-only-files))
     ((derived-mode-p 'dired-mode)
      (if observer
          (vc-dired-deduce-fileset)
        (error "State changing VC operations not supported in `dired-mode'")))
     ((setq backend (vc-backend buffer-file-name))
      (if state-model-only-files
          (list backend (list buffer-file-name)
                (list buffer-file-name)
                (vc-state buffer-file-name)
                (vc-checkout-model backend buffer-file-name))
        (list backend (list buffer-file-name))))
     ((and (buffer-live-p vc-parent-buffer)
           ;; FIXME: Why this test?  --Stef
           (or (buffer-file-name vc-parent-buffer)
               (with-current-buffer vc-parent-buffer
                 (derived-mode-p 'vc-dir-mode))))
      (progn                  ;FIXME: Why not `with-current-buffer'? --Stef.
        (set-buffer vc-parent-buffer)
        (vc-deduce-fileset observer allow-unregistered state-model-only-files)))
     ((and (derived-mode-p 'log-view-mode)
           (setq backend (vc-responsible-backend default-directory)))
      (list backend nil))
     ((not buffer-file-name)
      (error "Buffer %s is not associated with a file" (buffer-name)))
     ((and allow-unregistered (not (vc-registered buffer-file-name)))
      (if state-model-only-files
          (list (vc-backend-for-registration (buffer-file-name))
                (list buffer-file-name)
                (list buffer-file-name)
                (when state-model-only-files 'unregistered)
                nil)
        (list (vc-backend-for-registration (buffer-file-name))
              (list buffer-file-name))))
     (t (error "File is not under version control")))))
