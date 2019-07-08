


(defun org-capture-plus (type target template &rest plist)
    "Capture something.
  \\<org-capture-plus-mode-map>
  This will let you select a template from `org-capture-templates', and
  then file the newly captured information.  The text is immediately
  inserted at the target location, and an indirect buffer is shown where
  you can edit it.  Pressing `\\[org-capture-plus-finalize]' brings you back to the \
  previous
  state of Emacs, so that you can continue your work.

  When called interactively with a `\\[universal-argument]' prefix argument \
  GOTO, don't
  capture anything, just go to the file/headline where the selected
  template stores its notes.

  With a `\\[universal-argument] \\[universal-argument]' prefix argument, go to \
  the last note stored.

  When called with a `C-0' (zero) prefix, insert a template at point.

  When called with a `C-1' (one) prefix, force prompting for a date when
  a datetree entry is made.

  ELisp programs can set KEYS to a string associated with a template
  in `org-capture-templates'.  In this case, interactive selection
  will be bypassed.

  If `org-capture-use-agenda-date' is non-nil, capturing from the
  agenda will use the date at point as the default date.  Then, a
  `C-1' prefix will tell the capture process to use the HH:MM time
  of the day at point (if any) or the current HH:MM time."
    ;; (interactive "P")

    (when (and org-capture-use-agenda-date
               (eq major-mode 'org-agenda-mode))
      (setq org-overriding-default-time
            (org-get-cursor-date t))) ;; (equal goto 1)


    (let* ((orig-buf (current-buffer))
           (annotation (if (and (boundp 'org-capture-link-is-already-stored)
                                org-capture-link-is-already-stored)
                           (plist-get org-store-link-plist :annotation)
                         (ignore-errors (org-store-link nil))))
           ;; (template (or org-capture-entry (org-capture-select-template keys)))
           (template (or org-capture-entry
                         (org-capture-plus-get-template template)))
           initial)
      (setq initial (or org-capture-initial
                        (and (org-region-active-p)
                             (buffer-substring (point) (mark)))))
      (when (stringp initial)
        (remove-text-properties 0 (length initial) '(read-only t) initial))
      (when (stringp annotation)
        (remove-text-properties 0 (length annotation)
                                '(read-only t) annotation))



      ;; (org-capture-set-plist template)

      (setq org-capture-plist plist)
      (org-capture-put
       ;; :key (car entry)
       ;; :description (nth 1 entry)
       :target target)

      (let ((txt template)
            (type (or type 'entry)))
        (when (or (not txt) (and (stringp txt) (not (string-match "\\S-" txt))))
          ;; The template may be empty or omitted for special types.
          ;; Here we insert the default templates for such cases.
          (cond
           ((eq type 'item) (setq txt "- %?"))
           ((eq type 'checkitem) (setq txt "- [ ] %?"))
           ((eq type 'table-line) (setq txt "| %? |"))
           ((member type '(nil entry)) (setq txt "* %?\n  %a"))))
        (org-capture-put :template txt :type type))

      (org-capture-get-template)

      (org-capture-put :original-buffer orig-buf
                       :original-file (or (buffer-file-name orig-buf)
                                          (and (featurep 'dired)
                                               (car (rassq orig-buf
                                                           dired-buffers))))
                       :original-file-nondirectory
                       (and (buffer-file-name orig-buf)
                            (file-name-nondirectory
                             (buffer-file-name orig-buf)))
                       :annotation annotation
                       :initial initial
                       :return-to-wconf (current-window-configuration)
                       :default-time
                       (or org-overriding-default-time
                           (org-current-time)))

      (org-capture-set-target-location-improved)

      (condition-case error
          (org-capture-put :template (org-capture-fill-template))
        ((error quit)
         (if (get-buffer "*Capture*") (kill-buffer "*Capture*"))
         (error "Capture abort: %s" error)))

      (setq org-capture-clock-keep (org-capture-get :clock-keep))
      (if (and
           (not (org-capture-get :target))
           (eq 'immdediate (car (org-capture-get :target)))) ;; (equal goto 0)
          ;;insert at point
          (org-capture-insert-template-here)
        (progn
          (org-capture-place-template
           (eq (car (org-capture-get :target)) 'function)))
        (if (and (derived-mode-p 'org-mode)
                 (org-capture-get :clock-in))
            (condition-case nil
                (progn
                  (if (org-clock-is-active)
                      (org-capture-put :interrupted-clock
                                       (copy-marker org-clock-marker)))
                  (org-clock-in)
                  (setq-local org-capture-clock-was-started t))
              (error
               "Could not start the clock in this capture buffer")))
        (if (org-capture-get :immediate-finish)
            (org-capture-plus-finalize)))))
