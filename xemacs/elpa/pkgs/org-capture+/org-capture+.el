;; Required libraries

;; [[file:~/.xemacs/elpa/pkgs/org-capture+/org-capture+.org::*Required%20libraries][Required libraries:1]]
(require 'org-capture)
;; Required libraries:1 ends here

;; set target improved

;; [[file:~/.xemacs/elpa/pkgs/org-capture+/org-capture+.org::*set%20target%20improved][set target improved:1]]
(defun org-capture-set-target-location-improved (&optional target)
  "Find TARGET buffer and position.
Store them in the capture property list."
  (let ((target-entry-p t))
    (save-excursion
      (pcase (or target (org-capture-get :target))
        (`(file ,path)
          (set-buffer (org-capture-target-buffer path))
          (org-capture-put-target-region-and-position)
          (widen)
          (setq target-entry-p nil))
        (`(id ,id)
          (pcase (org-id-find id)
            (`(,path . ,position)
              (set-buffer (org-capture-target-buffer path))
              (widen)
              (org-capture-put-target-region-and-position)
              (goto-char position))
            (_ (error "Cannot find target ID \"%s\"" id))))
        (`(file+headline ,path ,headline)
          (set-buffer (org-capture-target-buffer path))
          ;; Org expects the target file to be in Org mode, otherwise
          ;; it throws an error.  However, the default notes files
          ;; should work out of the box.  In this case, we switch it to
          ;; Org mode.
          (unless (derived-mode-p 'org-mode)
            (org-display-warning
             (format "Capture requirement: switching buffer %S to Org mode"
                     (current-buffer)))
            (org-mode))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))
          (if (re-search-forward (format org-complex-heading-regexp-format
                                         (regexp-quote headline))
                                 nil t)
              (beginning-of-line)
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert "* " headline "\n")
              (beginning-of-line 0)))
        (`(file+olp ,path . ,outline-path)
          (let ((m (org-find-olp (cons (org-capture-expand-file path)
                                       outline-path))))
            (set-buffer (marker-buffer m))
            (org-capture-put-target-region-and-position)
            (widen)
            (goto-char m)
            (set-marker m nil)))
        (`(file+regexp ,path ,regexp)
          (set-buffer (org-capture-target-buffer path))
          (org-capture-put-target-region-and-position)
          (widen)
          (goto-char (point-min))
          (if (not (re-search-forward regexp nil t))
              (error "No match for target regexp in file %s" path)
              (goto-char (if (org-capture-get :prepend)
                             (match-beginning 0)
                             (match-end 0)))
              (org-capture-put :exact-position (point))
              (setq target-entry-p
                    (and (derived-mode-p 'org-mode) (org-at-heading-p)))))
        (`(file+olp+datetree ,path . ,outline-path)
          (let ((m (if outline-path
                       (org-find-olp (cons (org-capture-expand-file path)
                                           outline-path))
                       (set-buffer (org-capture-target-buffer path))
                       (point-marker))))
            (set-buffer (marker-buffer m))
            (org-capture-put-target-region-and-position)
            (widen)
            (goto-char m)
            (set-marker m nil)
            (require 'org-datetree)
            (org-capture-put-target-region-and-position)
            (widen)
            ;; Make a date/week tree entry, with the current date (or
            ;; yesterday, if we are extending dates for a couple of hours)
            (funcall
             (if (eq (org-capture-get :tree-type) 'week)
                 #'org-datetree-find-iso-week-create
                 #'org-datetree-find-date-create)
             (calendar-gregorian-from-absolute
              (cond
                (org-overriding-default-time
                 ;; Use the overriding default time.
                 (time-to-days org-overriding-default-time))
                ((or (org-capture-get :time-prompt)
                     (equal current-prefix-arg 1))
                 ;; Prompt for date.
                 (let ((prompt-time (org-read-date
                                     nil t nil "Date for tree entry:"
                                     (current-time))))
                   (org-capture-put
                    :default-time
                    (cond ((and (or (not (boundp 'org-time-was-given))
                                    (not org-time-was-given))
                                (not (= (time-to-days prompt-time) (org-today))))
                           ;; Use 00:00 when no time is given for another
                           ;; date than today?
                           (apply #'encode-time
                                  (append '(0 0 0)
                                          (cl-cdddr (decode-time prompt-time)))))
                          ((string-match "\\([^ ]+\\)--?[^ ]+[ ]+\\(.*\\)"
                                         org-read-date-final-answer)
                           ;; Replace any time range by its start.
                           (apply #'encode-time
                                  (org-read-date-analyze
                                   (replace-match "\\1 \\2" nil nil
                                                  org-read-date-final-answer)
                                   prompt-time (decode-time prompt-time))))
                          (t prompt-time)))
                   (time-to-days prompt-time)))
                (t
                 ;; Current date, possibly corrected for late night
                 ;; workers.
                 (org-today))))
             ;; the following is the keep-restriction argument for
             ;; org-datetree-find-date-create
             (if outline-path 'subtree-at-point))))
        (`(file+function ,path ,function)
          (set-buffer (org-capture-target-buffer path))
          (org-capture-put-target-region-and-position)
          (widen)
          (funcall function)
          (org-capture-put :exact-position (point))
          (setq target-entry-p
                (and (derived-mode-p 'org-mode) (org-at-heading-p))))
        (`(function ,fun)
          (funcall fun)
          (org-capture-put :exact-position (point))
          (setq target-entry-p
                (and (derived-mode-p 'org-mode) (org-at-heading-p))))
        (`(clock)
          (if (and (markerp org-clock-hd-marker)
                   (marker-buffer org-clock-hd-marker))
              (progn (set-buffer (marker-buffer org-clock-hd-marker))
                     (org-capture-put-target-region-and-position)
                     (widen)
                     (goto-char org-clock-hd-marker))
              (error "No running clock that could be used as capture target")))
        (`(marker ,hd-mark)
          (if (and (markerp hd-marker)
                   (marker-buffer hd-marker))
              (progn (set-buffer (marker-buffer hd-marker))
                     (org-capture-put-target-region-and-position)
                     (widen)
                     (goto-char hd-marker))
              (error "No running clock that could be used as capture target")))
        (target (error "Invalid capture target specification: %S" target)))

      (org-capture-put :buffer (current-buffer)
                       :pos (point)
                       :target-entry-p target-entry-p
                       :decrypted
                       (and (featurep 'org-crypt)
                            (org-at-encrypted-entry-p)
                            (save-excursion
                              (org-decrypt-entry)
                              (and (org-back-to-heading t) (point))))))))
;; set target improved:1 ends here
