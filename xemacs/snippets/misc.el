
(defun org-copy-block (arg)
  (interactive "P")
  (save-excursion
    (save-restriction
      (org-narrow-to-block)
      (kill-new
       (if arg
           (buffer-substring (point-min) (point-max))
         (let ((min
                (progn
                  (goto-char (point-min))
                  (next-line)
                  (beginning-of-line)
                  (point)))
               (max
                (progn
                  (goto-char (point-max))
                  (previous-line)
                  (end-of-line)
                  (point))))
           (buffer-substring min max)))))))






(setq ag-ignore-list '("doc"))


(progn
  ;; Fix it

(defvar elscreen-screen-update-hook nil)
(defun elscreen-run-screen-update-hook ()
  (when elscreen-frame-confs
    (elscreen-notify-screen-modification-suppress
     (run-hooks 'elscreen-screen-update-hook)))
  (remove-hook 'post-command-hook 'elscreen-run-screen-update-hook)))
