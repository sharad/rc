

(defun org-copy-block ()
  (interactive)
  (org-narrow-to-block)
  (let ((min
         (save-excursion
           (goto-char (point-min))
           (next-line)
           (beginning-of-line)
           (point)))
        (max
         (save-excursion
           (goto-char (point-max))
           (previous-line)
           (end-of-line)
           (point))))
    (kill-new (buffer-substring min max)))
  (widen))



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
