

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







(setq ag-ignore-list '("doc"))
