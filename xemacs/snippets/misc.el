

(defun org-copy-block ()
  (interactive)
  (org-narrow-to-block)
  (let ((min
         (save-excursion
           (goto-char (point-min))
           (next-line))))
    (kill-new (buffer-substring (point-min) (point-max))))
  (widen))







(setq ag-ignore-list '("doc"))
