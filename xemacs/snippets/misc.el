

(defun org-copy-block ()
  (interactive)
  (org-narrow-to-block)
  (let (())
    (kill-new (buffer-substring (point-min) (point-max))))
  (widen))







(setq ag-ignore-list '("doc"))
