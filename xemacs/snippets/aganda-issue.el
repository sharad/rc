


;; Debugger entered--Lisp error: (error "Selecting deleted buffer")
;;   org-agenda-new-marker(1514)
;;   org-agenda-get-deadlines()
;;   org-agenda-get-day-entries("/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/personal/report.org" (9 8 2019) :deadline :scheduled :timestamp :sexp)
;;   apply(org-agenda-get-day-entries "/home/s/hell/Documents/CreatedContent/contents/virtual/org/default/tasks/personal/report.org" (9 8 2019) (:deadline :scheduled :timestamp :sexp))
;;   (spacemacs-buffer//make-org-items file (apply (quote org-agenda-get-day-entries) file date types))


(defun make-frame-fix-issue ()
  (interactive)
  (when org-agenda-buffer
    (unless (buffer-live-p org-agenda-buffer)
      (setq org-agenda-buffer nil))))




;; spacemacs-buffer//resize-on-hook
