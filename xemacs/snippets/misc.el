





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
  ;; it through error when frame closed
  ;; redefine and find who is responsible.


  (defvar elscreen-screen-update-hook nil)
  (defun elscreen-run-screen-update-hook ()
    (when elscreen-frame-confs
      (elscreen-notify-screen-modification-suppress
       (run-hooks 'elscreen-screen-update-hook)))
    (remove-hook 'post-command-hook 'elscreen-run-screen-update-hook)))


(defun region-length (posBegin posEnd)
  "Print number of words and chars in region."
  (interactive "r")
  (let ((len (- posEnd posBegin)))
   (kill-new (format "%d" len))
   (message "length %d" len)))



(set-variable 'ycmd-server-command '("python" "/usr/bin/ycmd"))

ycmd-server-command



(progn
  (string-match "\\(.+\\):\\([0-9]+\\)" "aaa:11")
  (match-string 2 "aaa:11"))

(defun test-file-find-line (file line)
  (interactive
   (let* ((current-word (current-word))
          (file-line (string-match "(.+):([0-9]+)" current-word))
          (file (match-string 1 current-word))
          (line (match-string 2 current-word)))
     (list file line)))
  (when (file-exists-p file)
    (find-file file)))
