;;
;; from: http://1010.co.uk/emacs.html
;; ra-index -v ~/experiment/RA-indexes/svn ~/svn_test/trunk

;; in .emacs:

;; (define-prefix-command 'remem-command-map)
;; (global-set-key (kbd "C-cx") 'remem-command-map)

;; ;; Keys We want to start with before running the RA
;; (define-key remem-command-map (kbd "t") 'remem-toggle)
;; (define-key remem-command-map (kbd "h") 'remem-create-help-page)



(deh-require-maybe remem

  ;; (define-key perly-sense-map (kbd "C-d") 'perly-sense-smart-docs-at-point)
  ;; (define-key perly-sense-map (kbd "C-g") 'perly-sense-smart-go-to-at-point)

  (setq remem-logfile (auto-config-file "remem/remem-log-file")
        remem-prog-dir "/usr/bin"
        remem-database-dir "~/.RA-indexes"
        remem-scopes-list '(("doc" 6 5 500)
                            ("mail" 6 5 500)
                            ("office" 6 5 500))
  ;;(setq remem-terminal-mode t)
        remem-load-original-suggestion  t)


  ;; C-c r t - toggle

  ;; C-c r [number] see numbered suggestion

  ;; C-c r

  ;;{{ Faces
  (set-face-foreground 'remem-odd  "White")
  ;;}}

  ;;from: http://1010.co.uk/tech_notes.html
  ;;
  (defun other-buffer-to-kill ()
    "other buffer - most prob remembrance - to kill ring"
    (interactive)
    (save-excursion
      (other-window 1)
      (let* ((beg (point-min))
             (end (point-max)))
        (kill-ring-save beg end))
      (other-window 1)))
  ;; complete remem buffer to kill ring code:
  (defun remem-to-kill ()
    (with-current-buffer "*remem-display*"
      (kill-ring-save (point-min) (point-max))))

  (defun splice-buffer ()
    (save-excursion
      (setf (point) (point-min))
      (insert "\n<example>\n")
      (setf (point) (point-max))
      (insert "\n</example>")))

  (defun remem-yank ()
    (interactive)
    (let ((target (current-buffer)))
      (with-temp-buffer
        (let ((source (current-buffer)))
          (yank)
          (splice-buffer)
          (with-current-buffer target
            (insert-buffer-substring source))))))

  (defun remem-append ()
    "remem-display buffer is appended to buffer with correct example tags"
    (interactive)
    (save-excursion
      (remem-to-kill)
      (goto-char (point-max))
      (remem-yank))))
;;
;;end

(provide 'remem-config)
