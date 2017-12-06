

;; ;; from http://sachachua.com/notebook/wiki/EmacsTips.php#13
;; (setq erc-encoding-coding-alist (quote (("#lisp" . utf-8)
;;           ("#nihongo" . iso-2022-jp) ("#truelambda" . iso-latin-1)
;;           ("#bitlbee" . iso-latin-1))))

;; http://sachachua.com/notebook/wiki/EmacsTips.php#12
;; Handy way to prevent XEmacs from interpreting the rest of your .emacs.
;; (when (featurep 'xemacs) (top-level))




;; (defvar ido-enable-tramp-completion-abc t "asdf")

;; (setq ido-enable-tramp-completion nil)
;; (setq ido-enable-tramp-completion-abc nil)

;; (defun ido-is-tramp-root (&optional dir)
;;   (and ido-enable-tramp-completion
;;        (string-match "\\`/[^/]+[@:]\\'"
;; 		     (or dir ido-current-directory))))


;; (defun ido-set-current-directory (dir &optional subdir no-merge)
;;   ;; Set ido's current directory to DIR or DIR/SUBDIR
;;   (unless (and ido-enable-tramp-completion
;; 	       (string-match "\\`/[^/]*@\\'" dir))
;;     (setq dir (ido-final-slash dir t)))
;;   (setq ido-use-merged-list nil
;; 	ido-try-merged-list (not no-merge))
;;   (when subdir
;;     (setq dir (concat dir subdir))
;;     (unless (and ido-enable-tramp-completion
;; 		 (string-match "\\`/[^/]*@\\'" dir))
;;       (setq dir (ido-final-slash dir t))))
;;   (if (get-buffer ido-completion-buffer)
;;       (kill-buffer ido-completion-buffer))
;;   (cond
;;    ((equal dir ido-current-directory)
;;     nil)
;;    ((ido-is-unc-root dir)
;;     (ido-trace "unc" dir)
;;     (setq ido-current-directory dir)
;;     (setq ido-directory-nonreadable nil)
;;     (setq ido-directory-too-big nil)
;;     t)
;;    (t
;;     (ido-trace "cd" dir)
;;     (setq ido-current-directory dir)
;;     (if (get-buffer ido-completion-buffer)
;; 	(kill-buffer ido-completion-buffer))
;;     (setq ido-directory-nonreadable (ido-nonreadable-directory-p dir))
;;     (setq ido-directory-too-big (and (not ido-directory-nonreadable)
;; 				     (ido-directory-too-big-p dir)))
;;     t)))



;; Debugger entered--Lisp error: (wrong-type-argument number-or-marker-p ftp)
;;   ido-file-name-all-completions("/scp:s@localhost:/etc/acpi/")
;;   ido-make-file-list-1("/scp:s@localhost:/etc/acpi/")
;;   ido-make-file-list(nil)
;;   ido-read-internal(file "Find file: " ido-file-history nil confirm-after-completion nil)
;;   ido-file-internal(raise-frame)
;;   ido-find-file()
;;   call-interactively(ido-find-file nil nil)






;; ;; http://stackoverflow.com/questions/2001485/how-do-i-keep-emacs-server-running-when-the-current-window-is-closed-x-on-wind

;; (defvar bnb/really-kill-emacs nil)

;; (defadvice kill-emacs (around bnb/really-exit activate)
;;     "Only kill emacs if the variable is true"
;;     (if bnb/really-kill-emacs
;;         ad-do-it)
;;       (bnb/exit))

;; ;; The bnb/exit function just makes the frame invisible like what you have bound to C-x C-c.

;; ;; I then have an additional function to properly exit emacs if that is ever necessary. That will set the variable and call kill-emacs as follows.

;; (defun bnb/really-kill-emacs ()
;;     (interactive)
;;     (setq bnb/really-kill-emacs t)
;;     (kill-emacs))




(defadvice kill-emacs (around lotus-noexit activate)
    "Only kill emacs if the variable is true"
    ad-do-it
    ;; (message "Getting killed")
    )


(provide 'test-config)
