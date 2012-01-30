;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;; start

;; Setting up for OrgMode
;;Basically you should follow the instructions in the org-mode info
;;file. However this currently forgets that you also need something like
;;this in your .emacs:

(when (and (xrequire 'remember)
           (xrequire 'org)
           (xrequire 'planner)
           (xrequire 'remember-planner))
  ;;If you are, like me, missing the function org-remember-insinuate, try
  ;;the following
  ;; start
  ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html


  (setq org-default-notes-file (concat org-directory "notes.org"))


  (defun remember-sys ()
    (cond
      ((string-match "spratap" (system-name)) 'office)
      (t 'myself)))

  ;; (defun get-org-file (dirpath file &optional org-base-dir)
  ;;   (let ((org-parent-dir (or org-parent-dir "~/.Organize/emacs/org/")))
  ;;     (if )
  ;;     ))

  (defun org-template-gen (s &optional org-parent-dir)
    (let ((org-parent-dir (or org-parent-dir "~/.Organize/emacs/org/")))
      `(("Todo" ;; todos
         ?t
         "* TODO %? %^g\n %i\n"
         ,(concat org-parent-dir s "/" "todo.org")
         "G T D")
        ("Journal" ;; any kind of note
         ?j
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "j o u r n a l")
        ("Plan" ;; any kind of note
         ?n
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "plan.org")
         "p l a n")
        ("Learn" ;; any kind of note
         ?l
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "learn.org")
         "Learn")
        ("Idea" ;; any kind of note
         ?i
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "idea.org")
         "Ideas")
        ("Book" ;; book descp
         ?b
         "\n* %^{Book Title} %t :READING: \n%[~/.Organize/emacs/remember/templates/book]\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "Books")
        ("Private" ;; private note
         ?p
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "privnotes.org"))
        ("Remember" ;; private note
         ?r
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "remember.org"))
        ("SomeDay" ;; private note
         ?s
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "someday.org"))
        ("Waiting-For" ;; private note
         ?w
         "\n* %^{topic} %T \n%i%?\n"
         ,(concat org-parent-dir s "/" "waiting4.org"))
        ("Contact" ;; contact
         ?c
         "\n* %^{Name} :CONTACT:\n%[~/.Organize/emacs/remember/templates/contact]\n"
         ,(concat org-parent-dir s "/" "privnotes.org"))
        ("Receipt" ;; receipt
         ?e
         "** %^{BriefDesc} %U %^g\n%?"
         ,(concat org-parent-dir s "/" "finances.org")))))

  ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html
  (setq org-remember-templates (org-template-gen (symbol-name (remember-sys))))


  (defun th-org-remember-conkeror (url)
    (interactive "s")
    (org-remember nil ?t)
    (save-excursion
      (insert "\n\n  [[" url "]]"))
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (org-ctrl-c-ctrl-c)
                     (delete-frame nil t)))))


;; End
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(deh-section "My Remember"
  (defvar sharad/remember-functions-alist nil "")

  (setq sharad/remember-functions-alist
        `((planner .
                   ((annotation . ,planner-annotation-functions)
                    (handler    . (remember-planner-append))))
          (org .
               ((annotation . (org-remember-annotation))
                (handler    . (org-remember-handler))
                (hook       . (org-remember-apply-template))))))

  (defmacro cdr-assoc-cdr-assoc (key1 key2 alist)
    `(cdr (assoc ,key2 (cdr (assoc ,key1 ,alist)))))

  (defun sharad/remember-org ()
    (interactive)
    (sharad/remember 'org))

  (defun sharad/remember-planner ()
    (interactive)
    (sharad/remember 'planner))


  (defun run-list-until-success (flist)
    (some 'funcall flist))


  (defun sharad/remember (organizer &optional initial)
    "Remember an arbitrary piece of data.
With a prefix, uses the region as INITIAL."
    (interactive
     (list (when current-prefix-arg
             (buffer-substring (point) (mark)))))
    (window-configuration-to-register remember-register)
    (let* ((xremember-annotation-functions
            (cdr-assoc-cdr-assoc organizer 'annotation sharad/remember-functions-alist))
           (annotation
            (if remember-run-all-annotation-functions-flag
                (mapconcat 'identity
                           (delq nil (mapcar 'funcall xremember-annotation-functions))
                           "\n")
                (run-list-until-success xremember-annotation-functions)))
           (buf (get-buffer-create remember-buffer)))
      (mapc 'funcall remember-before-remember-hook)
      (switch-to-buffer-other-window buf)
      (sharad/remember-mode organizer)
      (when (= (point-max) (point-min))
        (when initial (insert initial))
        (setq remember-annotation annotation)
        (when remember-initial-contents (insert remember-initial-contents))
        (when (and (stringp annotation)
                   (not (equal annotation "")))
          (insert "\n\n" annotation))
        (setq remember-initial-contents nil)
        (goto-char (point-min)))
      (message "Use C-c C-c to remember the data.")))


  (defun sharad/remember-region (organizer &optional beg end)
    "Remember the data from BEG to END.
If called from within the remember buffer, BEG and END are ignored,
and the entire buffer will be remembered.

This function is meant to be called from the *Remember* buffer.
If you want to remember a region, supply a universal prefix to
`remember' instead. For example: C-u M-x remember."
    ;; Sacha: I have no idea where remember.el gets this context information, but
    ;; you can just use remember-annotation-functions.
    (interactive)
    (let ((xremember-handler-functions
           (cdr-assoc-cdr-assoc organizer 'handler sharad/remember-functions-alist))
          (b (or beg (min (point) (or (mark) (point-min)))))
          (e (or end (max (point) (or (mark) (point-max))))))
      (save-restriction
        (narrow-to-region b e)
        (if remember-all-handler-functions
            (mapc 'funcall xremember-handler-functions)
            (run-list-until-success xremember-handler-functions))
        (remember-destroy))))

  (defvar remember-mode-map ()
    "Keymap used in Remember mode.")
  (when (or t (not remember-mode-map))
    (setq remember-mode-map (make-sparse-keymap))
    (define-key remember-mode-map "\C-x\C-s" 'sharad/remember-buffer)
    (define-key remember-mode-map "\C-c\C-c" 'sharad/remember-buffer)
    (define-key remember-mode-map "\C-c\C-k" 'remember-destroy))

  (defun sharad/remember-mode (organizer)
    "Major mode for output from \\[remember].
\\<remember-mode-map>This buffer is used to collect data that you want
remember.  Just hit \\[remember-region] when you're done entering, and
it will go ahead and file the data for latter retrieval, and possible
indexing.  \\{remember-mode-map}"
    (interactive)
    (kill-all-local-variables)
    (indented-text-mode)
    (use-local-map remember-mode-map)
    (setq major-mode 'remember-mode
          mode-name "Remember")
    (set (make-local-variable 'organizer-for-remember-region) organizer)
    (mapc 'funcall (cdr-assoc-cdr-assoc organizer 'hook sharad/remember-functions-alist)))

;;;###autoload
  (defun sharad/remember-clipboard ()
    "Remember the contents of the current clipboard.
Most useful for remembering things from Netscape or other X Windows
application."
    (interactive)
    (sharad/remember (current-kill 0)))

;;;###autoload
  (defun sharad/remember-buffer ()
    "Remember the contents of the current buffer."
    (interactive)
    (let ((organizer-for-remember-region
           (or (if (boundp 'organizer-for-remember-region)
                   organizer-for-remember-region)
               (if (or (equal major-mode 'org-mode)
                       org-remember-mode) 'org))))
      (sharad/remember-region organizer-for-remember-region (point-min) (point-max)))))


;; org-remember.el
(defun org-remember-finalize ()
  "Finalize the remember process."
  (interactive)
  (unless org-remember-mode
    (error "This does not seem to be a remember buffer for Org-mode"))
  (run-hooks 'org-remember-before-finalize-hook)
  (unless (fboundp 'remember-finalize)
    (defalias 'remember-finalize 'sharad/remember-buffer))
  (when (and org-clock-marker
	     (equal (marker-buffer org-clock-marker) (current-buffer)))
    ;; the clock is running in this buffer.
    (when (and (equal (marker-buffer org-clock-marker) (current-buffer))
	       (or (eq org-remember-clock-out-on-exit t)
		   (and org-remember-clock-out-on-exit
			(y-or-n-p "The clock is running in this buffer.  Clock out now? "))))
      (let (org-log-note-clock-out) (org-clock-out))))
  (when buffer-file-name
    (do-auto-save))
  (remember-finalize))


;; (let ((map (make-sparse-keymap)))

;;   ;; Now for the stuff that has direct keybindings
;;   ;;
;;   (define-key map "\C-crp" 'sharad/remember-planner)
;;   (define-key map "\C-c r o" 'sharad/remember-org)
;;   map)


(user-provide 'remember)
