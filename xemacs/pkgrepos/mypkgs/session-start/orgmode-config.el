
(deh-require-maybe org
    ;; The following lines are always needed.  Choose your own keys.
  (defvar org2rem-create-remind-file nil "Create remind file on when saving org file.")
  (setq org-remind-escape-percentage nil
        org-remind-include-todo t
        org-log-done '(stat)        ;use for prgress logging.
        org-remind-suppress-last-newline nil)

  ;; #+SEQ_TODO: TODO ORDERED INVOICE PAYED RECEIVED SENT
  ;; #+STARTUP: lognotestate
  ;;;; from http://www.djcbsoftware.nl/dot-emacs.html
  ;; (add-hook 'org-mode-hook
  ;;           (lambda() (add-hook 'before-save-hook 'org-agenda-to-appt t t)))
      ;;;; from http://www.djcbsoftware.nl/dot-emacs.html
  (deh-require-maybe org2rem
    ;; (add-hook 'org-mode-hook
    ;;           (lambda()
    ;;             (add-hook 'after-save-hook 'org2rem-all-agenda-files t t)))
    (add-hook 'org-mode-hook
              (lambda()
                (if org2rem-create-remind-file
                    (add-hook 'after-save-hook 'org2rem-this-file t t)))))

  (add-hook 'org-mode-hook
            (lambda()
              (add-hook 'after-save-hook 'org-agenda-to-appt t t)))

      (add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
      ;; see key binding in binding.el

      (defun gtd ()
        (interactive)
        (find-file "~/.Organize/emacs/org/office/plan.org" )
        ;;(org-show-todo-tree 4)
        )

      (setq org-agenda-custom-commands
            '(("P" "Project List"
               ((tags "PROJECT")))
              ("O" "Office"
               ((agenda)
                 (tags-todo "OFFICE")))
              ("W" "Weekly Plan"
               ((agenda)
                 (todo "TODO")
                 (tags "PROJECT")))
              ("H" "Home NA Lists"
               ((agenda)
                (tags-todo "HOME")
                (tags-todo "COMPUTER")))))

      (xrequire 'org-export-freemind-install)
      (defadvice org-agenda-to-appt (before wickedcool activate)
        "Clear the appt-time-msg-list."
        (setq appt-time-msg-list nil)))






(deh-require-maybe (and remember
                        org
                        planner
                        remember-planner
                        read-file-name
                        ;; remember-blosxom
                        ;; remember-experimental ;; will start mail at daemon startup time.
                        remember-autoloads
                        remember-diary
                        remember-planner
                        remember-bbdb
                        remember
                        ;; remember-bibl
                        ;; macs-wiki-journal
                        )

  ;;If you are, like me, missing the function org-remember-insinuate, try
  ;;the following
  ;; start
  ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html


  (setq org-default-notes-file (concat org-directory "/notes.org"))

  (defun sharad/org-remember-sys ()
    (cond
      ((string-match "spratap" (system-name)) 'office)
      (t 'myself)))

  (defvar org-template-files-revert nil "")

  (add-hook 'ad-remember-mode-after-hook
            (lambda ()
              ;;(dolist (f org-template-files-revert)
              (while org-template-files-revert
                (let ((f (pop org-template-files-revert)))
                  (if (find-buffer-visiting f)
                      (with-current-buffer (find-buffer-visiting f)
                        (setq buffer-read-only t
                              view-read-only t
                              view-mode t)))))))

  (defun org-template-set-file-writable (xfile)
    (if (consp xfile)
        (error "xfile %s not file" xfile))
    (let* ((buf (or (find-buffer-visiting xfile)
                    (find-file-noselect xfile))))

      (with-current-buffer buf
            (when buffer-read-only
              (setq buffer-read-only nil
                    view-read-only nil
                    view-mode nil)
              (add-to-list 'org-template-files-revert xfile)))
          xfile))

  (defvar org-remember-template-alist nil "org-remember-template-alist")


  (defun org-template-push (template &rest keys)
    (pushnew (cons key template)
             org-remember-template-alist
             :key 'car))

  ;; (get-tree '((a (b (c . d)))) 'a 'b 'c)

  (defun make-orgremember-tmpl-with-sys (key s )
    )


  (defun org-template-gen (s &optional org-parent-dir)
    (let ((org-parent-dir (or org-parent-dir "~/.Organize/emacs/org/")))
      `(("Current Task"
         ?k
         "* TODO %? %^g\n %i\n [%a]\n"
         (lambda ()
           (org-template-set-file-writable (expand-file-name "notes.org" (find-task-dir)))))
        ("Emacs"
         ?m
         "* TODO %? %^g\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "emacs.org"))
        ("Todo" ;; todos
         ?t
         "* TODO %? %^g\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "todo.org")
         "G T D")
        ("Journal" ;; any kind of note
         ?j
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "j o u r n a l")
        ("Plan" ;; any kind of note
         ?n
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "plan.org")
         "p l a n")
        ("Learn" ;; any kind of note
         ?l
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "learn.org")
         "Learn")
        ("Idea" ;; any kind of note
         ?i
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "idea.org")
         "Ideas")
        ("Book" ;; book descp
         ?b
         "\n* %^{Book Title} %t :READING: \n%[~/.Organize/emacs/remember/templates/book]\n [%a]\n"
         ,(concat org-parent-dir s "/" "journal.org")
         "Books")
        ("Private" ;; private note
         ?p
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "privnotes.org"))
        ("Remember" ;; private note
         ?r
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "remember.org"))
        ("SomeDay" ;; private note
         ?s
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "someday.org"))
        ("Waiting-For" ;; private note
         ?w
         "\n* %^{topic} %T \n%i%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "waiting4.org"))
        ("Contact" ;; contact
         ?c
         "\n* %^{Name} :CONTACT:\n%[~/.Organize/emacs/remember/templates/contact]\n %i\n [%a]\n"
         ,(concat org-parent-dir s "/" "contacts.org"))
        ("Receipt" ;; receipt
         ?e
         "** %^{BriefDesc} %U %^g\n%?\n [%a]\n"
         ,(concat org-parent-dir s "/" "finances.org")))))

  ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html
  ;; (defvar org-remember-templates nil "templates for org.")

  (setq org-remember-templates (org-template-gen (symbol-name (sharad/org-remember-sys))))


  (functionp
   (nth 3 (car org-remember-templates)))

  (defun th-org-remember-conkeror (url)
    (interactive "s")
    (org-remember nil ?t)
    (save-excursion
      (insert "\n\n  [[" url "]]"))
    (local-set-key (kbd "C-c C-c")
                   (lambda ()
                     (interactive)
                     (org-ctrl-c-ctrl-c)
                     (delete-frame nil t))))



;; End
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  )

(provide 'orgmode-config)

