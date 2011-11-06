;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;; start

;; Setting up for OrgMode
;;Basically you should follow the instructions in the org-mode info
;;file. However this currently forgets that you also need something like
;;this in your .emacs:

(if (and (xrequire 'remember) (xrequire 'org)
          (xrequire 'planner) (xrequire 'remember-planner))
    ;;If you are, like me, missing the function org-remember-insinuate, try
    ;;the following
    ( progn
      ;; start
      ;; from: http://members.optusnet.com.au/~charles57/GTD/remember.html

      (defun oremp (arg)
        "Display the value of the raw prefix arg."
        (interactive "P")
        (if arg
            (progn
              (setq org-remember-templates
                    (cond
                     ((= (prefix-numeric-value arg) 1) org-remtpl-office)
                     ((= (prefix-numeric-value arg) 2) org-remtpl-myself)
                     ((= (prefix-numeric-value arg) 4) org-remtpl-default)))

              (if (functionp 'org-remember-insinuate)
                (org-remember-insinuate)
              (setq remember-annotation-functions '(org-remember-annotation)
                    remember-handler-functions '(org-remember-handler))
              (add-hook 'remember-mode-hook 'org-remember-apply-template))
              (remember))
         ;;      (define-key global-map "\C-cr" 'org-remember)
          (progn
            (remove-hook 'remember-mode-hook 'org-remember-apply-template)
            (setq remember-handler-functions '(remember-planner-append)
                  remember-annotation-functions planner-annotation-functions)
            (remember))))

      (define-key global-map "\C-cr" 'oremp)

      ;; ;; planner only
      ;; (remove-hook 'remember-mode-hook 'org-remember-apply-template)
      ;; (setq remember-handler-functions '(remember-planner-append)
      ;;       remember-annotation-functions planner-annotation-functions)
      ;; (define-key global-map "\C-cr" 'remember)


;;;       (if (functionp 'org-remember-insinuate)
;;;           (org-remember-insinuate)
;;;         (setq remember-annotation-functions '(org-remember-annotation))
;;;         (setq remember-handler-functions '(org-remember-handler))
;;;         (add-hook 'remember-mode-hook 'org-remember-apply-template))
;;; ;;      (define-key global-map "\C-cr" 'org-remember)


      (setq org-default-notes-file (concat org-directory "notes.org"))
      ;; (setq org-remember-templates

      (setq org-defult-remtpl-directory "~/.Organize/emacs/org/")

;;;       (mapcar
;;;        (lambda (s)
;;;          (set (make-symbol (concat "xx" s)) s)) '("a"))

;;; (set (intern (concat "aq")) (list (list (concat "ddd"))))
;;; aq

      (mapc (lambda (s)
              (set (intern (concat "org-remtpl-" s))
                   (list
                    (list "Todo" ;; todos
                          ?t
                          "* TODO %? %^g\n %i\n"
                          (concat org-defult-remtpl-directory s "/" "todo.org")
                          "G T D")
                    (list "Journal" ;; any kind of note
                          ?j
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "journal.org")
                          "j o u r n a l")
                    (list "Plan" ;; any kind of note
                          ?n
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "plan.org")
                          "p l a n")
                    (list "Learn" ;; any kind of note
                          ?l
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "learn.org")
                          "Learn")
                    (list "Idea" ;; any kind of note
                          ?i
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "idea.org")
                          "Ideas")
                    (list "Book" ;; book descp
                          ?b
                          "\n* %^{Book Title} %t :READING: \n%[~/.Organize/emacs/remember/templates/book]\n"
                          (concat org-defult-remtpl-directory s "/" "journal.org")
                          "Books")
                    (list "Private" ;; private note
                          ?p
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "privnotes.org"))
                    (list "Remember" ;; private note
                          ?r
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "remember.org"))
                    (list "SomeDay" ;; private note
                          ?s
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "someday.org"))
                    (list "Waiting-For" ;; private note
                          ?w
                          "\n* %^{topic} %T \n%i%?\n"
                          (concat org-defult-remtpl-directory s "/" "waiting4.org"))
                    (list "Contact" ;; contact
                          ?c
                          "\n* %^{Name} :CONTACT:\n%[~/.Organize/emacs/remember/templates/contact]\n"
                          (concat org-defult-remtpl-directory s "/" "privnotes.org"))
                    (list "Receipt" ;; receipt
                          ?e
                          "** %^{BriefDesc} %U %^g\n%?"
                          (concat org-defult-remtpl-directory s "/" "finances.org"))
                    )
                   )) '("office" "myself"))

      (cond
       ((string-match "pcz-c-sharad" (system-name))
        (and org-remtpl-office
             (setq org-remtpl-default org-remtpl-office
                   org-directory "~/.Organize/emacs/org/office")))
       (t (and org-remtpl-myself
               (setq org-remtpl-default org-remtpl-myself
                     org-directory "~/.Organize/emacs/org/myself"))))

;;;       (setq org-remember-templates
;;;             (lambda ()
;;;                     (org-office)
;;;                     )
;;;             )

      ;; (setq org-remember-templates
;;             '(
;;               (?t ;;"Todo"
;;                "* TODO %^{what ?: }\n%i\n%?\n%a"
;;                "~/.Organize/gtd/newgtd.org"
;;                "Office")
;;               (?j ;;"Journal"
;;                "\n* %^{topic} %T \n%i%?\n%?\n%a"
;;                "~/.Organize/gtd/journal.org")
;;               (?b ;;"Book"
;;                "\n* %^{Book Title} %t :READING: \n%[~/.Organize/templates/book]\n%a"
;;                "~/.Organize/gtd/journal.org")
;;               (?p ;;"Private"
;;                "\n* %^{topic} %T \n%i%?\n%?\n%a"
;;                "~/.Organize/gtd/privnotes.org")
;;               (?c ;;"Contact"
;;                "\n* %^{Name} :CONTACT:\n%[~/.Organize/templates/contact]\n%a"
;;                "~/.Organize/gtd/privnotes.org")
;;               (?r ;;"Receipt"
;;                "** %^{BriefDesc} %U %^g\n%?"
;;                "~/.Organize/gtd/finances.org")
;;               (?a ;;"Daily Review"
;;                "** %t :COACH: \n%[~/.Organize/templates/dailyreview]\n%a"
;;                "~/GTD/journal.org")
;;               )
;;             )

      ;; end: from: http://members.optusnet.com.au/~charles57/GTD/remember.html

     ;;Setting up for PlannerMode
     ;;Add this to your .emacs:

;;;       ( if (and (xrequire 'planner) (xrequire 'remember-planner) nil)
;;;           ( progn
;;;             (setq remember-handler-functions '(remember-planner-append))
;;;             (setq remember-annotation-functions planner-annotation-functions)
;;;             )
;;;         )
      )
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
  )


;; End
;; from http://www.emacswiki.org/emacs/RememberMode#toc7
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(user-provide 'remember)
