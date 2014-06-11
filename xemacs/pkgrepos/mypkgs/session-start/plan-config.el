;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/PlannerModeQuickStart
;; start

;; Setting up Planner
;; Add the files to your load-path. Change these paths as needed.



(deh-require-maybe planner
  (setq planner-sections
        '((tasks . "Tasks")
          (notes . "Notes")
          (diary . "Diary")
          (env   . "Environment")))

  ;; Setting up Planner
  ;; Add the files to your load-path. Change these paths as needed.
  (setq planner-directory "~/.Organize/emacs/plan/Plans")
  (setq planner-project "WikiPlanner"
        ;; faced muse-get-keyword void-variable planner-mode
        planner-mode #'planner-mode)
  (add-to-list
   'muse-project-alist
   '("WikiPlanner"
     ("~/.Organize/emacs/plan/Plans"   ;; Or wherever you want your planner files to be
      :default "index"
      ;; :major-mode #'planner-mode
      :major-mode planner-mode
      ;; :final muse-project-publish-file
      :visit-link planner-visit-link)
     ;; This next part is for specifying where Planner pages
     ;; should be published and what Muse publishing style to
     ;; use. In this example, we will use the XHTML publishing
     ;; style.
     (:base "planner-xhtml"
            ;; where files are published to
            ;; (the value of 'planner-publishing-directory', if
            ;; if you have configuration for an older version
            ;; of Planner)
            :path "~/public_html/Plans")))

  ;; (global-set-key (kbd "C-c r") 'remember)

  ;; Planning Projects
  ;;
  ;; The TaskPool is a default Plan Page, on which tasks are created by
  ;; default. You can create other Plan Pages to group or organize a
  ;; collection of related tasks into a project.
  ;;
  ;; All the pages in your planner can contain links of the form WikiName,
  ;; [[destination]], or [[destination][description]]. which will link to
  ;; the alternate plan pages if they exist.
  ;;
  ;; You can enable easy creation of WikiName type links to Plan Pages in
  ;; tasks with these incantations in your .elisp file:

  (deh-require-maybe muse-wiki    ;;; Allow wiki-links
      (setq muse-wiki-allow-nonexistent-wikiword t))
  (setq
   calendar-mark-diary-entries-flag t
   ;;   mark-diary-entries-in-calendar t
   diary-file "~/.Organize/emacs/diary/diary")

  ;;(global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)

  (deh-require-maybe planner-gnus
      (planner-gnus-insinuate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (deh-require-maybe planner-id
      (setq
       planner-id-add-task-id-flag t
       planner-id-update-automatically t ;; default
       planner-id-tracking-file "~/.Organize/emacs/plan/planner-ids"))
  (deh-require-maybe planner-cyclic
      (setq
       planner-cyclic-diary-file "~/.Organize/emacs/plan/planner-cyclic-diary-file"
       planner-cyclic-diary-nag t))
  (xrequire 'planner-deadline)

  (deh-require-maybe planner-multi
    (load-library "planner-multi")
    (setq
     ;; planner-multi-copy-tasks-to-page "TaskPool"
     planner-multi-separator ","
     planner-multi-copy-tasks-to-page "[[TasksByProject][p]],[[TasksByContext][c]]")

    (assert (eq (symbol-function 'planner-copy-or-move-task)
                'planner-multi-copy-or-move-task))
    (assert (not
             (eq (symbol-function 'planner-copy-or-move-task)
                 'planner-copy-or-move-task-basic)))

    ;; (assert (eq (symbol-function 'planner-replan-task)
    ;;             'planner-multi-replan-task))
    (assert (not
             (eq (symbol-function 'planner-replan-task)
                 'planner-replan-task-basic)))

    (assert (eq (symbol-function 'planner-update-task)
                'planner-multi-update-task))
    (assert (not
             (eq (symbol-function 'planner-update-task)
                 'planner-update-task-basic)))


    (assert (eq (symbol-function 'planner-tasks-equal-p)
                'planner-multi-tasks-equal-p))

    (assert (eq (symbol-function 'planner-edit-task-description)
                'planner-multi-edit-task-description))
    (assert (not
             (eq (symbol-function 'planner-edit-task-description)
                 'planner-edit-task-description-basic)))

    (assert (eq (symbol-function 'planner-task-date)
                'planner-multi-task-date))

    (assert (eq (symbol-function 'planner-link-as-list)
                'planner-multi-link-as-list))

    (defalias 'planner-task-date 'planner-multi-task-date)
    (defalias 'planner-task-link-as-list 'planner-multi-task-link-as-list)
    (defalias 'planner-copy-or-move-task 'planner-multi-copy-or-move-task)
    (defalias 'planner-replan-task 'planner-multi-replan-task)
    (defalias 'planner-update-task 'planner-multi-update-task)
    (defalias 'planner-tasks-equal-p 'planner-multi-tasks-equal-p)
    (defalias 'planner-edit-task-description 'planner-multi-edit-task-description)
    (defalias 'planner-multi-xref-note 'planner-multi-note-xref)
    (defalias 'planner-multi-delete-note 'planner-multi-note-delete)
    (defalias 'planner-multi-delete-note-this-page 'planner-multi-note-delete-this-page)
    (defalias 'planner-multi-xref-task 'planner-multi-task-xref)
    (defalias 'planner-multi-delete-task-this-page 'planner-multi-task-delete-this-page)

    (progn ;;modification
      (defun planner-make-link (link &optional name single)
        "Return a Wiki link to LINK with NAME as the text.
If SINGLE is non-nil, treat it as a single link.
If LINK is already a valid link, replace it's description
by NAME"
        (cond ((or (null link) (string= link ""))
               "")
              ((string-match muse-explicit-link-regexp link)
               (muse-make-link (match-string 1 link) (or name (match-string 2 link))))
              (t
               (muse-make-link link name))))


      (defun planner-multi-task-string (info page-name links &optional new-pages)
        "Return task line for INFO on PAGE-NAME with LINKS, a list of pages to link to.
If non-nil, PAGES should be a list of the `planner-link-base's of LINKS."
        ;; Set up the new links list for easy testing
        ;; (setq new-pages (mapcar 'planner-link-base links))
        (setq new-pages (mapcar 'identity links))
        (cond
          ;; If this is a no-link task
          ((and (= (length new-pages) 1)
                (string= (car new-pages) page-name))
           (planner-format-task info nil nil nil nil "" ""))
          ;; If this is a standard singly-linked task (date, plan)
          ((and (= (length new-pages) 2)
                (string-match planner-date-regexp (car new-pages))
                (not (string-match planner-date-regexp (cadr new-pages))))
           (planner-format-task info nil nil nil nil
                                (planner-make-link
                                 (if (string-match planner-date-regexp page-name)
                                     (cadr new-pages)
                                     (car new-pages)))))
          ;; If this is a standard singly-linked task (plan, date)
          ((and (= (length new-pages) 2)
                (not (string-match planner-date-regexp (car new-pages)))
                (string-match planner-date-regexp (cadr new-pages)))
           (planner-format-task info nil nil nil nil
                                (planner-make-link
                                 (if (string-match planner-date-regexp page-name)
                                     (car new-pages)
                                     (cadr new-pages)))))
          ;; Multilink
          (t (planner-format-task info nil nil nil nil
                                  (planner-make-link new-pages)))))))

  (deh-require-maybe bbdb-com          ;checking it as it fail emacs to move ahead.
      (xrequire 'planner-bbdb))

  (xrequire 'planner-erc)
  ;; (xrequire 'planner-w3m)
  (xrequire 'planner-bibtex)
  (xrequire 'planner-id)
  (xrequire 'planner-gnus)
  (xrequire 'planner-rank)
  (xrequire 'planner-trunk)
  (when (featurep 'xtla) (xrequire 'planner-xtla))
  (xrequire 'planner-accomplishments) ; M-x planner-accomplishments-show after M-x plan
  (xrequire 'planner-tasks-overview) ; M-x planner-tasks-overview

  ;;(setq planner-renumber-tasks-automatically t)
  (setq
   planner-task-dates-favor-future-p t
   planner-carry-tasks-forward 4 ;; 'scan-all ;; t ;; this value badly affect performance and speed or operation, so be careful.
   planner-use-task-numbers t
   planner-sort-tasks-automatically t
   planner-align-tasks-automatically t
   planner-renumber-tasks-automatically t
   planner-renumber-notes-automatically t
   planner-align-notes-automatically t
   planner-annotation-strip-directory t
   planner-annotation-use-relative-file t
   planner-sort-tasks-key-function 'planner-sort-tasks-default-key
;;;      possible vaules...
;;;      `planner-sort-tasks-default-key', `planner-sort-tasks-basic',
;;;      `planner-sort-tasks-by-date', and `planner-sort-tasks-by-link'.
;;;      `planner-sort-tasks-by-rank', `planner-sort-tasks-by-importance', and
;;;      `planner-sort-tasks-by-urgency'.
   ;; planner-add-task-at-end-flag nil
   )


  (setq
   planner-day-page-template
   (concat
    "* Tasks\n\n\n"
    "* Schedule\n\n\n"
    "* Notes\n\n\n"
    ))

  (when (and
         (xrequire 'planner-diary)
         (file-readable-p diary-file))
    (xrequire 'diary-lib)
    (xrequire 'diary)
    (xrequire 'holidays)
    (xrequire 'cal-menu)
    (cond
     ((functionp 'diary-sort-entries)
       (add-hook 'diary-display-hook 'diary-sort-entries))
      ((functionp 'sort-diary-entries)
       (add-hook 'diary-display-hook 'sort-diary-entries)))

    ;;check existance
    (when (and
           (xrequire 'cal-desk-calendar)
           (functionp 'fancy-schedule-display-desk-calendar))
      (add-hook 'diary-display-hook 'fancy-schedule-display-desk-calendar t)
      (add-hook 'diary-display-hook 'diary-fancy-display))

    (planner-diary-insinuate)
    (planner-insinuate-calendar)
    (planner-calendar-insinuate)

    (setq
     planner-diary-use-diary t
     planner-diary-number-of-days 5
     planner-diary-create-section-flag t
     planner-day-page-template
     (concat
      planner-day-page-template
      ;; "* Diary Appointments\n\n"
      "* Diary\n\n\n"
      "* Public Diary\n\n"
      "* Private Diary\n\n"
      "* Cal-Desk\n\n")))

  ;; "* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>\n"


  (deh-require-maybe planner-appt
    (planner-appt-use-schedule)
    (planner-appt-insinuate)
    (planner-appt-schedule-cyclic-insinuate)
    (planner-appt-calendar-insinuate)
    (setq
     planner-appt-update-appts-on-save-flag t))

  (deh-require-maybe (and timeclock planner-timeclock planner-timeclock-summary)
    (make-directory (expand-file-name "autoconfig/timeclock/" user-emacs-directory) t)
    (setq timeclock-file (convert-standard-filename (expand-file-name "autoconfig/timeclock/timelog" user-emacs-directory)))
    (planner-timeclock-summary-insinuate))

  ;; (deh-require-maybe planner-ledger
  ;;   (add-hook 'planner-goto-hook 'planner-ledger-insert-maybe))

  (deh-require-maybe planner-accomplishments
    (setq
     planner-day-page-template
     (concat
      planner-day-page-template
      "* Accomplishments\n\n\n"))
    (planner-accomplishments-insinuate))

  ;;{{
  ;; (deh-require-maybe planner-registry
  ;;     (planner-registry-insinuate))
  ;; planner-registry-insinuate some time create problem in daemon startup.
  ;; as it may start to ask input.

  (make-directory (expand-file-name "autoconfig/planner/" user-emacs-directory) t)
  (setq planner-registry-file (expand-file-name "autoconfig/planner/planner-registry.el" user-emacs-directory))

  (when nil ;; find out somewhere else (plan) is already started.
   ;; (add-hook '*sharad/after-init-hook*

   (add-hook 'sharad/enable-startup-inperrupting-feature-hook ;; '*sharad/after-init-hook*
             '(lambda ()
               (deh-require-maybe planner-registry
                 (setq planner-registry-file "~/.emacs.d/autoconfig/planner/planner-registry.el")
                 (save-excursion
                   (save-window-excursion
                     (plan 2)
                     (planner-registry-insinuate)))))))

  ;;}}
  (xrequire 'planner-zoom)
  (xrequire 'planner-lisp)

  (deh-require-maybe planner-report ;; for using      M-x planner-report-generate
    (setq planner-report-unfinished-offset 2))

  (deh-require-maybe planner-publish
      ;; To automatically publish files when you save them,
      ;; add the following code to your `~/.emacs' (or `_emacs'):

      ;; (eval-after-load "muse-mode"
      ;;  (add-hook 'after-save-hook
      ;;            #'(lambda ()
      ;;                (when (planner-derived-mode-p 'muse-mode)
      ;;                  (muse-project-publish nil)))
      ;;            nil t))
      nil
    )
  (xrequire 'planner-calendar)
  (xrequire 'planner-authz)
;;;       (deh-require-maybe planner-ical
;;;           (planner-ical-export-file
;;;            (planner-today)
;;;            (expand-file-name
;;;             "tasks.ics"
;;;             (muse-style-element :path (car (cddr (muse-project planner-project)))))))

  (xrequire 'planner-experimental)
  (xrequire 'planner-unix-mail)
  (deh-require-maybe planner-gnus
      (planner-gnus-insinuate))
  ;; (xrequire 'planner-vm)
  ;; (deh-require-maybe planner-wl
  ;;    (planner-wl-insinuate))
  ;; (xrequire 'planner-mhe)
  (xrequire 'planner-rmail)


  ;; alternate 2nd method to insert the diary contents in the file instead of calling planner-diary-entries-here
  ;;(setq planner-day-page-template "* Tasks\n\n\n* Diary\n\n\n* Notes\n\n\n")
  ;;(setq planner-diary-use-diary t)
  ;;(planner-diary-insinuate)

  (define-key planner-mode-map [(shift return)] 'planner-edit-task-description)
  (define-key planner-mode-map [f2] 'xsteve-planner-save)
  (planner-install-extra-task-keybindings)
  (planner-install-extra-note-keybindings)
  (planner-install-extra-context-keybindings)
  (defun xsteve-planner-save ()
    (interactive)
    (planner-fix-tasks)
    (planner-save-buffers))
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; (plan)

  ;; http://lists.gnu.org/archive/html/emacs-wiki-discuss/2005-08/msg00168.html
  (defun planner-create-note-this-page ()
    (interactive)
    (planner-create-note (planner-page-name)))

  (define-key planner-mode-map (kbd "C-c n") 'planner-create-note-this-page)


  (deh-section "Day Pages Cleanup"

    (require 'string) ;; from elib package

    (defun planner-clean-page-section (section empty-string page-string)
      (or
       (string-replace-match (concat "* " section "\n\n" empty-string) page-string (concat "* " section "\n") t)
       page-string))

    (defun planner-clean-page-sections (page-string)
      (setq page-string (planner-clean-page-section "Schedule" "[\r\n\t ]+@13:00 | 14:00 | Lunch\n\n" page-string))
      (setq page-string (planner-clean-page-section "Diary" "No entries\n\n" page-string))
      (setq page-string (planner-clean-page-section "Public Diary" "No entries\n\n" page-string))
      (setq page-string (planner-clean-page-section "Private Diary" "No entries\n\n" page-string))
      (setq page-string (planner-clean-page-section "Cal-Desk" "No entries\n\n" page-string))
      (setq page-string (planner-clean-page-section "Accomplishments"
                                                    "Link[\r\n\t ]+| Total\nTotal[\r\n\t ]+|[\r\n\t ]+0\n" page-string))
      page-string)

    ;; planner-day-page-template
    ;; planner-diary-appts-string
    (defun planner-page-default-p (&optional buffer)
      "Return t if this plan page can be safely deleted.
If the contents of this plan page are the same as the value of
`planner-day-page-template' or the plan page is empty, then no
information has been added and the page can safely be removed.

If BUFFER is given, considers the planner page in BUFFER instead.

Override this if `planner-day-page-template' is a function
instead of a string."
      (with-current-buffer (or buffer (current-buffer))
        (let* ((bufstr (planner-clean-page-sections (buffer-string))))
          (when (and (stringp planner-day-page-template)
                     (not (> (length bufstr)
                             (+
                              (if (member 'planner-diary-insert-all-diaries-maybe planner-goto-hook)
                                  (length planner-diary-appts-string) 0)
                              (length planner-day-page-template)
                              planner-template-fuzz-factor))))
            (let ((body (planner-strip-whitespace
                         (if (member 'planner-diary-insert-all-diaries-maybe planner-goto-hook)
                             (or (string-replace-match (concat planner-diary-appts-string "\n\n\n") bufstr "") bufstr)
                             bufstr))))
              (or (= (length body) 0)
                  (string=
                   body
                   (planner-strip-whitespace
                    planner-day-page-template))))))))

    (defun planner-maybe-remove-file ()
      "Delete the planner file if it does not contain new information."
      (if (planner-page-default-p (current-buffer))
          (let ((filename buffer-file-name))
            (set-buffer-modified-p nil)
            (kill-buffer (current-buffer))
            (when (file-exists-p filename)
              (message "planner-maybe-remove-file: deleting %s" filename)
              (funcall planner-delete-file-function filename)))
          (kill-buffer (current-buffer))))

    (defun planner-project-cleanup-empty-pages ()
      (interactive)
      (dolist (f (directory-files (or (caadr (assoc "WikiPlanner" muse-project-alist))
                                      planner-publishing-directory)
                                  t
                                  (concat planner-date-regexp ".muse$")))
        (when (file-exists-p f)
          (with-current-buffer (find-file-noselect f)
            (unless (planner-maybe-remove-file)
              (message "removed")))))))


  (deh-require-maybe (progn
                       planner-bugz
                       planner-env)))

;; Call M-x plan to start planning!
;; end
;; from http://www.emacswiki.org/emacs/PlannerModeQuickStart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide 'plan-config)
