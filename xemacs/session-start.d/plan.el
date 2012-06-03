;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; from http://www.emacswiki.org/emacs/PlannerModeQuickStart
;; start

;; Setting up Planner
;; Add the files to your load-path. Change these paths as needed.



(deh-require-maybe 'planner
  (setq planner-sections
        '((tasks . "Tasks")
          (notes . "Notes")
          (diary . "Diary")
          (env . "Environment")))

  ;; Setting up Planner
  ;; Add the files to your load-path. Change these paths as needed.
  (setq planner-directory "~/.Organize/emacs/plan/Plans")
  (setq planner-project "WikiPlanner")
  (add-to-list
   'muse-project-alist
   '("WikiPlanner"
     ("~/.Organize/emacs/plan/Plans"   ;; Or wherever you want your planner files to be
      :default "index"
      :major-mode planner-mode
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

  (deh-require-maybe 'muse-wiki    ;;; Allow wiki-links
      (setq muse-wiki-allow-nonexistent-wikiword t))
  (setq mark-diary-entries-in-calendar t)

  ;;(global-set-key (kbd "<f9> t") 'planner-create-task-from-buffer)

  (deh-require-maybe 'planner-gnus
      (planner-gnus-insinuate))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  (deh-require-maybe 'planner-id
      (setq
       planner-id-add-task-id-flag t
       planner-id-update-automatically t ;; default
       planner-id-tracking-file "~/.Organize/emacs/plan/planner-ids"
       ))
  (deh-require-maybe 'planner-cyclic
      (setq
       planner-cyclic-diary-file "~/.Organize/emacs/plan/planner-cyclic-diary-file"
       planner-cyclic-diary-nag t
       ))
  (xrequire 'planner-deadline)

  (deh-require-maybe 'planner-multi
      (setq
       ;; planner-multi-copy-tasks-to-page "TaskPool"
       planner-multi-copy-tasks-to-page "[[TasksByProject][p]] [[TasksByContext][c]]"
       planner-multi-separator ","))

  (deh-require-maybe 'bbdb-com          ;checking it as it fail emacs to move ahead.
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
   planner-carry-tasks-forward t
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
      "* Cal-Desk\n\n"
      )))

  ;; "* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>\n"


  (deh-require-maybe 'planner-appt
    (planner-appt-use-schedule)
    (planner-appt-insinuate)
    (planner-appt-schedule-cyclic-insinuate)
    (planner-appt-calendar-insinuate)
    (setq
     planner-appt-update-appts-on-save-flag t))

  (when (and (xrequire 'planner-timeclock)
           (xrequire 'planner-timeclock-summary))
      (planner-timeclock-summary-insinuate))

  ;; (deh-require-maybe 'planner-ledger
  ;;   (add-hook 'planner-goto-hook 'planner-ledger-insert-maybe))

  (deh-require-maybe 'planner-accomplishments
    (setq
     planner-day-page-template
     (concat
      planner-day-page-template
      "* Accomplishments\n\n\n"))
    (planner-accomplishments-insinuate))

  (deh-require-maybe 'planner-registry
      (planner-registry-insinuate))

  (xrequire 'planner-zoom)
  (xrequire 'planner-lisp)

  (deh-require-maybe 'planner-report ;; for using      M-x planner-report-generate
    (setq planner-report-unfinished-offset 2))

  (deh-require-maybe 'planner-publish
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
;;;       (deh-require-maybe 'planner-ical
;;;           (planner-ical-export-file
;;;            (planner-today)
;;;            (expand-file-name
;;;             "tasks.ics"
;;;             (muse-style-element :path (car (cddr (muse-project planner-project)))))))

  (xrequire 'planner-experimental)
  (xrequire 'planner-unix-mail)
  (deh-require-maybe 'planner-gnus
      (planner-gnus-insinuate))
  ;; (xrequire 'planner-vm)
  ;; (deh-require-maybe 'planner-wl
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


  (xrequire 'planner-bugz))

;; Call M-x plan to start planning!
;; end
;; from http://www.emacswiki.org/emacs/PlannerModeQuickStart
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(user-provide 'plan)
