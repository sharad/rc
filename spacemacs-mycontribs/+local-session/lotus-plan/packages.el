;;; packages.el --- lotus-plan layer packages file for Spacemacs.
;;
;; Copyright (c) 2012-2016 Sylvain Benner & Contributors
;;
;; Author: sharad <s@think530-spratap>
;; URL: https://github.com/syl20bnr/spacemacs
;;
;; This file is not part of GNU Emacs.
;;
;;; License: GPLv3

;;; Commentary:

;; See the Spacemacs documentation and FAQs for instructions on how to implement
;; a new layer:
;;
;;   SPC h SPC layers RET
;;
;;
;; Briefly, each package to be installed or configured by this layer should be
;; added to `lotus-plan-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-plan/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-plan/pre-init-PACKAGE' and/or
;;   `lotus-plan/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-plan-packages
  '(
    ;; (PACKAGE :location local)
    publishing
    ;; (muse-publishing :location local)
    ;; (org-publishing :location local)
    (planner :location local)
    (muse-wiki :location local)
    muse
    (planner-gnus :location local)
    (planner-id :location local)
    (planner-cyclic :location local)
    (planner-multi :location local)
    (bbdb-com :location local :location local)
    (planner-appt :location local)
    timeclock
    ;; planner-timeclock
    ;; planner-timeclock-summary
    (planner-ledger :location local)
    (planner-accomplishments :location local)
    (planner-registry :location local)
    (planner-report :location local) ;; for using      M-x planner-report-generate
    (planner-publish :location local)
    (planner-calendar :location local)
    (planner-ical :location local)
    (planner-authz :location local)
    (planner-experimental :location local)
    (planner-unix-mail :location local)
    (planner-gnus :location local)
    (planner-rmail :location local)
    (planner-bugz :location local)
    (planner-env :location local))
  "The list of Lisp packages required by the lotus-plan layer.

Each entry is either:

1. A symbol, which is interpreted as a package to be installed, or

2. A list of the form (PACKAGE KEYS...), where PACKAGE is the
    name of the package to be installed or loaded, and KEYS are
    any number of keyword-value-pairs.

    The following keys are accepted:

    - :excluded (t or nil): Prevent the package from being loaded
      if value is non-nil

    - :location: Specify a custom installation location.
      The following values are legal:

      - The symbol `elpa' (default) means PACKAGE will be
        installed using the Emacs package manager.

      - The symbol `local' directs Spacemacs to load the file at
        `./local/PACKAGE/PACKAGE.el'

      - A list beginning with the symbol `recipe' is a melpa
        recipe.  See: https://github.com/milkypostman/melpa#recipe-format")

(defun lotus-plan/post-init-publishing ()
  (use-package publishing
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/post-init-muse-publishing ()
  (use-package muse-publishing
      :defer t
      :config
      (progn
        )))

;; (defun lotus-plan/post-init-org-publishing ()
;;   (use-package org-publishing
;;       :defer t
;;       :config
;;       (progn
;;         )))

(defun lotus-plan/init-planner ()
  (use-package planner
    :defer t

      :commands (plan)
      :config
      (progn
        (use-package muse-publishing
            :defer t
            :config
            (progn

              ;; (add-muse-project
              ;;  '("WikiPlanner"
              ;;    ("~/.Organize/emacs/plan/Plans"   ;; Or wherever you want your planner files to be
              ;;     :default "index"
              ;;     ;; :major-mode #'planner-mode
              ;;     :major-mode planner-mode
              ;;     ;; :final muse-project-publish-file
              ;;     :visit-link planner-visit-link)
              ;;    ;; This next part is for specifying where Planner pages
              ;;    ;; should be published and what Muse publishing style to
              ;;    ;; use. In this example, we will use the XHTML publishing
              ;;    ;; style.
              ;;    (:base "planner-xhtml"
              ;;           ;; where files are published to
              ;;           ;; (the value of 'planner-publishing-directory', if
              ;;           ;; if you have configuration for an older version
              ;;           ;; of Planner)
              ;;           :path "~/public_html/Plans")))


              (let* ((local-planner-relative-path "web/site/wiki/Organize/plan/Plans")
                     (local-planner-directory
                      ;; (expand-file-name local-planner-relative-path *muse-top-dir*)
                       (content-muse-dir local-planner-relative-path))
                     (local-planner-output-directory
                      (expand-file-name "planner-xhtml" (expand-file-name local-planner-relative-path *muse-generated-top-dir*))
                       (content-misc-publishing-dir)))
                (if (file-directory-p local-planner-directory)
                    (progn
                      (setq
                       ;; Setting up Planner
                       ;; Add the files to your load-path. Change these paths as needed.
                       planner-directory (expand-file-name local-planner-relative-path *muse-top-dir*)
                       planner-project "WikiPlanner")

                      (add-muse-project
                       `("WikiPlanner"
                         (,local-planner-directory  ;; Or wherever you want your planner files to be
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
                                :path ,local-planner-output-directory))))
                    (error "planner directory `%s' do not exists" local-planner-directory)))))

        ;; (require 'muse-publishing)

        ;; (debug)

        ;; (debug)

        (progn
          (setq
           planner-sections
           '((tasks . "Tasks")
             (notes . "Notes")
             (diary . "Diary")
             (env   . "Environment")))

          (setq
           ;; faced muse-get-keyword void-variable planner-mode
           planner-mode #'planner-mode
           ;;(setq planner-renumber-tasks-automatically t)
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
           ;;      possible vaules...
           ;;      `planner-sort-tasks-default-key', `planner-sort-tasks-basic',
           ;;      `planner-sort-tasks-by-date', and `planner-sort-tasks-by-link'.
           ;;      `planner-sort-tasks-by-rank', `planner-sort-tasks-by-importance', and
           ;;      `planner-sort-tasks-by-urgency'.
           ;;       planner-add-task-at-end-flag nil
           planner-day-page-template
           (concat
            "* Tasks\n\n\n"
            "* Schedule\n\n\n"
            "* Notes\n\n\n"
            )
           ))

        (progn

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

          (define-key planner-mode-map (kbd "C-c n") 'planner-create-note-this-page))

        (progn ;; "Day Pages Cleanup"

          ;; (require 'string) ;; from elib package

          (defun planner-clean-page-section (section empty-string page-string)
            (or
             (string-replace-match (concat "* " section "\n\n" empty-string) page-string (concat "* " section "\n") t)
             ;; alternate
             ;; (replace-regexp-in-string (concat "* " section "\n\n" empty-string) (concat "* " section "\n") page-string nil t)
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
            (dolist (f (directory-files (or (caadr (assoc planner-project muse-project-alist))
                                            planner-publishing-directory)
                                        t
                                        (concat planner-date-regexp ".muse$")))
              (when (file-exists-p f)
                (with-current-buffer (find-file-noselect f)
                  (unless (planner-maybe-remove-file)
                    (message "removed")))))))

        ;; Call M-x plan to start planning!
        ;; end
        ;; from http://www.emacswiki.org/emacs/PlannerModeQuickStart
        ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
        (progn
          (use-package calendar
              :defer t
              :config
              (progn
                (setq
                 calendar-mark-diary-entries-flag t
                 ;;   mark-diary-entries-in-calendar t
                 diary-file (auto-config-file "diary/diary"))
                (require 'file-utils)
                (unless (file-exists-p diary-file)
                  (auto-config-dir (dirname-of-file diary-file) t)
                  (with-temp-buffer
                    (insert "")
                    (write-file diary-file)))))

          (use-package muse-wiki ;; Allow wiki-links
              :defer t
              :config
              (progn
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

                (setq muse-wiki-allow-nonexistent-wikiword t)))

          (use-package planner-zoom :defer t)
          (use-package planner-lisp :defer t)
          (use-package planner-erc :defer t)
          (use-package planner-w3m :defer t)
          (use-package planner-bibtex :defer t)
          (use-package planner-id :defer t)
          (use-package planner-gnus :defer t)
          (use-package planner-rank :defer t)
          (use-package planner-trunk :defer t)
          (use-package xtla
              :defer t
              :config
              (use-package planner-xtla :defer t))
          ;; (use-package planner-accomplishments :defer t) ; M-x planner-accomplishments-show after M-x plan
          (use-package planner-tasks-overview :defer t) ; M-x planner-tasks-overview
          )


        (progn
          (when t
            (require 'planner-registry)
            (planner-registry-insinuate)))

        (progn
          (when t
            (use-package startup-hooks
                :defer t
                :config
                (progn
                  (progn
                    (add-to-enable-startup-interrupting-feature-hook ;; '*lotus-after-init-hook*
                              '(lambda ()
                                (with-eval-after-load "planner-registry"
                                  (progn
                                    (setq
                                     planner-registry-file (auto-config-file "planner/planner-registry.el"))
                                    (unless (file-exists-p planner-registry-file)
                                      (auto-config-dir (dirname-of-file planner-registry-file) t))
                                    (save-excursion
                                      (save-window-excursion
                                        (plan 2)
                                        (planner-registry-insinuate)))))))))))))))

(defun lotus-plan/init-muse-wiki ()
  (use-package muse-wiki
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/post-init-muse ()
  (use-package muse
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-gnus ()
  (use-package planner-gnus
      :defer t
      :config
      (progn
        (planner-gnus-insinuate)
        )))

(defun lotus-plan/init-planner-id ()
  (use-package planner-id
      :defer t
      :config
      (progn
        (setq
         planner-id-add-task-id-flag t
         planner-id-update-automatically t ;; default
         planner-id-tracking-file (auto-config-file "plan/planner-ids")))))

(defun lotus-plan/init-planner-cyclic ()
  (use-package planner-cyclic
      :defer t
      :config
      (progn
        (setq
         planner-cyclic-diary-file (auto-config-file "plan/planner-cyclic-diary-file")
         planner-cyclic-diary-nag t))))

(defun lotus-plan/init-planner-deadline ()
  (use-package planner-deadline
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-multi ()
  (use-package planner-multi
      :defer t
      :config
      (progn
        (use-package planner-modified
            :defer t
            )
        (use-package planner-multi-modified
            :defer t
            )

        (progn
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

          ;; TODO Fix it
          (when nil
            (assert (eq (symbol-function 'planner-edit-task-description)
                        'planner-multi-edit-task-description)))

          (assert (not
                   (eq (symbol-function 'planner-edit-task-description)
                       'planner-edit-task-description-basic)))

          (assert (eq (symbol-function 'planner-task-date)
                      'planner-multi-task-date))

          ;; (assert (eq (symbol-function 'planner-link-as-list)
          ;;             'planner-multi-link-as-list))

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
          (defalias 'planner-multi-delete-task-this-page 'planner-multi-task-delete-this-page)))))

(defun lotus-plan/init-bbdb-com ()
  (use-package bbdb-com          ;checking it as it fail emacs to move ahead.
      :defer t
      :config
      (progn
        (use-package planner-bbdb
            :defer t
            ))))

(defun lotus-plan/init-planner-diary ()
  ;; "* Diary\n\n<lisp>(planner-diary-entries-here)</lisp>\n"
  (use-package planner-diary
      :defer t
      :config
      (progn
        (progn

          (unless (file-readable-p diary-file)
            (message "(file-readable-p diary-file)"))

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

        (progn
          (use-package diary-lib :defer t)
          (use-package diary :defer t)
          (use-package holidays :defer t)
          (use-package cal-menu :defer t))
        )))

(defun lotus-plan/init-planner-appt ()
  (use-package planner-appt
      :defer t
      :config
      (progn
        (planner-appt-use-schedule)
        (planner-appt-insinuate)
        (planner-appt-schedule-cyclic-insinuate)
        (planner-appt-calendar-insinuate)
        (setq
         planner-appt-update-appts-on-save-flag t))))

(defun lotus-plan/post-init-timeclock ()
  (use-package timeclock
      :defer t
      :config
      (progn
        (use-package planner-timeclock
            :defer t
            :config
            (progn
              (setq
               timeclock-file (convert-standard-filename (auto-config-file "timeclock/timelog")))))

        (use-package planner-timeclock-summary
            :defer t
            :config
            (progn
              (planner-timeclock-summary-insinuate))))))

(defun lotus-plan/init-planner-ledger ()
  (use-package planner-ledger
      :defer t
      :config
      (add-hook 'planner-goto-hook 'planner-ledger-insert-maybe)))

(defun lotus-plan/init-planner-accomplishments ()
  (use-package planner-accomplishments
      :defer t
      :config
      (progn
        (setq
         planner-day-page-template
         (concat
          planner-day-page-template
          "* Accomplishments\n\n\n"))
        (planner-accomplishments-insinuate))))

(defun lotus-plan/init-planner-registry ()
  (use-package planner-registry
      :defer t
      :commands (planner-registry-insinuate)
      :config
      (progn
        (progn
          ;;{{
          ;; (deh-require-maybe planner-registry
          ;;     (planner-registry-insinuate))
          ;; planner-registry-insinuate some time create problem in daemon startup.
          ;; as it may start to ask input.

          (setq planner-registry-file (auto-config-file "planner/planner-registry.el")))

        (when nil
         (use-package startup-hooks
            :defer t
            :config
            (progn
             (add-to-enable-startup-interrupting-feature-hook ;; '*lotus-after-init-hook*
                      '(lambda ()
                         (with-eval-after-load "planner-registry"
                           (progn
                             (setq planner-registry-file "~/.emacs.d/autoconfig/planner/planner-registry.el")
                             (save-excursion
                               (save-window-excursion
                                 (plan 2)
                                 (planner-registry-insinuate)))))))))))))

(defun lotus-plan/init-planner-report ()
  ;; for using      M-x planner-report-generate
  (use-package planner-report
      :defer t
      :config
      (progn
        (setq
         planner-report-unfinished-offset 2))))

(defun lotus-plan/init-planner-publish ()
  (use-package planner-publish
      :defer t
      :config
      (progn
        ;; To automatically publish files when you save them,
        ;; add the following code to your `~/.emacs' (or `_emacs'):

        ;; (eval-after-load "muse-mode"
        ;;  (add-hook 'after-save-hook
        ;;            #'(lambda ()
        ;;                (when (planner-derived-mode-p 'muse-mode)
        ;;                  (muse-project-publish nil)))
        ;;            nil t))
        )))

(defun lotus-plan/init-planner-calendar ()
  (use-package planner-calendar
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-ical ()
  (use-package planner-ical
      :defer t
      :config
      (progn
        ;;           (planner-ical-export-file
        ;;            (planner-today)
        ;;            (expand-file-name
        ;;             "tasks.ics"
        ;;             (muse-style-element :path (car (cddr (muse-project planner-project))))))
        )))

(defun lotus-plan/init-planner-authz ()
  (use-package planner-authz
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-experimental ()
  (use-package planner-experimental
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-unix-mail ()
  (use-package planner-unix-mail
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-gnus ()
  ;; (xrequire 'planner-vm)
  ;; (deh-require-maybe planner-wl
  ;;    (planner-wl-insinuate))
  ;; (xrequire 'planner-mhe)
  (use-package planner-gnus
      :defer t
      :config
      (progn
        (planner-gnus-insinuate))))

(defun lotus-plan/init-planner-rmail ()
  (use-package planner-rmail
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/init-planner-bugz ()
  (use-package planner-bugz
      :defer t
      :config
      (progn
        )))

(defun lotus-plan/post-init-planner-env ()
  (use-package planner-env
      :defer t
      :config
      (progn
        )))

;;; packages.el ends here
