;;; packages.el --- lotus-orgclocktask layer packages file for Spacemacs.
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
;; added to `lotus-orgclocktask-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-orgclocktask/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-orgclocktask/pre-init-PACKAGE' and/or
;;   `lotus-orgclocktask/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/LAYERS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-orgclocktask-packages
  '(
    org
    ;; org-notify
    org-doing
    org-alert
    ;; org-wild-notifier
    lotus-utils
    ;; org-misc-utils-lotus
    org-clock-unnamed-task
    org-clock-hooks
    org-clock-check
    org-clock-in-if-not
    ;; org-clock-utils-lotus
    org-clock-wrapper
    org-clock-daysummary
    org-clock-table-misc-lotus
    occ
    activity
    org-clock-resolve-advanced
    timesheet
    ;; wakatime-mode
    task-manager
    startup-hooks
    counsel-org-clock
    org-clock-split)


  "The list of Lisp packages required by the lotus-orgclocktask layer.

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

;; (spacemacs|use-package-add-hook org
;;   :pre-init
;;   (package-initialize))

(defun lotus-orgclocktask/post-init-org ()
  (use-package org
    :defer t
    :config
    (progn
      (lotus-orgmode-config/post-init-org)
      (assert (null org-show-notification-handler))))
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
  (use-package org-notify
    ;; alternate very small https://github.com/syohex/org-notify
    :init
    (org-notify-start)
    :commands (org-notify-start)
    :defer t
    :config
    (progn
      (assert (null org-show-notification-handler)))))

(defun lotus-orgclocktask/init-org-doing ()
  (use-package org-doing
    :defer t
    :config
    (progn)))

(defun lotus-orgclocktask/init-org-alert ()
  ;; https://www.reddit.com/r/emacs/comments/3nx0d0/introducing_orgalert_system_notifications_for/
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
  ;; https://www.reddit.com/r/emacs/comments/8bywj3/orgnotify_to_show_scheduled_items/
  (use-package org-alert
    :init
    (org-alert-enable)
    :commands (org-alert-enable)
    :defer t
    :config
    (progn)))

;; not working
;; (defun lotus-orgclocktask/init-org-wild-notifier ()
;;   ;; see also https://github.com/akhramov/org-wild-notifier.el
;;   (use-package org-wild-notifier
;;     :init
;;     (org-wild-notifier-mode)
;;     :commands (org-wild-notifier-mode)
;;     :defer t
;;     :config
;;     (progn)))

(defun lotus-orgclocktask/init-lotus-utils ()
  (use-package org-misc-utils-lotus
    :defer t
    :config
    (progn

      (progn                          ;settings
        (setq
         ;; https://stackoverflow.com/questions/8281604/remove-done-tasks-from-agenda-view
         org-agenda-skip-scheduled-if-done t))

      (progn
        (setq org-refile-targets
              '((nil :maxlevel . 3)           ; only the current file
                (org-agenda-files :maxlevel . 3) ; all agenda files, 1st/2nd level
                (org-files-list :maxlevel . 4)   ; all agenda and all open files
                (lotus-org-files-list :maxlevel . 4))))
      (progn
        (add-hook
         'kill-emacs-hook
         #'(lambda ()
             (if (and
                  (org-clock-is-active)
                  ;; (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil)
                  )
                 (org-with-clock-writeable
                   (let (org-log-note-clock-out)
                     (if (org-clock-is-active)
                         (org-clock-out))))))))))



  (progn
    (progn
      (use-package org-clock-utils-lotus
        :defer t
        :config
        (progn
          (progn
            (use-package task-manager
              :defer t
              :config
              (progn)))

          (progn))))

    (progn
      (progn
        (use-package org
          :defer t
          :config
          (progn
            (use-package sessions-unified
              :defer t
              :config
              (when t
                (add-to-enable-desktop-restore-interrupting-feature-hook
                 #'(lambda ()
                     (if (fboundp 'org-clock-persistence-insinuate)
                         (org-clock-persistence-insinuate)
                       (message "Error: Org Clock function org-clock-persistence-insinuate not available."))))))))))))


(defun lotus-orgclocktask/init-org-clock-unnamed-task ()
  (use-package org-clock-unnamed-task
      :defer t
      :config
      (progn
        (progn
          )
        (progn
          )
        (progn
          (setq
           *lotus-org-unnamed-task-file*        (expand-file-name "Unnamed.org" (task-party-base-dir))
           *lotus-org-unnamed-parent-task-name* "Unnamed tasks"
           *lotus-org-unnamed-task-name-fmt*    "Unnamed task %d")))))

(defun lotus-orgclocktask/init-org-clock-hooks ()
  (use-package org-clock-hooks
    :defer t
    :config
    (progn
      (progn
        )
      (progn
        )
      (progn
        (lotus-org-clock-in/out-insinuate-hooks))))

  (progn
    (use-package sessions-unified
      :defer t
      :config
      (progn
        (when t
          (add-to-enable-desktop-restore-interrupting-feature-hook
           #'(lambda ()
               (lotus-org-clock-in/out-insinuate-hooks)))))))

  (progn
    (use-package startup-hooks
      :defer t
      :config
      (progn
        (progn
          (add-to-enable-startup-interrupting-feature-hook
           #'(lambda ()
               (lotus-org-clock-in/out-insinuate-hooks)) t))))))

(defun lotus-orgclocktask/init-org-clock-check ()
  (use-package org-clock-check
    :defer t
    :config
    (progn
      (progn
        )
      (progn
        )))

  (progn
    (use-package sessions-unified
      :defer t
      :config
      (progn
        (when t
          (add-to-enable-desktop-restore-interrupting-feature-hook
           #'(lambda ()
               (when (fboundp 'org-clock-start-check-timer-insiuate)
                 (org-clock-start-check-timer-insiuate))
               (when (fboundp 'org-clock-lotus-log-note-on-change-insinuate)
                 (org-clock-lotus-log-note-on-change-insinuate))))))))
  (progn
    (use-package startup-hooks
      :defer t
      :config
      )))

(defun lotus-orgclocktask/init-org-clock-in-if-not ()
  (use-package org-clock-in-if-not
    :defer t
    :config
    (progn
      (progn
        )
      (progn
        )))
  (progn
    (use-package startup-hooks
      :defer t
      :config
      (progn

        (progn ;code will not get to run as when
          ;;`enable-startup-interrupting-feature-hook' run at start,
          ;;that time package `org-misc-utils-lotus' did not get
          ;;loaded.
          ;; BUG: not getting included

          (defun call-org-clock-in-if-not-at-time-delay-frame-fn (frame)
            (if (functionp 'org-clock-in-if-not-at-time-delay-fn)
                (org-clock-in-if-not-at-time-delay-fn)
              (warn "function org-clock-in-if-not-at-time-delay-frame-fn not defined.")))

          (add-to-enable-startup-interrupting-feature-hook
           #'(lambda ()
               (add-hook
                'delete-frame-functions
                #'(lambda (nframe)
                    (when (fboundp 'org-clock-out-if-active)
                      (org-clock-out-if-active))))) t)

          (add-to-enable-login-session-interrupting-feature-hook
           #'(lambda ()
               (when t ; was nil           ;BUG: may be causing emacs to crash when no frame is open.
                 (add-hook 'after-make-frame-functions
                           #'call-org-clock-in-if-not-at-time-delay-frame-fn
                           t))
               (add-hook
                'delete-frame-functions
                #'(lambda (nframe)
                    (when (fboundp 'org-clock-out-if-active)
                      (org-clock-out-if-active))))) t)

          (add-to-disable-login-session-interrupting-feature-hook
           #'(lambda ()
               (when t ; was nil           ;BUG: may be causing emacs to crash when no frame is open.
                 (remove-hook 'after-make-frame-functions
                              #'call-org-clock-in-if-not-at-time-delay-frame-fn))) t))))))

(defun lotus-orgclocktask/init-org-clock-utils-lotus ()
  (progn
    (progn
      (use-package org-clock-utils-lotus
        :defer t
        :config
        (progn
          (progn
            (use-package task-manager
              :defer t
              :config
              (progn
                )))
          (progn
            ))))

    (progn
      (progn
        (use-package org
          :defer t
          :config
          (progn
            (use-package sessions-unified
              :defer t
              :config
              (when t
                (add-to-enable-desktop-restore-interrupting-feature-hook
                 #'(lambda ()
                     (if (fboundp 'org-clock-persistence-insinuate)
                         (org-clock-persistence-insinuate)
                       (message "Error: Org Clock function org-clock-persistence-insinuate not available."))))))))))))

(defun lotus-orgclocktask/init-org-clock-wrapper ()
  (progn
    (progn
      (use-package org-clock-wrapper
          :defer t
          :config
          (progn
            (progn))))


    (progn ;; Need it.
      (progn
        (use-package startup-hooks
          :defer t
          :config
          (progn
            (add-to-enable-startup-interrupting-feature-hook #'org-clock-wrapper-insinuate))))
      (progn
        ;; (warn  "running org-clock-wrapper-insinuate")
        ;; (add-hook 'after-init-hook #'org-clock-wrapper-insinuate)
        (org-clock-wrapper-insinuate)))))


(defun lotus-orgclocktask/init-org-clock-daysummary ()
  (progn
    (use-package org-clock-daysummary
      ;; :defer t
      :defer t
      :commands (org-clock-work-day-mode-line-add)
      :config
      (progn
        (use-package occ
          :defer t
          :config
          (progn
            (defun lotus-day-summary-add-occ-files ()
              (dolist (f (occ-files))
                (org-clock-monitor-files-add-files f))
              (org-clock-work-day-mode-line-add t))

            (lotus-day-summary-add-occ-files)

            (occ-run-with-global-tsk-collection
             #'lotus-day-summary-add-occ-files)))

        (use-package publishing
          :defer t
          :config
          (progn
            (progn
              (use-package task-manager
                :defer t
                :commands (office-mode
                           task-current-party-select-set
                           task-current-party task-party-dir
                           task-select-party-dir
                           find-task-dir)
                :config
                (progn
                  (progn
                    ;; (message "test1")
                    (unless (task-current-party)
                      (task-current-party "meru"))
                    (when (task-current-party)
                      (let* ((monitor-dir (task-party-dir))
                             (unnamed-file (expand-file-name "../Unnamed.org" monitor-dir)))
                        (if (file-directory-p monitor-dir)
                            (progn
                              ;; (org-clock-monitor-files-set-from-dir monitor-dir)
                              (when (file-exists-p unnamed-file)
                                (org-clock-monitor-files-add-files unnamed-file))
                              (org-clock-work-day-mode-line-add t))
                          (message "[1]org monitor dir %s not exists." monitor-dir)))))

                  (progn
                    (add-to-task-current-party-change-hook
                     #'(lambda ()
                         ;; (unless (task-current-party)
                         ;;   (task-current-party "meru"))
                         ;; (message "test2")
                         (when (task-current-party)
                           (let* ((monitor-dir (task-party-dir))
                                  (unnamed-file (expand-file-name "../Unnamed.org" monitor-dir)))
                             (if (file-directory-p monitor-dir)
                                 (progn
                                   ;; (org-clock-monitor-files-set-from-dir monitor-dir)
                                   (when (file-exists-p unnamed-file)
                                     (org-clock-monitor-files-add-files unnamed-file))
                                   (org-clock-work-day-mode-line-add t))
                               (message "[2]org monitor dir %s not exists." monitor-dir))))))))))))))

    (use-package startup-hooks
      :defer t
      :config
      (progn
        (progn
          (add-to-enable-login-session-interrupting-feature-hook
           #'(lambda ()
               ;; (message "test3")
               (org-clock-work-day-mode-line-add t)) t)

          (when t
            (add-to-enable-startup-interrupting-feature-hook
             #'(lambda ()
                 ;; (message "test4")
                 (condition-case err
                     (when (fboundp 'task-current-party)
                       (unless (task-current-party)
                         (task-current-party "meru"))
                       (unless (and
                                (boundp 'org-clock-monitor-files)
                                org-clock-monitor-files)
                         (when (task-current-party)
                           (when (task-current-party)
                             (let* ((monitor-dir (task-party-dir))
                                    (unnamed-file (expand-file-name "../Unnamed.org" monitor-dir)))
                               (if (file-directory-p monitor-dir)
                                   (progn
                                     ;; (org-clock-monitor-files-set-from-dir monitor-dir)
                                     (when (file-exists-p unnamed-file)
                                       (org-clock-monitor-files-add-files unnamed-file))
                                     (org-clock-work-day-mode-line-add t))
                                 (message "[4]org monitor dir %s not exists." monitor-dir))))))
                       (org-clock-work-day-mode-line-add t))
                   ((error) (message "Error: %s" err))) t))))))))

(defun lotus-orgclocktask/init-org-clock-table-misc-lotus ()
  (use-package org-clock-table-misc-lotus
      :defer t
      :config
      (progn
        ))
  (use-package org-nagora-report
      :defer t
      :config
      (progn
        ))
  (use-package org-timesheet
      :defer t
      :config
      (progn
        )))

(defun lotus-orgclocktask/init-occ ()
  (progn
    (use-package occ
      ;; :commands (occ-insinuate occ-uninsinuate)
      :defer t
      :config
      (progn
        (progn
          (use-package task-manager
            :defer t
            :config
            (progn
              (progn
                (let* ((party-base-dir (task-party-base-dir))
                       (start-file (expand-file-name "start.org" party-base-dir)))
                  (if (and
                       (file-directory-p party-base-dir)
                       (file-exists-p start-file))
                      (progn
                        (if (functionp 'occ-set-global-tsk-collection-spec)
                            (occ-set-global-tsk-collection-spec (list :tree start-file))
                          (warn "function occ-setup-task-tree-task-root-org-file not available.")))
                    (message "org party dir %s or file %s not exists."
                             party-base-dir
                             start-file))))

              (progn
                (add-to-task-current-party-change-hook
                 #'(lambda ()
                     (unless task-current-party
                       (task-current-party "meru"))
                     (when (task-current-party)
                       (let* ((party-base-dir (task-party-base-dir))
                              (start-file (expand-file-name "start.org" party-base-dir)))
                         (if (and
                              (file-directory-p party-base-dir)
                              (file-exists-p start-file))
                             (progn
                               (if (functionp 'occ-set-global-tsk-collection-spec)
                                   (occ-set-global-tsk-collection-spec (list :tree start-file))
                                 (warn "function occ-setup-task-tree-task-root-org-file not available.")))
                           (message "org party dir %s or file %s not exists."
                                    party-base-dir
                                    start-file))))))))))

        (progn
          ;; (setq occ-task-tree-task-root-org-file
          ;;       (expand-file-name "start.org" (task-party-base-dir)))
          )

        (progn
          (spaceline-toggle-org-clock-on))))

    (progn
      (progn
        (defun lotus-load-task-manager-delay (delay)
          (run-at-time-or-now delay
                              #'(lambda ()
                                  (task-party-base-dir))))

        (defun lotus-load-task-manager-delay-time ()
          (lotus-load-task-manager-delay 100)))

      (progn
        (defun lotus-config-start-occ-insinuate-after-delay (delay)
          (run-at-time-or-now delay
                              #'(lambda ()
                                  (if (functionp 'occ-insinuate)
                                      (occ-insinuate)
                                    (warn "function occ-insinuate not available."))
                                  (when (fboundp 'occ-config-disable-clock-in)
                                    (occ-config-disable-clock-in))
                                  (when (fboundp 'occ-enable-debug)
                                    (occ-enable-debug)))))

        (defun lotus-config-start-occ-insinuate-after-delay-time ()
          (lotus-config-start-occ-insinuate-after-delay 70)))

      (defun lotus-config-start-occ-insinuate-with-session-unified ()
        (use-package sessions-unified
          :defer t
          :config
          (progn
            (when t
              (progn
                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'lotus-load-task-manager-delay-time)

                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'lotus-config-start-occ-insinuate-after-delay-time)

                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'spaceline-toggle-org-clock-on)))))))

    (use-package startup-hooks
      :defer t
      :config
      (progn
        (progn
          (add-to-enable-login-session-interrupting-feature-hook
           'lotus-config-start-occ-insinuate-with-session-unified
           nil)

          (add-to-enable-startup-interrupting-feature-hook
           'lotus-config-start-occ-insinuate-with-session-unified
           nil))))))

(defun lotus-orgclocktask/init-activity ()
  (use-package activity
    :defer t
    :config
    (progn))


  (activity-activate-all))

(defun lotus-orgclocktask/init-org-clock-resolve-advanced ()
  (use-package org-clock-resolve-advanced
    :defer t
    :config
    (progn
      (progn
        )
      (progn
        )))

  (use-package startup-hooks
    :defer t
    :config
    (progn
      (progn
        (add-to-enable-startup-interrupting-feature-hook
         #'(lambda ()
             (if (functionp 'org-clock-resolve-advanced-insinuate)
                 (org-clock-resolve-advanced-insinuate)
               (warn "function org-clock-resolve-advanced-insinuate not available.")))
         t)))))

(defun lotus-orgclocktask/init-timesheet ()
  ;; https://github.com/tmarble/timesheet.el
  (use-package timesheet
    :defer t
    :config
    (progn
      )))

;; (defun lotus-orgclocktask/post-init-wakatime-mode ()
;;   ;; https://github.com/tmarble/timesheet.el
;;   (use-package wakatime-mode
;;       :defer t
;;       :config
;;       (progn)))                            ;do not need it now. will see later.
;;         ;; (global-wakatime-mode)


(defun lotus-orgclocktask/init-task-manager ()
  (use-package task-manager
    :defer t
    :commands (office-mode
               task-current-party-select-set
               task-current-party task-party-dir
               task-select-party-dir
               find-task-dir)
    :config
    (progn
      (progn
        ;; BUG: TODO: will load publishing which agian trigger task-manager configs
        (let ((org-task-base-dir
               (org-publish-get-attribute "tasks" "org" :base-directory)))

          ;; (task-current-party "meru")
          (unless org-task-base-dir
            (error "Not able to get org-task-base-dir"))

          (when (and
                 org-task-base-dir
                 (file-directory-p org-task-base-dir))

            (task-party-base-dir     (org-publish-get-attribute "tasks" "org" :base-directory))
            (task-scratch-dir        "~/Scratches/main")
            (task-projbuffs-base-dir (publishing-created-contents-path 'misc "projbuffs"))

            (message "lotus-orgclocktask/init-task-manager: org-task-base-dir = %s" org-task-base-dir)

            (task-add-task-party
             "personal"
             "report.org"
             "Personal work"
             "https://bugzilla.merunetworks.com")

            (task-add-task-party
             "meru"
             "report.org"
             "Office related work"
             "https://bugzilla.merunetworks.com")

            (task-current-party "meru"))))

      (progn
        (autoload 'magit-git-lines "magit")
        (autoload 'magit-process-file "magit")

        (defvar office-git-remote-regex "")

        (setq office-git-remote-regex "fortinet")
        (defun office-file-p (file)
          (let ((remote-repo
                 (car
                  (remove-if-not
                   #'(lambda (s) (if s (string-match-p "^origin" s)))
                   (magit-git-lines "remote" "-v")))))
            (if (and
                 (functionp 'magit-git-lines)
                 remote-repo)
                (string-match-p
                 office-git-remote-regex
                 remote-repo))))

        (defun office-activate ()
          (interactive)
          (let ((file (buffer-file-name)))
            (when (and file (office-file-p file))
              ;; if file is handled by perforce than assume it is
              ;; related to office perforce repository.
              (office-mode 1))))

        (add-hook 'prog-mode-hook 'office-activate)
        (add-hook 'nxml-mode-hook 'office-activate)))))


(defun lotus-orgclocktask/post-init-startup-hooks () ;getting run when run-each-hooks called at last
  (use-package startup-hooks
    :defer t
    :config
    (progn
      (progn ;code will not get run as when
        ;`enable-startup-interrupting-feature-hook' run at early start,
        ;that time package `org-misc-utils-lotus' did not get loaded.
        ;; BUG: not getting included
        (add-to-enable-startup-interrupting-feature-hook
         #'(lambda ()
             (when t ; was nil           ;BUG: may be causing emacs to crash when no frame is open.
               (add-hook 'after-make-frame-functions
                         '(lambda (nframe)
                            (run-at-time-or-now 100
                                                '(lambda ()
                                                   (if (any-frame-opened-p)
                                                       (org-clock-in-if-not)))))
                         t))
             ;; (add-hook
             ;;  'delete-frame-functions
             ;;  #'(lambda (nframe)
             ;;      (if (and
             ;;           (org-clock-is-active)
             ;;           (y-or-n-p-with-timeout (format "Do you want to clock out current task %s: " org-clock-heading) 7 nil))
             ;;          (org-with-clock-writeable
             ;;           (let (org-log-note-clock-out)
             ;;             (if (org-clock-is-active)
             ;;                 (org-clock-out)))))))
             )
         t))

      ;; (progn
      ;;   (add-to-enable-desktop-restore-interrupting-feature-hook
      ;;    #'(lambda ()
      ;;       (if (fboundp 'org-clock-persistence-insinuate)
      ;;           (org-clock-persistence-insinuate)
      ;;           (message "Error: Org Clock function org-clock-persistence-insinuate not available."))
      ;;       (if (fboundp 'org-clock-start-check-timer-insiuate)
      ;;           (org-clock-start-check-timer-insiuate)))
      ;;     t))
      )))

(defun lotus-orgclocktask/init-counsel-org-clock ()
  (use-package counsel-org-clock
    :defer t
    :config
    (progn
      )))

(defun lotus-orgclocktask/init-org-clock-split ()
  (use-package org-clock-split
    :defer t
    :config
    (progn)))

;;; packages.el ends here
