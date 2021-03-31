;;; config.el --- config                             -*- lexical-binding: t; -*-

;; Copyright (C) 2016  sharad

;; Author: sharad <sh4r4d _at_ _G-mail_>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(when (configuration-layer/package-usedp 'org-clock-in-if-not)
  (defun spacemacs/org-clock-in-if-not-enable ()
    (progn
      (defun call-org-clock-in-if-not-at-time-delay-frame-fn (frame)
        (if (functionp 'org-clock-in-if-not-at-time-delay-fn)
            (org-clock-in-if-not-at-time-delay-fn)
          (warn "function org-clock-in-if-not-at-time-delay-frame-fn not defined.")))))

       ;; "Keybinding: Elscreen"


  (defun spacemacs/org-clock-in-if-not-disable ()
    (progn)) ;; "Keybinding: Elscreen"


  (spacemacs/org-clock-in-if-not-enable))

(defun lotus-orgmode-config/post-init-org ())


(defun lotus-orgclocktask/post-init-org-init ()
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
  (use-package org-notify
    ;; alternate very small https://github.com/syohex/org-notify
    :init
    (org-notify-start)
    :commands (org-notify-start)
    :defer t
    :config
    (progn
      (assert (null org-show-notification-handler))))

  (defun lotus-orgclocktask/post-init-org-config ()
    (progn
      (lotus-orgmode-config/post-init-org)
      (assert (null org-show-notification-handler)))))

(defun lotus-orgclocktask/init-org-doing-init ()
  ())

(defun lotus-orgclocktask/init-org-doing-config ()
  ())

(defun lotus-orgclocktask/init-org-alert-init ()
  (org-alert-enable))

(defun lotus-orgclocktask/init-org-alert-config ()
  ;; https://www.reddit.com/r/emacs/comments/3nx0d0/introducing_orgalert_system_notifications_for/
  ;; https://lists.gnu.org/archive/html/emacs-orgmode/2016-06/msg00122.html
  ;; https://www.reddit.com/r/emacs/comments/8bywj3/orgnotify_to_show_scheduled_items/
  ())

(defun lotus-orgclocktask/init-lotus-utils-init ()
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

(defun lotus-orgclocktask/init-lotus-utils-config ()
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
      (add-hook 'kill-emacs-hook #'kill-emacs-org-clock-out))))

(defun lotus-orgclocktask/init-org-clock-unnamed-task-init ()
  (progn))

(defun lotus-orgclocktask/init-org-clock-unnamed-task-config ()
  (progn
    (setq
     *lotus-org-unnamed-task-file*        (expand-file-name "Unnamed.org" (task-party-base-dir))
     *lotus-org-unnamed-parent-task-name* "Unnamed tasks"
     *lotus-org-unnamed-task-name-fmt*    "Unnamed task %d")))

(defun lotus-orgclocktask/init-org-clock-hooks-init ()
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

(defun lotus-orgclocktask/init-org-clock-hooks-config ()
  (progn
    (lotus-org-clock-in/out-insinuate-hooks)))

(defun lotus-orgclocktask/init-org-clock-check-init ()
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
      :config)))

(defun lotus-orgclocktask/init-org-clock-check-config ()
  (progn))

(defun lotus-orgclocktask/init-org-clock-in-if-not-init ()
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

(defun lotus-orgclocktask/init-org-clock-in-if-not-config ()
  (progn))

(defun lotus-orgclocktask/init-org-clock-utils-lotus-init ()
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
                     (message "Error: Org Clock function org-clock-persistence-insinuate not available.")))))))))))

(defun lotus-orgclocktask/init-org-clock-utils-lotus-config ()
  (progn
    (progn
      (use-package task-manager
        :defer t
        :config
        (progn)))

    (progn)))


(defun lotus-orgclocktask/init-org-clock-wrapper-init ()
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
      (org-clock-wrapper-insinuate))))

(defun lotus-orgclocktask/init-org-clock-daysummary-init ()
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
                       (unless (and (boundp 'org-clock-monitor-files)
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
                   ((error) (message "Error: %s" err))) t)))))))

(defun lotus-orgclocktask/init-org-clock-daysummary-config ()
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
         #'lotus-day-summary-add-occ-files)

        (use-package publishing
          :defer t
          :config
          (progn
            (progn
              (use-package task-manager
                :defer t
                :commands (task-current-party-select-set
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
                               (message "[2]org monitor dir %s not exists." monitor-dir))))))))))))))))

(defun lotus-orgclocktask/init-occ-init ()
  (progn
      (progn
        (defun lotus-load-task-manager-delay (delay)
          (message "lotus-load-task-manager-delay: running lotus-load-task-manager-delay after %d" delay)
          (run-at-time-or-now delay
                              #'(lambda ()
                                  (task-party-base-dir))))

        (defun lotus-load-task-manager-delay-time ()
          (message "lotus-load-task-manager-delay-time: running lotus-load-task-manager-delay after %d" delay)
          (lotus-load-task-manager-delay 100)))

      (progn
        (defun lotus-config-start-occ-mode-after-delay (delay)
          (message "lotus-config-start-occ-mode-after-delay: running occ-mode after %d" delay)
          (run-at-time-or-now delay
                              #'(lambda ()
                                  (message "running occ-mode")
                                  (if (functionp 'occ-mode)
                                      (occ-mode t)
                                    (warn "function occ-mode not available."))
                                  (when nil
                                    (when (fboundp 'occ-config-disable-clock-in)
                                      (occ-config-disable-clock-in))
                                    (when (fboundp 'occ-enable-debug)
                                      (occ-enable-debug))))))

        (defun lotus-config-start-occ-mode-after-delay-time ()
          (lotus-config-start-occ-mode-after-delay 70)))

      (defun lotus-config-start-occ-mode-with-session-unified ()
        (use-package sessions-unified
          :defer t
          :config
          (progn
            (when t
              (progn
                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'lotus-load-task-manager-delay-time)

                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'lotus-config-start-occ-mode-after-delay-time)

                (add-to-enable-desktop-restore-interrupting-feature-hook
                 'spaceline-toggle-org-clock-on))))))

    (use-package startup-hooks
      :defer t
      :config
      (progn
        (progn
          (add-to-enable-login-session-interrupting-feature-hook
           'lotus-config-start-occ-mode-with-session-unified
           nil)

          (add-to-enable-startup-interrupting-feature-hook
           'lotus-config-start-occ-mode-with-session-unified
           nil))))))

(defun lotus-orgclocktask/init-occ-config ()
  (progn
    (progn
      (use-package task-manager
        :defer t
        :config
        (progn
          (progn
            (let* ((party-base-dir (task-party-base-dir))
                   (start-file (expand-file-name "start.org" party-base-dir)))
              (if (and (file-directory-p party-base-dir)
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
                     (if (and (file-directory-p party-base-dir)
                              (file-exists-p start-file))
                         (progn
                           (if (functionp 'occ-set-global-tsk-collection-spec)
                               (occ-set-global-tsk-collection-spec (list :tree start-file))
                             (warn "function occ-setup-task-tree-task-root-org-file not available.")))
                       (message "org party dir %s or file %s not exists."
                                party-base-dir
                                start-file))))))))

        (progn)
          ;; (setq occ-task-tree-task-root-org-file
          ;;       (expand-file-name "start.org" (task-party-base-dir)))


        (progn
          (spaceline-toggle-org-clock-on))))))

(defun lotus-orgclocktask/init-activity-init ()
  (activity-activate-all))

(defun lotus-orgclocktask/init-activity-config ())

(defun lotus-orgclocktask/init-org-clock-resolve-advanced-init ()
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

(defun lotus-orgclocktask/init-org-clock-resolve-advanced-config ())

(defun lotus-orgclocktask/init-timesheet-init ())
(defun lotus-orgclocktask/init-timesheet-config ())

(defun lotus-orgclocktask/init-task-manager-init ())

(defun lotus-orgclocktask/init-task-manager-config ()
  (progn
    ;; BUG: TODO: will load publishing which agian trigger task-manager configs
    (let ((org-task-base-dir (org-publish-get-attribute "tasks"
                                                        "org" :base-directory)))
      ;; (task-current-party "meru")
      (unless org-task-base-dir
        (error "Not able to get org-task-base-dir"))

      (when (and org-task-base-dir
                 (file-directory-p org-task-base-dir))
        (task-party-base-dir     (org-publish-get-attribute "tasks" "org" :base-directory))
        (task-scratch-dir        "~/Scratch/main")
        (task-projbuffs-base-dir (publishing-created-contents-path 'misc "projbuffs"))

        (message "lotus-orgclocktask/init-task-manager: org-task-base-dir = %s" org-task-base-dir)

        (task-add-task-party "personal"
                             "report.org"
                             "Personal work"
                             "https://bugzilla.merunetworks.com")

        (task-add-task-party "meru"
                             "report.org"
                             "Office related work"
                             "https://bugzilla.merunetworks.com")

        (task-current-party "meru")))))

(defun lotus-orgclocktask/init-org-sync-init ())

(defun lotus-orgclocktask/init-org-sync-config ())

(defun lotus-orgclocktask/post-init-startup-hooks-init ())

(defun lotus-orgclocktask/post-init-startup-hooks-config ()
  (progn
    ;code will not get run as when
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
                     t)))
     t)))

(defun lotus-orgclocktask/init-counsel-org-clock-init ())

(defun lotus-orgclocktask/init-counsel-org-clock-config ())

(defun lotus-orgclocktask/init-org-clock-split-init ())
(defun lotus-orgclocktask/init-org-clock-split-config ())



;;; config.el ends here
