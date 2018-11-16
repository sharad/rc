;;; packages.el --- lotus-schedule layer packages file for Spacemacs.
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
;; added to `lotus-schedule-packages'. Then, for each package PACKAGE:
;;
;; - If PACKAGE is not referenced by any other Spacemacs layer, define a
;;   function `lotus-schedule/init-PACKAGE' to load and initialize the package.

;; - Otherwise, PACKAGE is already referenced by another Spacemacs layer, so
;;   define the functions `lotus-schedule/pre-init-PACKAGE' and/or
;;   `lotus-schedule/post-init-PACKAGE' to customize the package as it is loaded.

;;; Code:


;;; Documentation
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/lotus-scheduleS.org
;; https://github.com/syl20bnr/spacemacs/blob/master/doc/DOCUMENTATION.org

(defconst lotus-schedule-packages
  '(
    diary-lib
    (weekly-view :location local)
    (planner-interface :location local)
    midnight
    calfw
    (calfw-howm :location local)
    (calfw-ical :location local)
    calfw-org
    calfw-cal
    )
  "The list of Lisp packages required by the lotus-schedule layer.

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

(defun lotus-schedule/init-diary-lib ()
  (use-package diary-lib
      :defer t
      :config
      (progn
        (progn
          (use-package misc-publishing
              :defer t
              :config
              (progn
                (progn
                  (setq
                   diary-file (touch-file (misc-publishing-created-contents-path "emacs/schedule/diary/diary")))

                  (use-package appt
                      ;; :defer t
                      :defer t
                      :config
                      (progn
                        (progn
                          (if (not running-xemacs)
                              (appt-activate 1) ; use (appt-activate 1) for GNU Emacs
                            (appt-initialize)))))))))
        (progn
          (setq diary-display-function 'diary-fancy-display)
          (add-hook 'diary-list-entries-hook 'diary-include-other-diary-files)
          (add-hook 'diary-mark-entries-hook 'diary-mark-included-diary-files)
          (add-hook 'diary-list-entries-hook 'diary-sort-entries t))))

  (use-package startup-hooks
      :defer t
      :config
      (progn
        (use-package diary-lib
            :defer t
            :config
            (progn
              (progn
                (progn
                  (add-to-enable-startup-interrupting-feature-hook
                   #'(lambda ()
                       (when t ; was nil           ;BUG: may be causing emacs to crash when no frame is open.
                         (add-hook 'after-make-frame-functions
                                   '(lambda (nframe)
                                     (run-at-time-or-now 100
                                      '(lambda ()
                                        (setq
                                         diary-file (misc-publishing-created-contents-path "emacs/schedule/diary/diary"))
                                        ;; https://stackoverflow.com/questions/2592095/how-do-i-create-an-empty-file-in-emacs/2592558#2592558
                                        (touch-file diary-file)
                                        (if (not running-xemacs)
                                            (appt-activate 1) ; use (appt-activate 1) for GNU Emacs
                                          (appt-initialize)))))
                                   t)))
                   t))))))))

(defun lotus-schedule/init-weekly-view ()
  (use-package weekly-view
      :commands (disable-diary-appt-display-for)
      :defer t
      :config
      (progn
        (progn
          (defun fancy-diary-display-week-graph-if-appt ()
            (if (or (not diary-entries-list)
                    (and (not (cdr diary-entries-list))
                         (string-equal (car (cdr (car diary-entries-list))) "")))

                (let* ((holiday-list (if holidays-in-diary-buffer
                                         (check-calendar-holidays original-date)))
                       (msg (format "No diary entries for %s %s"
                                    (concat date-string (if holiday-list ":" ""))
                                    (mapconcat 'identity holiday-list "; "))))
                  (if (<= (length msg) (frame-width))
                      (message msg)
                      (set-buffer (get-buffer-create holiday-buffer))
                      (setq buffer-read-only nil)
                      (calendar-set-mode-line date-string)
                      (erase-buffer)
                      (insert (mapconcat 'identity holiday-list "\n"))
                      (goto-char (point-min))
                      (set-buffer-modified-p nil)
                      (setq buffer-read-only t)
                      (display-buffer holiday-buffer)
                      (message  "No diary entries for %s" date-string) ))
                (fancy-diary-display-week-graph)))

          ;; http://www.emacswiki.org/emacs/CalendarWeeklyView
          ;; (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph))
          (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt)

          (defun toggle-fancy-diary-display-week-graph ()
            (interactive)
            (if (memq 'fancy-diary-display-week-graph-if-appt diary-display-function)
                (remove-hook 'diary-display-hook 'fancy-diary-display-week-graph-if-appt)
                (add-hook 'diary-display-function 'fancy-diary-display-week-graph-if-appt)))

          (defun diary-nonintrusive-display ()
            (if diary-entries-list
                (message "Some appointment exists today run M-x diary-show")))

          (defun diary-get-entries-list ()
            diary-entries-list)

          (defun diary-show ()
            (interactive)
            (let ((diary-display-function
                   (intern
                    (completing-read
                     "what: "
                     (append
                      (mapcar 'symbol-name
                              '(diary-fancy-display
                                fancy-diary-display-week-graph-if-appt
                                diary-get-entries-list
                                diary-nonintrusive-display))) nil t))))
              (if (not running-xemacs)
                  (appt-activate 1) ; use (appt-activate 1) for GNU Emacs
                  (appt-initialize))))

          (defvar diary-display-function-old nil "diary-display-function-old")
          (defvar diary-display-functions-list '(diary-nonintrusive-display))
          (defvar disable-diary-appt-display-timer nil "disable diary appt display timer")

          (defun disable-diary-appt-display-for (howlong fn)
            ;; unfinished
            (interactive
             (list
              (get-time "When ")
              (intern (completing-read "what: " (mapcar 'symbol-name diary-display-functions-list) nil t))))
            (if (null diary-display-function-old)
                (when (and howlong fn)
                  (setq diary-display-function-old diary-display-function
                        diary-display-function fn
                        disable-diary-appt-display-timer
                        (run-at-time howlong nil
                                     '(lambda ()
                                       (if diary-display-function-old
                                           (setq
                                            diary-display-function diary-display-function-old
                                            diary-display-function-old nil))))))
                (message "Diary already disabled, not doing anything.")))))))

(defun lotus-schedule/post-init-planner-interface ()
  (use-package planner-interface
      :defer t
      :config
      (progn
        )))

(defun lotus-schedule/init-midnight ()
  (use-package midnight
    :defer t
    :config
    (progn
      (progn
        ;; (midnight-delay-set 'midnight-delay 16200) ;; (eq (* 4.5 60 60) "4:30am")
        (midnight-delay-set 'midnight-delay "4:30am"))

      (progn
        ;;https://www.emacswiki.org/emacs/CleanBufferList
        (setq
         clean-buffer-list-delay-general 1       ;day
         clean-buffer-list-delay-special (* 3 60 60)) ;hour min sec

        (dolist (el
                 '("*buffer-selection*"
                   "*Finder*"
                   "*Finder Category*"
                   "*Finder-package*"
                   "*RE-Builder*"
                   "*vc-change-log*"))
          (add-to-list 'clean-buffer-list-kill-buffer-names el))

        (dolist (el
                 '("\\`\\*Customize .*\\*\\'"
                   "\\`\\*\\(Wo\\)?Man .*\\*\\'"
                   """\\`*.org\\'"      ;all org files
                   ))
          (add-to-list 'clean-buffer-list-kill-regexps el))

        (dolist (el
                 '("*eshell*"
                   "*ielm*"
                   "*mail*"
                   "*w3m*"
                   "*w3m-cache*"))
          (add-to-list 'clean-buffer-list-kill-never-buffer-names el))

        (when nil
          (dolist (el
                   '("\\`\\*tramp/.*\\*\\`"
                     "\\`\\*ftp .*\\*\\`"))
            (add-to-list 'clean-buffer-list-kill-never-regexps el))))

      (progn
        (use-package "planner"
          :defer t
          :config
          (progn
            (add-hook 'midnight-hook
                      '(lambda ()
                         (with-safe-plan-env ;so it will not call update-ssh-agent in night.
                          (save-excursion
                            (save-window-excursion
                              (message "Midnight: running calendar and planner")
                              (calendar)
                              ;; check planner-carry-tasks-forward
                              (plan 7)
                              (muse-project-publish "WikiPlanner"))))))))

        (when nil
          (run-with-timer
           10
           nil
           '(lambda ()
              (with-safe-plan-env
               (condition-case perr
                   (save-excursion
                     (save-window-excursion
                       (progn
                         (calendar)
                         ;; check planner-carry-tasks-forward
                         (defadvice error (before dumptrace activate)
                           (backtrace-to-buffer "*errbuf*")
                           t)
                         (setq find-file-hook nil)
                         (message "sharad11-Midnight: running calendar and planner planner-use-other-window %s" planner-use-other-window)
                         ;; (find-file "/home/s/hell/.Organize/emacs/plan/Plans/AAA.muse")
                         ;; (muse-project-find-file (planner-link-base (planner-today))
                         ;;                         planner-project
                         ;;                         'find-file)

                         ;; (find-file "~/test123.txt")
                         (plan 7)
                         (ad-disable-advice 'error 'before 'dumptrace)
                         (ad-update 'error)
                         (message "sharad22-Midnight: running calendar and planner"))))
                 (error
                  (progn
                    (message "XXXPl Error: %s" perr)
                    (ad-disable-advice 'error 'before 'dumptrace)
                    (ad-update 'error)))))))


          (run-with-timer
           10
           nil
           '(lambda ()
              (with-safe-plan-env
               (condition-case perr
                   (save-excursion
                     (save-window-excursion
                       (progn
                         (calendar)
                         (message "sharad22-Midnight: running calendar and planner")
                         (plan 7)
                         (message "sharad22-Midnight: running calendar and planner"))))
                 (error
                  (progn
                    (message "XXXPl Error: %s" perr)
                    )))))))))))

(defun lotus-schedule/init-calfw ()
  ;; https://github.com/kiwanami/emacs-calfw
  (use-package calfw
      :defer t
      :config
      (progn
        (progn
          (defun my-open-calendar ()
            (interactive)
            (cfw:open-calendar-buffer
             :contents-sources
             (list
              (cfw:org-create-source "Green")  ; orgmode source
              (cfw:howm-create-source "Blue")  ; howm source
              (cfw:cal-create-source "Orange") ; diary source
              (cfw:ical-create-source "Moon" "~/moon.ics" "Gray")  ; ICS source1
                                        ; google calendar ICS
              (cfw:ical-create-source "gcal" "https://..../basic.ics" "IndianRed"))))

          ;; Holidays

          ;; The calfw collects holidays from the customize variable
          ;; calendar-holidays which belongs to holidays.el in the Emacs. See
          ;; the document and source of holidays.el for details.

          ;; Format of month and week days

          ;; Month
          (setq calendar-month-name-array
                ["January"
                 "February"
                 "March"
                 "April"
                 "May"
                 "June"
                 "July"
                 "August"
                 "September"
                 "October"
                 "November"
                 "December"])

          ;; Week days
          (setq calendar-day-name-array
                ["Sunday"
                 "Monday"
                 "Tuesday"
                 "Wednesday"
                 "Thursday"
                 "Friday"
                 "Saturday"])

          ;; First day of the week
          (setq calendar-week-start-day 0) ; 0:Sunday, 1:Monday

          ;; Faces
          (custom-set-faces
           '(cfw:face-title ((t (:foreground "#f0dfaf" :weight bold :height 2.0 :inherit variable-pitch))))
           '(cfw:face-header ((t (:foreground "#d0bf8f" :weight bold))))
           '(cfw:face-sunday ((t :foreground "#cc9393" :background "grey10" :weight bold)))
           '(cfw:face-saturday ((t :foreground "#8cd0d3" :background "grey10" :weight bold)))
           '(cfw:face-holiday ((t :background "grey10" :foreground "#8c5353" :weight bold)))
           '(cfw:face-grid ((t :foreground "DarkGrey")))
           '(cfw:face-default-content ((t :foreground "#bfebbf")))
           '(cfw:face-periods ((t :foreground "cyan")))
           '(cfw:face-day-title ((t :background "grey10")))
           '(cfw:face-default-day ((t :weight bold :inherit cfw:face-day-title)))
           '(cfw:face-annotation ((t :foreground "RosyBrown" :inherit cfw:face-day-title)))
           '(cfw:face-disable ((t :foreground "DarkGray" :inherit cfw:face-day-title)))
           '(cfw:face-today-title ((t :background "#7f9f7f" :weight bold)))
           '(cfw:face-today ((t :background: "grey10" :weight bold)))
           '(cfw:face-select ((t :background "#2f2f2f")))
           '(cfw:face-toolbar ((t :foreground "Steelblue4" :background "Steelblue4")))
           '(cfw:face-toolbar-button-off ((t :foreground "Gray10" :weight bold)))
           '(cfw:face-toolbar-button-on ((t :foreground "Gray50" :weight bold))))

          ;; Grid setting example:
          ;; Default setting
          (setq cfw:fchar-junction ?+
                cfw:fchar-vertical-line ?|
                cfw:fchar-horizontal-line ?-
                cfw:fchar-left-junction ?+
                cfw:fchar-right-junction ?+
                cfw:fchar-top-junction ?+
                cfw:fchar-top-left-corner ?+
                cfw:fchar-top-right-corner ?+ )

          ;; Unicode characters
          (setq cfw:fchar-junction ?╋
                cfw:fchar-vertical-line ?┃
                cfw:fchar-horizontal-line ?━
                cfw:fchar-left-junction ?┣
                cfw:fchar-right-junction ?┫
                cfw:fchar-top-junction ?┯
                cfw:fchar-top-left-corner ?┏
                cfw:fchar-top-right-corner ?┓)

          ;; Another unicode chars
          (setq cfw:fchar-junction ?╬
                cfw:fchar-vertical-line ?║
                cfw:fchar-horizontal-line ?═
                cfw:fchar-left-junction ?╠
                cfw:fchar-right-junction ?╣
                cfw:fchar-top-junction ?╦
                cfw:fchar-top-left-corner ?╔
                cfw:fchar-top-right-corner ?╗)


          ;;  Line breaking

          ;; If a content string is longer than the cell width, the calfw breaks
          ;; into the multiple lines. In the current implementation, the Calfw
          ;; has 3 strategies: none, simple and wordwrap. The variable
          ;; cfw:render-line-breaker selects the strategy to break lines.

          ;;     cfw:render-line-breaker-none
          ;;         Never breaks lines. Longer contents are truncated.
          ;;     cfw:render-line-breaker-simple (default)
          ;;         This strategy breaks lines with rigid width. This may be not so beautiful, but In the most cases it looks good.
          ;;     cfw:render-line-breaker-wordwrap

          ;;         This strategy breaks lines with the emacs function
          ;;         fill-region. Although, the line breaking algorithm of the
          ;;         Emacs is not so smart as more complicated ones, such as
          ;;         Knuth/Plass algorithm, this strategy is better than the
          ;;         simple one.


          (add-to-enable-startup-interrupting-feature-hook 'cfw:open-calendar-buffer t)))))

(defun lotus-schedule/init-calfw-howm ()
  (use-package calfw-howm
      :defer t
      :config
      (progn
        (progn ;; calfw-howm
          (with-eval-after-load "howm-menu"
            '(progn
              (require 'calfw-howm)
              (cfw:install-howm-schedules)
              ))))))

(defun lotus-schedule/init-calfw-ical ()
  (use-package calfw-ical
      :defer t
      :config
      (progn
        ;; (cfw:open-ical-calendar "http://www.google.com/calendar/ical/.../basic.ics")
        )))

(defun lotus-schedule/init-calfw-org ()
  (use-package calfw-org
      :defer t
      :config
      (progn
        (progn
          ))))

(defun lotus-schedule/init-calfw-cal ()
  (use-package calfw-cal
      :defer t
      :config
      (progn
        (progn
          ))))

;;; packages.el ends here
